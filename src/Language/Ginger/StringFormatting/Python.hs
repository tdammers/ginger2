{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ginger.StringFormatting.Python
where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Data.Void (Void)
import Data.Char (isDigit, GeneralCategory (..), generalCategory)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Float (floatToDigits)

data FormatArgVT m a =
  FormatArgVT
    { lookupAttrib :: Text -> a -> m (Maybe a)
    , lookupItem :: Text -> a -> m (Maybe a)
    , lookupIndex :: Integer -> a -> m (Maybe a)
    , argAsString :: a -> m Text
    , argAsRepr :: a -> m Text
    , argAsASCII :: a -> m Text
    , argAsInt :: a -> m (Maybe Integer)
    , argAsFloat :: a -> m (Maybe Double)
    , formattingGroup :: a -> m FormattingGroup
    }

data FormattingGroup
  = FormatAsInt
  | FormatAsFloat
  | FormatAsString
  deriving (Show, Eq, Ord, Enum, Bounded)

formatList :: (Monad m)
           => FormatArgVT m a
           -> Text
           -> [(Maybe Text, a)]
           -> m (Either String Text)
formatList vt fmt allArgs = runExceptT $ do
  items <- either (throwError . P.errorBundlePretty) pure $
            P.parse (P.many pFormatItem <* P.eof) "format string" fmt
  go 0 items
  where
    args = Vector.fromList (map snd allArgs)
    kwargs = Map.fromList [ (k, v) | (Just k, v) <- allArgs ]

    go _ [] = pure ""
    go n (item:items) = case item of
      PlainFormatItem {} ->
        (<>) <$> renderFormatItem vt n args kwargs item <*> go n items
      _ ->
        (<>) <$> renderFormatItem vt n args kwargs item <*> go (succ n) items

padL :: Char -> Int -> Text -> Text
padL p w t =
  let pw = max 0 $ Text.length t - w
  in Text.replicate pw (Text.singleton p) <> t

renderFormatItem :: forall m a.
                    ( Monad m
                    )
                 => FormatArgVT m a
                 -> Integer
                 -> Vector a
                 -> Map Text a
                 -> FormatItem
                 -> ExceptT String m Text
renderFormatItem _ _ _ _ (PlainFormatItem txt) = pure txt
renderFormatItem vt defPosition args kwargs (FieldFormatItem field) = do
  val <- lookupFormatItemArg vt args kwargs (fromDefault (FieldNameNumber defPosition) $ formatFieldName field)
  fgroup <- lift $ formattingGroup vt val
  let spec = formatFieldSpec field

  let formatAsString =
        lift $ argAsString vt val

      formatAsInt = do
        i <- maybe (throwError "Invalid format argument, expected int") pure =<<
              lift (argAsInt vt val)
        pure . applyGrouping $ Text.show i

      formatAsFloat = do
        f <- maybe (throwError "Invalid format argument, expected float") pure =<<
              lift (argAsFloat vt val)
        pure $ Text.show f

      formatAsFixed :: ExceptT String m Text
      formatAsFixed = do
        f <- maybe (throwError "Invalid format argument, expected float") pure =<<
              lift (argAsFloat vt val)
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (intpart, fracpart) = properFraction $ abs f
            intpartStr = applyGrouping $ Text.show (intpart :: Integer)
            fracpartMul :: Integer = round $ fracpart * 10 ^ precision
            fracpartStr = padL '0' precision $ Text.show fracpartMul
        let sign = getFloatSign f
        pure $ sign <> Text.show intpartStr <> "." <> fracpartStr

      formatAsPercentage :: ExceptT String m Text
      formatAsPercentage = do
        f <- maybe (throwError "Invalid format argument, expected float") (pure . (* 100)) =<<
              lift (argAsFloat vt val)
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (intpart, fracpart) = properFraction $ abs f
            intpartStr = Text.show (intpart :: Integer)
            fracpartMul :: Integer = round $ fracpart * 10 ^ precision
            fracpartStr = padL '0' precision $ Text.show fracpartMul
        let sign = getFloatSign f
        pure $ sign <> Text.show intpartStr <> "." <> fracpartStr <> "%"

      formatAsScientific :: ExceptT String m Text
      formatAsScientific = do
        f <- maybe (throwError "Invalid format argument, expected float") pure =<<
              lift (argAsFloat vt val)
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (mantissaDigits, exponentInt) = floatToDigits 10 (abs f)
            sign = getFloatSign f
            mantissaStr = applyGrouping . mconcat . map Text.show . take precision $ mantissaDigits
            expStr = Text.show exponentInt
        pure $ sign <> "0." <> mantissaStr <> "e" <> expStr

      insertThousandsSep :: Text -> Text -> Text
      insertThousandsSep sep src =
        let chunks = reverse . map Text.reverse . Text.chunksOf 3 . Text.reverse $ src
        in Text.intercalate sep chunks

      applyGrouping :: Text -> Text
      applyGrouping src =
        case fieldSpecGrouping spec of
          NoGrouping -> src
          GroupComma -> insertThousandsSep "," src
          GroupUnderscore -> insertThousandsSep "_" src

      getFloatSign :: Double -> Text
      getFloatSign f =
        case (compare (signum f) 0, fieldSpecSign spec) of
          (LT, _) -> "-"
          (GT, SignNegative) -> "+"
          (EQ, SignNegative) -> ""
          (_, SignAlways) -> "+"
          (_, SignSpacePadded) -> " "

  strVal <- case fieldSpecType spec of
    FieldTypeGeneral -> case fgroup of
      FormatAsString -> formatAsString
      FormatAsInt -> formatAsInt
      FormatAsFloat -> formatAsFloat

    FieldTypeDecimalInt -> formatAsInt

    FieldTypeFixedPoint -> formatAsFixed
    FieldTypeFixedPointUpper -> Text.toUpper <$> formatAsFixed
    FieldTypeScientific -> formatAsScientific
    FieldTypeScientificUpper -> Text.toUpper <$> formatAsScientific

    FieldTypePercentage -> formatAsPercentage

    FieldTypeString -> formatAsString
    t -> throwError $ "Field type " ++ show t ++ " not implemented"

  let defAlignment = case fgroup of
        FormatAsString -> AlignLeft
        _ -> AlignRight

  pure $ align
          (fromDefault defAlignment $ fieldSpecAlign spec)
          (fieldSpecWidth spec)
          (fromDefault ' ' $ fieldSpecFill spec)
          strVal

align :: FieldAlign -> OrDefault Int -> Char -> Text -> Text
align _ Default _ str = str
align AlignLeft (Specific w) fill str =
  let extra = max 0 (w - Text.length str)
  in str <> Text.replicate extra (Text.singleton fill)
align AlignRight (Specific w) fill str =
  let extra = max 0 (w - Text.length str)
  in Text.replicate extra (Text.singleton fill) <> str
align AlignCenter (Specific w) fill str =
  let extra = max 0 (w - Text.length str)
      extraL = extra `div` 2
      extraR = extra - extraL
  in Text.replicate extraL (Text.singleton fill)
      <> str
      <> Text.replicate extraR (Text.singleton fill)
align AlignZeroPad (Specific w) fill str =
  if Text.take 1 str `elem` ["-", "+", " "] then
    Text.take 1 str <> align AlignRight (Specific $ w - 1) fill (Text.drop 1 str)
  else
    align AlignRight (Specific w) fill str

lookupFormatItemArg :: Monad m
                    => FormatArgVT m a
                    -> Vector a
                    -> Map Text a
                    -> FieldName
                    -> ExceptT String m a
lookupFormatItemArg _vt _args kwargs (FieldNameIdentifier n) =
  maybe (throwError $ "Field not found: " ++ show n) pure $
    Map.lookup n kwargs
lookupFormatItemArg _vt args _kwargs (FieldNameNumber i) =
  maybe (throwError $ "Field not found: " ++ show i) pure $
    args Vector.!? fromInteger i
lookupFormatItemArg vt args kwargs (FieldNameAttrib a b) =
  lookupFormatItemArg vt args kwargs b >>=
  lift . lookupAttrib vt a >>=
  maybe (throwError $ "Not found: attribute " ++ show a) pure
lookupFormatItemArg vt args kwargs (FieldNameKeyIndex a b) =
  lookupFormatItemArg vt args kwargs b >>=
  lift . lookupItem vt a >>=
  maybe (throwError $ "Not found: item " ++ show a) pure
lookupFormatItemArg vt args kwargs (FieldNameNumIndex a b) =
  lookupFormatItemArg vt args kwargs b >>=
  lift . lookupIndex vt a >>=
  maybe (throwError $ "Not found: item " ++ show a) pure

class FormatArg a where
  formatArg :: FormatField -> a -> Text

type P a = P.Parsec Void Text a

data FormatItem
  = PlainFormatItem !Text
  | FieldFormatItem !FormatField
  deriving (Show, Eq)

data FormatField =
  FormatField
    { formatFieldName :: !(OrDefault FieldName)
    , formatFieldConversion :: !FieldConversion
    , formatFieldSpec :: !FieldSpec
    }
    deriving (Show, Eq)

data FieldName
  = FieldNameIdentifier !Text
  | FieldNameNumber !Integer
  | FieldNameAttrib !Text !FieldName
  | FieldNameKeyIndex !Text !FieldName
  | FieldNameNumIndex !Integer !FieldName
  deriving (Show, Eq)

data FieldConversion
  = FieldConvNone
  | FieldConvRepr
  | FieldConvString
  | FieldConvASCII
  deriving (Show, Eq)

data FieldZeroCoercion
  = AllowNegativeZero
  | ForcePositiveZero
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldAlternateForm
  = NormalForm
  | AlternateForm
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldZeroPadding
  = NoZeroPadding
  | ZeroPadding
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldGrouping
  = NoGrouping
  | GroupComma
  | GroupUnderscore
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldSign
  = SignNegative -- ^ Default: show sign only when negative
  | SignSpacePadded -- ^ Show sign if negative, add padding space if positive
  | SignAlways -- ^ Always show sign
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldType
  = FieldTypeString
  | FieldTypeBinary
  | FieldTypeCharacter
  | FieldTypeDecimalInt
  | FieldTypeOctal
  | FieldTypeHex
  | FieldTypeHexUpper
  | FieldTypeNumber
  | FieldTypeScientific
  | FieldTypeScientificUpper
  | FieldTypeFixedPoint
  | FieldTypeFixedPointUpper
  | FieldTypeGeneral
  | FieldTypeGeneralUpper
  | FieldTypePercentage
  deriving (Show, Eq, Ord, Enum, Bounded)

data FieldAlign
  = AlignLeft
  | AlignRight
  | AlignCenter
  | AlignZeroPad
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrDefault a
  = Default
  | Specific a
  deriving (Show, Eq, Ord, Functor)

fromDefault :: a -> OrDefault a -> a
fromDefault d Default = d
fromDefault _ (Specific x) = x

data FieldSpec =
  FieldSpec
    { fieldSpecAlign :: !(OrDefault FieldAlign)
    , fieldSpecFill :: !(OrDefault Char)
    , fieldSpecSign :: !FieldSign
    , fieldSpecZeroCoercion :: !FieldZeroCoercion
    , fieldSpecAlternateForm :: !FieldAlternateForm
    , fieldSpecZeroPadding :: !FieldZeroPadding
    , fieldSpecWidth :: !(OrDefault Int)
    , fieldSpecGrouping :: !FieldGrouping
    , fieldSpecPrecision :: !(OrDefault Int)
    , fieldSpecType :: !FieldType
    }
    deriving (Show, Eq)

defFieldSpec :: FieldSpec
defFieldSpec =
  FieldSpec
    { fieldSpecAlign = Default
    , fieldSpecFill = Default
    , fieldSpecSign = SignNegative
    , fieldSpecZeroCoercion = AllowNegativeZero
    , fieldSpecAlternateForm = NormalForm
    , fieldSpecZeroPadding = NoZeroPadding
    , fieldSpecWidth = Default
    , fieldSpecGrouping = NoGrouping
    , fieldSpecPrecision = Default
    , fieldSpecType = FieldTypeGeneral
    }

pFormatItem :: P FormatItem
pFormatItem = P.choice
  [ PlainFormatItem <$> P.chunk "{{"
  , PlainFormatItem <$> P.chunk "}}"
  , FieldFormatItem <$> (P.char '{' *> pFormatField <* P.char '}')
  , PlainFormatItem <$> P.takeWhile1P Nothing (/= '{')
  ]

pFormatField :: P FormatField
pFormatField =
  FormatField
    <$> P.option Default (Specific <$> pFieldName)
    <*> P.option FieldConvNone pFieldConv
    <*> P.option defFieldSpec pFieldSpec

pFieldName :: P FieldName
pFieldName = do
  base <- P.choice
    [ FieldNameIdentifier <$> pIdentifier
    , FieldNameNumber <$> pInteger
    ]
  pTail base
  where
    pTail base = P.choice
      [ pDotTail base >>= pTail
      , pIndexTail base >>= pTail
      , pure base
      ]

    pDotTail base = do
      void $ P.char '.'
      FieldNameAttrib <$> pIdentifier <*> pure base

    pIndexTail base =
      P.char '[' *>
      P.choice
        [ FieldNameKeyIndex <$> pIdentifier <*> pure base
        , FieldNameNumIndex <$> pInteger <*> pure base
        ]
      <* P.char ']'

pInteger :: Num a => P a
pInteger = do
  digits <- P.takeWhile1P (Just "digit") isDigit
  pure . fromInteger . read . Text.unpack $ digits

pIdentifier :: P Text
pIdentifier = do
  t0 <- P.satisfy isIdentStartChar
  ts <- P.takeWhileP (Just "identifier char") isIdentChar
  pure $ Text.cons t0 ts

-- See https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-python-grammar-identifier

identStartCategories :: Set GeneralCategory
identStartCategories =
  Set.fromList
    [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    , ModifierLetter
    , OtherLetter
    , LetterNumber
    ]

identCategories :: Set GeneralCategory
identCategories =
  identStartCategories <>
  Set.fromList
    [ NonSpacingMark
    , SpacingCombiningMark
    , DecimalNumber
    , ConnectorPunctuation
    ]

-- See https://www.unicode.org/Public/15.1.0/ucd/PropList.txt

identStartCharacters :: Set Char
identStartCharacters =
  Set.fromList
    [ '_'
    , '\x1885'
    , '\x1886'
    , '\x2118'
    , '\x212E'
    , '\x309B'
    , '\x309C'
    ]

identCharacters :: Set Char
identCharacters =
  identStartCharacters <>
  Set.fromList
    [ '\x00B7'
    , '\x0387'
    , '\x1369'
    , '\x1370'
    , '\x1371'
    , '\x19DA'
    , '\x200C'
    , '\x200D'
    , '\xFF65'
    ]

isIdentStartChar :: Char -> Bool
isIdentStartChar c = 
  c `Set.member` identStartCharacters ||
  generalCategory c `Set.member` identStartCategories

isIdentChar :: Char -> Bool
isIdentChar c = 
  c `Set.member` identCharacters ||
  generalCategory c `Set.member` identCategories


pFieldConv :: P FieldConversion
pFieldConv =
  P.choice
    [ FieldConvRepr <$ P.char 'r'
    , FieldConvString <$ P.char 's'
    , FieldConvASCII <$ P.char 'a'
    ]

pFieldSpec :: P FieldSpec
pFieldSpec = do
  void $ P.char ':'
  (fill, alignment) <- pFillAlign
  FieldSpec
    <$> pure alignment
    <*> pure fill
    <*> P.option (fieldSpecSign defFieldSpec) pFieldSign
    <*> P.option (fieldSpecZeroCoercion defFieldSpec) pFieldZeroCoercion
    <*> P.option (fieldSpecAlternateForm defFieldSpec) pFieldAlternateForm
    <*> P.option (fieldSpecZeroPadding defFieldSpec) pFieldZeroPadding
    <*> P.option Default (Specific <$> pInteger)
    <*> P.option (fieldSpecGrouping defFieldSpec) pFieldGrouping
    <*> P.option Default (Specific <$> pInteger)
    <*> P.option (fieldSpecType defFieldSpec) pFieldType

pFillAlign :: P (OrDefault Char, OrDefault FieldAlign)
pFillAlign =
  P.choice
    [ P.try $
        (,) <$> (Specific <$> P.anySingle) <*> (Specific <$> pFieldAlign)
    , (Default,) . Specific <$> pFieldAlign
    , pure (Default, Default)
    ]

pFieldAlign :: P FieldAlign
pFieldAlign =
  P.choice
    [ AlignLeft <$ P.char '<'
    , AlignRight <$ P.char '>'
    , AlignCenter <$ P.char '^'
    , AlignZeroPad <$ P.char '='
    ]

pFieldZeroCoercion :: P FieldZeroCoercion
pFieldZeroCoercion = ForcePositiveZero <$ P.char 'z'

pFieldSign :: P FieldSign
pFieldSign =
  P.choice
    [ SignAlways <$ P.char '+'
    , SignSpacePadded <$ P.char ' '
    , SignNegative <$ P.char '-'
    ]

pFieldAlternateForm :: P FieldAlternateForm
pFieldAlternateForm = AlternateForm <$ P.char '#'

pFieldZeroPadding :: P FieldZeroPadding
pFieldZeroPadding = ZeroPadding <$ P.char '0'

pFieldGrouping :: P FieldGrouping
pFieldGrouping =
  P.choice
    [ GroupComma <$ P.char ','
    , GroupUnderscore <$ P.char '_'
    ]

pFieldType :: P FieldType
pFieldType =
  P.choice
    [ FieldTypeString <$ P.char 's'
    , FieldTypeBinary <$ P.char 'b'
    , FieldTypeCharacter <$ P.char 'c'
    , FieldTypeDecimalInt <$ P.char 'd'
    , FieldTypeOctal <$ P.char 'o'
    , FieldTypeHex <$ P.char 'x'
    , FieldTypeHexUpper <$ P.char 'X'
    , FieldTypeNumber <$ P.char 'n'
    , FieldTypeScientific <$ P.char 'e'
    , FieldTypeScientificUpper <$ P.char 'E'
    , FieldTypeFixedPoint <$ P.char 'f'
    , FieldTypeFixedPointUpper <$ P.char 'F'
    , FieldTypeGeneral <$ P.char 'g'
    , FieldTypeGeneralUpper <$ P.char 'G'
    , FieldTypePercentage <$ P.char '%'
    ]
