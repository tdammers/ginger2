{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Python-style string formatting.
-- See https://docs.python.org/3/library/string.html#formatstrings for spec.
module Language.Ginger.StringFormatting.Python
( -- * Running
  formatList
, FormattingGroup (..)
, FormatArg (..)
  -- * Parsing
, parseFormat
  -- * Rendering
, renderFormat
, renderFormatItem
  -- * AST
, FormatItem (..)
, FormatField (..)
, FieldName (..)
, FieldConversion (..)
, FieldZeroCoercion (..)
, FieldAlternateForm (..)
, FieldZeroPadding (..)
, FieldGrouping (..)
, FieldSign (..)
, FieldType (..)
, FieldAlign (..)
, OrDefault (..)
, fromDefault
, FieldSpec (..)
, defFieldSpec
)
where

import Control.Monad (void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Data.Void (Void)
import Data.Char (isDigit, GeneralCategory (..), generalCategory, isAscii)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Float (floatToDigits)
import Text.Read (readMaybe)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Formatting / Running
--------------------------------------------------------------------------------

-- | Formattable argument. We reduce potential inputs to a limited set of
-- representations; it is the responsibility of the caller to convert whatever
-- values they want to use as formatting args to this type.
data FormatArg
  = IntArg !Integer
    -- ^ Integer arguments
  | FloatArg !Double
    -- ^ Floating-point arguments
  | StringArg !Text
    -- ^ Any scalar argument that is best represented as a string
  | ListArg !(Vector FormatArg)
    -- ^ List argument; cannot be formatted directly, but can be accessed
    -- using index syntax (@{0[0]}@)
  | DictArg !(Map Text FormatArg)
    -- ^ Dictionary argument; cannot be formatted directly, but can be accessed
    -- using index syntax (@{0[key]}@) or attribute syntax (@0.key@).
  | -- | Polymorphic argument; may offer any of the available representations,
    -- allowing the formatter to pick the most appropriate one.
    PolyArg
      (Maybe Text) -- ^ string representation
      (Maybe Integer) -- ^ integer representation
      (Maybe Double) -- ^ float representation
      (Maybe (Vector FormatArg))
        -- ^ list representation (equivalent to 'ListArg')
      (Maybe (Map Text FormatArg))
        -- ^ dictionary representation (equivalent to 'DictArg')
  deriving (Show, Eq, Ord)

-- | Convert to string, imitating Python's @str()@
argAsString :: FormatArg -> Either String Text
argAsString (IntArg i) = pure $ Text.show i
argAsString (FloatArg f) = pure $ Text.show f
argAsString (StringArg s) = pure s
argAsString (PolyArg (Just s) _ _ _ _) = pure s
argAsString (PolyArg _ (Just i) _ _ _) = pure $ Text.show i
argAsString (PolyArg _ _ (Just f) _ _) = pure $ Text.show f
argAsString _ = Left "Cannot convert argument to string"

-- | Convert to string, imitating Python's @repr()@
argAsRepr :: FormatArg -> Either String Text
argAsRepr (IntArg i) = pure $ Text.show i
argAsRepr (FloatArg f) = pure $ Text.show f
argAsRepr (StringArg s) = pure $ Text.show s
argAsRepr (ListArg xs) = do
  inner <- mapM argAsRepr (Vector.toList xs)
  pure $ "[" <> Text.intercalate ", " inner <> "]"
argAsRepr (DictArg xs) = do
  inner <- mapM (\(k, v) -> (k <>) . (": " <>) <$> argAsRepr v) (Map.toList xs)
  pure $ "{" <> Text.intercalate ", " inner <> "}"
argAsRepr (PolyArg (Just s) _ _ _ _) = pure $ Text.show s
argAsRepr (PolyArg _ (Just i) _ _ _) = pure $ Text.show i
argAsRepr (PolyArg _ _ (Just f) _ _) = pure $ Text.show f
argAsRepr (PolyArg _ _ _ (Just xs) _) = argAsRepr (ListArg xs)
argAsRepr (PolyArg _ _ _ _ (Just xs)) = argAsRepr (DictArg xs)
argAsRepr _ = Left "Cannot convert argument to string"

-- | Coerce to @StringArg@, imitating Python's @repr()@.
reprArg :: FormatArg -> Either String FormatArg
reprArg a = StringArg <$> argAsRepr a

-- | Coerce to @StringArg@, imitating Python's @str()@.
stringArg :: FormatArg -> Either String FormatArg
stringArg a = StringArg <$> argAsString a

-- | Coerce to @StringArg@, imitating Python's @ascii()@.
asciiArg :: FormatArg -> Either String FormatArg
asciiArg a = StringArg . Text.filter isAscii <$> argAsString a

-- | Interpret formatting argument as integer, using rounding and string
-- parsing if necessary.
argAsInt :: FormatArg -> Either String Integer
argAsInt (IntArg i) = pure i
argAsInt (FloatArg f) = pure $ round f
argAsInt (StringArg s) =
  maybe (Left "Non-numeric string used as integer") pure .
  readMaybe .
  Text.unpack $
  s
argAsInt (PolyArg _ (Just i) _ _ _) = pure i
argAsInt (PolyArg _ _ (Just f) _ _) = pure $ round f
argAsInt (PolyArg (Just s) _ _ _ _) = argAsInt (StringArg s)
argAsInt _ = Left "Cannot convert non-scalar to integer"

-- | Interpret formatting argument as float, using casting and string
-- parsing if necessary.
argAsFloat :: FormatArg -> Either String Double
argAsFloat (IntArg i) = pure $ fromInteger i
argAsFloat (FloatArg f) = pure f
argAsFloat (StringArg s) =
  maybe (Left "Non-numeric string used as float") pure .
  readMaybe .
  Text.unpack $
  s
argAsFloat (PolyArg _ _ (Just f) _ _) = pure f
argAsFloat (PolyArg _ (Just i) _ _ _) = pure $ fromInteger i
argAsFloat (PolyArg (Just s) _ _ _ _) = argAsFloat (StringArg s)
argAsFloat _ = Left "Cannot convert non-scalar to float"

-- | Look up an attribute by name.
lookupAttrib :: Text -> FormatArg -> Either String FormatArg
lookupAttrib name (DictArg items) =
  maybe (Left $ "Attribute " ++ show name ++ " not found")  pure .
  Map.lookup name $
  items
lookupAttrib name (ListArg items) = do
  maybe (Left $ "Attribute " ++ show name ++ " not found")  pure $ do
    index <- readMaybe . Text.unpack $ name
    items Vector.!? index
lookupAttrib name (PolyArg _ _ _ _ (Just xs)) = lookupAttrib name (DictArg xs)
lookupAttrib name (PolyArg _ _ _ (Just xs) _) = lookupAttrib name (ListArg xs)
lookupAttrib name _ = Left $ "Cannot get attribute " ++ show name ++ " from scalar"

-- | Look up an item by index.
lookupIndex :: Integer -> FormatArg -> Either String FormatArg
lookupIndex index (DictArg items) =
  maybe (Left $ "Item " ++ show index ++ " not found") pure .
  Map.lookup (Text.show index) $
  items
lookupIndex index (ListArg items) = do
  maybe (Left $ "Item " ++ show index ++ " not found") pure $
    items Vector.!? (fromInteger index)
lookupIndex index (PolyArg _ _ _ (Just xs) _) = lookupIndex index (ListArg xs)
lookupIndex index (PolyArg _ _ _ _ (Just xs)) = lookupIndex index (DictArg xs)
lookupIndex index _ = Left $ "Cannot get item " ++ show index ++ " from scalar"

-- | Formatting group; this determines the interpretation of formatting types
-- such as @g@, which will behave differently depending on the type of
-- argument.
data FormattingGroup
  = FormatAsInt
  | FormatAsFloat
  | FormatAsString
  | FormatInvalid
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Determine the formatting group of a 'FormatArg'. For 'PolyArg', the order
-- of preference is float, int, string.
formattingGroup :: FormatArg -> FormattingGroup
formattingGroup StringArg {} = FormatAsString
formattingGroup IntArg {} = FormatAsInt
formattingGroup FloatArg {} = FormatAsFloat
formattingGroup (PolyArg _ _ (Just _) _ _) = FormatAsFloat
formattingGroup (PolyArg _ (Just _) _ _ _) = FormatAsInt
formattingGroup (PolyArg (Just _) _ _ _ _) = FormatAsString
formattingGroup _ = FormatInvalid

-- | Apply format string, passing arguments as a list of optional key / value
-- pairs. All arguments can be addressed positionally; those that have a key
-- that is a valid identifier can also be addressed by that.
formatList :: Text
           -> [(Maybe Text, FormatArg)]
           -> Either String Text
formatList fmt allArgs = do
  f <- parseFormat fmt
  renderFormat f allArgs

-- | Render a format string against the given arguments.
renderFormat :: [FormatItem] -> [(Maybe Text, FormatArg)] -> Either String Text
renderFormat xs allArgs =
  go 0 xs
  where
    args = Vector.fromList (map snd allArgs)
    kwargs = Map.fromList [ (k, v) | (Just k, v) <- allArgs ]

    go _ [] = pure ""
    go n (item:items) = case item of
      PlainFormatItem {} ->
        (<>) <$> renderFormatItem n args kwargs item <*> go n items
      _ ->
        (<>) <$> renderFormatItem n args kwargs item <*> go (succ n) items

-- | Parse a format string.
parseFormat :: Text -> Either String [FormatItem]
parseFormat fmt =
  either (Left . P.errorBundlePretty) pure $
    P.parse pFormat "format string" fmt


padL :: Char -> Int -> Text -> Text
padL p w t =
  let pw = max 0 $ Text.length t - w
  in Text.replicate pw (Text.singleton p) <> t

-- | Render a single formatting item using the provided arguments.
renderFormatItem :: Integer
                 -> Vector FormatArg
                 -> Map Text FormatArg
                 -> FormatItem
                 -> Either String Text
renderFormatItem _ _ _ (PlainFormatItem txt) = pure txt
renderFormatItem defPosition args kwargs (FieldFormatItem field) = do
  val' <- lookupFormatItemArg args kwargs (fromDefault (FieldNameNumber defPosition) $ formatFieldName field)
  val <- case formatFieldConversion field of
            FieldConvNone -> pure val'
            FieldConvRepr -> reprArg val'
            FieldConvString -> stringArg val'
            FieldConvASCII -> asciiArg val'
            
  let fgroup = formattingGroup val
      spec = formatFieldSpec field

  let formatAsString :: Either String Text
      formatAsString = argAsString val

      formatAsInt :: Either String Text
      formatAsInt = do
        i <- argAsInt val
        pure . applyGrouping $ Text.show i

      formatAsFloat :: Either String Text
      formatAsFloat = do
        f <- argAsFloat val
        pure $ Text.show f

      formatAsHex :: Either String Text
      formatAsHex = do
        i <- argAsInt val
        let hex = applyHexGrouping . Text.pack $ printf "%x" i
        case fieldSpecAlternateForm spec of
          AlternateForm -> pure $ "0x" <> hex
          NormalForm -> pure hex

      formatAsOctal :: Either String Text
      formatAsOctal = do
        i <- argAsInt val
        let octal = applyHexGrouping . Text.pack $ printf "%o" i
        case fieldSpecAlternateForm spec of
          AlternateForm -> pure $ "0o" <> octal
          NormalForm -> pure octal

      formatAsBinary :: Either String Text
      formatAsBinary = do
        i <- argAsInt val
        let binary = applyHexGrouping . Text.pack $ printf "%b" i
        case fieldSpecAlternateForm spec of
          AlternateForm -> pure $ "0b" <> binary
          NormalForm -> pure binary

      formatAsFixed :: Either String Text
      formatAsFixed = do
        f <- argAsFloat val
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (intpart, fracpart) = properFraction $ abs f
            intpartStr = applyGrouping $ Text.show (intpart :: Integer)
            fracpartMul :: Integer = round $ fracpart * 10 ^ precision
            fracpartStr = padL '0' precision $ Text.show fracpartMul
        let sign = getFloatSign f
        pure $ sign <> Text.show intpartStr <> "." <> fracpartStr

      formatAsPercentage :: Either String Text
      formatAsPercentage = do
        f <- argAsFloat val
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (intpart, fracpart) = properFraction $ abs f
            intpartStr = Text.show (intpart :: Integer)
            fracpartMul :: Integer = round $ fracpart * 10 ^ precision
            fracpartStr = padL '0' precision $ Text.show fracpartMul
        let sign = getFloatSign f
        pure $ sign <> Text.show intpartStr <> "." <> fracpartStr <> "%"

      formatAsScientific :: Either String Text
      formatAsScientific = do
        f <- argAsFloat val
        let precision = (fromDefault 5 $ fieldSpecPrecision spec)
            (mantissaDigits, exponentInt) = floatToDigits 10 (abs f)
            sign = getFloatSign f
            mantissaStr = applyGrouping . mconcat . map Text.show . take precision $ mantissaDigits
            expStr = Text.show exponentInt
        pure $ sign <> "0." <> mantissaStr <> "e" <> expStr

      insertThousandsSep :: Text -> Text -> Text
      insertThousandsSep = insertSep 3

      insertHexSep :: Text -> Text -> Text
      insertHexSep = insertSep 4

      insertSep :: Int -> Text -> Text -> Text
      insertSep n sep src =
        let chunks = reverse . map Text.reverse . Text.chunksOf n . Text.reverse $ src
        in Text.intercalate sep chunks

      applyGrouping :: Text -> Text
      applyGrouping src =
        case fieldSpecGrouping spec of
          NoGrouping -> src
          GroupComma -> insertThousandsSep "," src
          GroupUnderscore -> insertThousandsSep "_" src

      applyHexGrouping :: Text -> Text
      applyHexGrouping src =
        case fieldSpecGrouping spec of
          NoGrouping -> src
          GroupComma -> src
          GroupUnderscore -> insertHexSep "_" src

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
      FormatInvalid -> Left "Cannot format non-scalar as 'general'"

    FieldTypeDecimalInt -> formatAsInt

    FieldTypeHex -> formatAsHex
    FieldTypeHexUpper -> Text.toUpper <$> formatAsHex
    FieldTypeOctal -> formatAsOctal
    FieldTypeBinary -> formatAsBinary

    FieldTypeFixedPoint -> formatAsFixed
    FieldTypeFixedPointUpper -> Text.toUpper <$> formatAsFixed
    FieldTypeScientific -> formatAsScientific
    FieldTypeScientificUpper -> Text.toUpper <$> formatAsScientific

    FieldTypePercentage -> formatAsPercentage

    FieldTypeString -> formatAsString
    t -> Left $ "Field type " ++ show t ++ " not implemented"

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

lookupFormatItemArg :: Vector FormatArg
                    -> Map Text FormatArg
                    -> FieldName
                    -> Either String FormatArg
lookupFormatItemArg _args kwargs (FieldNameIdentifier n) =
  maybe (Left $ "Field not found: " ++ show n) pure $
    Map.lookup n kwargs
lookupFormatItemArg args _kwargs (FieldNameNumber i) =
  maybe (Left $ "Field not found: " ++ show i) pure $
    args Vector.!? fromInteger i
lookupFormatItemArg args kwargs (FieldNameAttrib a b) =
  lookupFormatItemArg args kwargs b >>= lookupAttrib a
lookupFormatItemArg args kwargs (FieldNameKeyIndex a b) =
  lookupFormatItemArg args kwargs b >>= lookupAttrib a
lookupFormatItemArg args kwargs (FieldNameNumIndex a b) =
  lookupFormatItemArg args kwargs b >>= lookupIndex a

--------------------------------------------------------------------------------
-- Format string AST
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

type P a = P.Parsec Void Text a

pFormat :: P [FormatItem]
pFormat = P.many pFormatItem <* P.eof

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
  P.char '!' *>
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
