{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ginger.Interpret.Builtins
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Char (isUpper, isLower, isAlphaNum, isPrint, isSpace, isAlpha, isDigit, ord)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Text.Read (readMaybe)
import Data.Bits (popCount)
import Data.List (sortBy)

--------------------------------------------------------------------------------
-- Builtins
--------------------------------------------------------------------------------

type BuiltinAttribs a m = Map Identifier (a -> m (Either RuntimeError (Value m)))

builtinFunctions :: forall m. Monad m => Map Identifier (Value m)
builtinFunctions = Map.fromList $
  [ ("abs", numericBuiltin abs abs)
  , ("attr", toValue (\x y -> case y :: Value m of
                                StringV yStr ->
                                  fmap (fromMaybe NoneV) <$> getAttrRaw @m x (Identifier yStr)
                                _ ->
                                  pure . Right $ NoneV))
  , ("batch", ProcedureV fnBatch)
  , ("capitalize", textBuiltin Text.toTitle)
  , ("center", ProcedureV fnCenter)
  , ("count", ProcedureV fnLength)
  , ("dictsort", ProcedureV fnDictsort)
  , ("escape", ProcedureV fnEscape)
  , ("even", intBuiltin even)
  -- , ("filesizeformat", undefined)
  , ("first", ProcedureV fnFirst)
  , ("float", ProcedureV fnToFloat)
  -- , ("forceescape", undefined)
  -- , ("format", undefined)
  -- , ("groupby", undefined)
  -- , ("indent", undefined)
  , ("int", ProcedureV fnToInt)
  , ("items", ProcedureV fnItems)
  , ("join", ProcedureV fnJoin)
  , ("last", ProcedureV fnLast)
  , ("length", ProcedureV fnLength)
  -- , ("list", undefined)
  , ("lower", textBuiltin Text.toLower)
  -- , ("map", undefined)
  -- , ("max", undefined)
  -- , ("min", undefined)
  , ("odd", intBuiltin odd)
  -- , ("pprint", undefined)
  -- , ("random", undefined)
  -- , ("rejectattr", undefined)
  -- , ("reject", undefined)
  , ("replace", ProcedureV fnStrReplace)
  -- , ("reverse", undefined)
  , ("round", ProcedureV fnRound)
  -- , ("safe", undefined)
  -- , ("selectattr", undefined)
  -- , ("select", undefined)
  -- , ("slice", undefined)
  -- , ("sort", undefined)
  -- , ("string", undefined)
  -- , ("striptags", undefined)
  -- , ("sum", undefined)
  , ("title", textBuiltin Text.toTitle)
  -- , ("tojson", undefined)
  -- , ("trim", undefined)
  -- , ("truncate", undefined)
  -- , ("unique", undefined)
  , ("upper", textBuiltin Text.toUpper)
  -- , ("urlencode", undefined)
  -- , ("urlize", undefined)
  , ("wordcount", textBuiltin (length . Text.words))
  -- , ("wordwrap", undefined)
  -- , ("xmlattr", undefined)
  ]

builtinIntAttribs :: forall m. Monad m => BuiltinAttribs Integer m
builtinIntAttribs = Map.fromList
  [ ("denominator", intProp (const (1 :: Integer)))
  , ("bit_count", intAttrib popCount)
  -- , ("bit_length", ?)
  -- , ("conjugate", ?)
  -- , ("from_bytes", ?)
  -- , ("to_bytes", ?)
  , ("imag", intProp (const (0 :: Integer)))
  , ("numerator", intProp id)
  , ("real", intProp id)
  ]

builtinFloatAttribs :: Monad m => BuiltinAttribs Double m
builtinFloatAttribs = Map.fromList
  [ ("imag", floatProp (const (0 :: Double)))
  , ("real", floatProp id)
  -- , ("is_integer", ?)
  -- , ("hex", ?)
  -- , ("as_integer_ratio", ?)
  -- , ("conjugate", ?)
  ]

builtinBoolAttribs :: Monad m => BuiltinAttribs Bool m
builtinBoolAttribs = Map.fromList
  [ ("denominator", boolProp (const (1 :: Integer)))
  , ("bit_count", boolAttrib popCount)
  -- , ("bit_length", ?)
  -- , ("conjugate", ?)
  -- , ("from_bytes", ?)
  , ("to_bytes", boolProp (BS.singleton . fromIntegral . fromEnum))
  , ("imag", boolProp (const (0 :: Integer)))
  , ("numerator", boolProp fromEnum)
  , ("real", boolProp fromEnum)
  ]

builtinStringAttribs :: forall m. Monad m => BuiltinAttribs Text m
builtinStringAttribs = Map.fromList
  [ ("length", textProp Text.length)
  , ("capitalize", textAttrib Text.toTitle)
  , ("casefold", textAttrib Text.toCaseFold)
  , ("center", textProcAttrib fnCenter)
  , ("count", textProcAttrib fnStrCount)
  , ("encode", textProcAttrib fnStrEncode)
  , ("endswith", textProcAttrib fnStrEndswith)
  -- , ("expandtabs", ?)
  -- , ("find", ?)
  -- , ("format", ?)
  -- , ("format_map", ?)
  -- , ("index", ?)
  , ("isalnum", textAttrib (Text.all isAlphaNum))
  , ("isalpha", textAttrib (Text.all isAlpha))
  , ("isascii", textAttrib (Text.all ((< 128) . ord)))
  -- , ("isdecimal", ?)
  , ("isdigit", textAttrib (Text.all isDigit))
  -- , ("isidentifier", ?)
  , ("islower", textNProcAttrib isLowerVal)
  -- , ("isnumeric", ?)
  , ("isprintable", textAttrib (Text.all isPrint))
  , ("isspace", textAttrib (Text.all isSpace))
  , ("isupper", textNProcAttrib isUpperVal)
  , ("join", textProcAttrib fnStrJoin)
  -- , ("ljust", ?)
  , ("lower", textAttrib Text.toLower)
  , ("lstrip", textProcAttrib fnStrLStrip)
  -- , ("maketrans", ?)
  -- , ("partition", ?)
  -- , ("removeprefix", ?)
  -- , ("removesuffix", ?)
  , ("replace", textProcAttrib fnStrReplace)
  -- , ("rfind", ?)
  -- , ("rindex", ?)
  -- , ("rjust", ?)
  -- , ("rpartition", ?)
  , ("rstrip", textProcAttrib fnStrRStrip)
  , ("split", textProcAttrib fnStrSplit)
  , ("splitlines", textAttrib Text.lines)
  , ("startswith", textProcAttrib fnStrStartswith)
  , ("strip", textProcAttrib fnStrStrip)
  -- , ("swapcase", ?)
  , ("title", textAttrib Text.toTitle)
  -- , ("translate", ?)
  , ("upper", textAttrib Text.toUpper)
  -- , ("zfill", ?)
  ]

builtinListAttribs :: Monad m => BuiltinAttribs [Value m] m
builtinListAttribs = Map.fromList
  [
  ]

builtinDictAttribs :: Monad m => BuiltinAttribs (Map Scalar (Value m)) m
builtinDictAttribs = Map.fromList
  [
  ]

--------------------------------------------------------------------------------
-- Built-in function implementations
--------------------------------------------------------------------------------

fnLength :: forall m. Monad m => Procedure m
fnLength = mkFn1 "length"
              ("value", Nothing :: Maybe (Value m))
  $ \case
      StringV s -> pure $ Text.length s
      ListV xs -> pure $ length xs
      DictV xs -> pure $ Map.size xs
      x -> 
          throwError $
            ArgumentError
              (Just "length")
              (Just "value")
              (Just "iterable")
              (Just . tagNameOf $ x)

fnEscape :: forall m. Monad m => Procedure m
fnEscape = mkFn1' "escape"
              ("value", Nothing)
  $ \ctx value ->
        (EncodedV @m) <$>
          encodeWith ctx value

fnToFloat :: forall m. Monad m => Procedure m
fnToFloat = mkFn2 "float"
              ("value", Nothing :: Maybe (Value m))
              ("default", Just 0)
  $ \value def ->
  case value of
      IntV i -> pure $ fromIntegral i
      BoolV b -> pure $ fromIntegral $ fromEnum b
      FloatV f -> pure f
      NoneV -> pure 0
      StringV s ->
          pure . fromMaybe def $ readMaybe (Text.unpack s)
      _ -> pure def

fnToInt :: forall m. Monad m => Procedure m
fnToInt = mkFn3 "int"
              ("value", Nothing :: Maybe (Value m))
              ("default", Just 0)
              ("base", Just 10 :: Maybe Integer)
  $ \value def _base ->
  case value of
      IntV i -> pure i
      BoolV b -> pure $ fromIntegral $ fromEnum b
      FloatV f -> pure $ round f
      NoneV -> pure 0
      StringV s ->
        pure . fromMaybe def $ readMaybe (Text.unpack s)
      _ -> pure def

fnItems :: forall m. Monad m => Procedure m
fnItems = mkFn1 "items"
            ("value", Nothing)
  $ \value ->
      pure (Map.toAscList value :: [(Scalar, Value m)])

data DictSortBy
  = ByKey
  | ByValue
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToValue DictSortBy m where
  toValue ByKey = StringV "key"
  toValue ByValue = StringV "value"

instance (Monad m) => FromValue DictSortBy m where
  fromValue (StringV "key") = pure . Right $ ByKey
  fromValue (StringV "value") = pure . Right $ ByValue
  fromValue (StringV x) = pure . Left $ TagError Nothing (Just "'key' or 'value'") (Just $ "string " <> Text.show x)
  fromValue x = pure . Left $ TagError Nothing (Just "string") (Just . tagNameOf $ x)

fnDictsort :: forall m. Monad m => Procedure m
fnDictsort = mkFn4 "dictsort"
              ("value", Nothing :: Maybe (Map Scalar (Value m)))
              ("case_sensitive", Just False)
              ("by", Just ByKey)
              ("reverse", Just False)
  $ \value caseSensitive by reverseSort -> do
    let cmp a b = if caseSensitive then
                    compare (fst a) (fst b)
                  else
                    compare (Text.toCaseFold (fst a)) (Text.toCaseFold (fst b))
        cmp' = if reverseSort then flip cmp else cmp
        proj (k, v) = do
            sk <- case by of
                        ByKey ->
                          case k of
                            StringScalar s -> pure s
                            IntScalar i -> pure $ Text.show i
                            FloatScalar f -> pure $ Text.show f
                            NoneScalar -> pure ""
                            BoolScalar b -> pure . Text.toLower $ Text.show b
                            EncodedScalar (Encoded e) -> pure e
                            BytesScalar bs -> pure $ Text.decodeUtf8 bs
                        ByValue ->
                          stringify v
            pure (sk, (k, v))
        itemsRaw = Map.toList value
    items' <- mapM proj itemsRaw
    pure $ map snd $ sortBy cmp' items'

fnRound :: forall m. Monad m => Procedure m
fnRound = mkFn3 "round"
              ("value", Nothing :: Maybe Double)
              ("precision", Just 0 :: Maybe Integer)
              ("method", Just "common")
              $ \value precision method -> do
  (r :: Double -> Integer) <- case method of
    "common" -> pure (floor . (+ 0.5))
    "ceil" -> pure ceiling
    "floor" -> pure floor
    x -> throwError $ ArgumentError (Just "round") (Just "method") (Just "one of 'common', 'floor', 'ceil'") (Just x)
  if precision == 0 then
    pure $ fromIntegral $ r value
  else do
    let factor :: Double = 10 ** (fromIntegral precision)
    pure $ fromIntegral (r (value * factor) :: Integer) / factor

fnStrReplace :: Monad m => Procedure m
fnStrReplace = mkFn4 "replace"
                ("value", Nothing)
                ("old", Nothing)
                ("new", Nothing)
                ("count", Just (Nothing :: Maybe Int))
                $ \value old new countMay -> do
  let parts = Text.splitOn old value
  case countMay of
    Nothing -> pure $ Text.intercalate new parts
    Just 0 -> pure value
    Just count | count < length parts ->
      pure $
        Text.intercalate new (take count parts) <>
        new <>
        Text.intercalate old (drop count parts)
    Just _ -> pure $ Text.intercalate new parts


fnStrStrip :: Monad m => Procedure m
fnStrStrip = mkFn2 "strip"
                ("value", Nothing)
                ("chars", Just Nothing)
                $ \value charsMay -> do
  case charsMay of
    Nothing -> pure $ Text.strip value
    Just charsText -> do
      let chars = Text.unpack charsText
      pure $
        Text.dropWhile (`elem` chars) .
        Text.dropWhileEnd (`elem` chars) $
        value

fnStrLStrip :: Monad m => Procedure m
fnStrLStrip = mkFn2 "lstrip"
                ("value", Nothing)
                ("chars", Just Nothing)
                $ \value charsMay -> do
  case charsMay of
    Nothing -> pure $ Text.stripStart value
    Just charsText -> do
      let chars = Text.unpack charsText
      pure $ Text.dropWhile (`elem` chars) value

fnStrRStrip :: Monad m => Procedure m
fnStrRStrip = mkFn2 "rstrip"
                ("value", Nothing)
                ("chars", Just Nothing)
                $ \value charsMay -> do
  case charsMay of
    Nothing -> pure $ Text.stripEnd value
    Just charsText -> do
      let chars = Text.unpack charsText
      pure $ Text.dropWhileEnd (`elem` chars) value


fnJoin :: forall m. Monad m => Procedure m
fnJoin = mkFn3 "join"
                ("iterable", Nothing :: Maybe [Value m])
                ("d", Just "" :: Maybe Text)
                ("attr", Just Nothing :: Maybe (Maybe Text))
                $ \iterable d attrMay -> do
  iterable' <- case attrMay of
    Nothing -> pure iterable
    Just attr -> catMaybes <$> mapM (eitherExceptM . \x -> getAttrRaw x (Identifier attr)) iterable
  Text.intercalate d <$> mapM (eitherExcept . asTextVal) iterable'

fnStrJoin :: Monad m => Procedure m
fnStrJoin = mkFn2 "join"
                ("value", Nothing)
                ("iterable", Just [])
                $ \value iterable -> do
  pure $ Text.intercalate value iterable

fnStrSplit :: Monad m => Procedure m
fnStrSplit = mkFn3 "split"
                ("value", Nothing)
                ("sep", Just Nothing)
                ("maxsplit", Just Nothing)
                $ \value sepMay maxsplitMay -> do
  items <- case sepMay of
    Nothing -> pure . Text.words . Text.strip $ value
    Just "" -> throwError $
                  ArgumentError
                    (Just "split")
                    (Just "sep")
                    (Just "non-empty string")
                    (Just "empty string")
    Just sep -> pure $ Text.splitOn sep value
  case maxsplitMay of
    Nothing ->
      pure items
    Just maxsplit ->
      pure $ take maxsplit items ++ [Text.unwords $ drop maxsplit items]

fnStrStartswith :: Monad m => Procedure m
fnStrStartswith = mkFn4 "startswith"
                  ("value", Nothing)
                  ("prefix", Nothing)
                  ("start", Just 0)
                  ("end", Just Nothing)
                  $ \value prefix start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ prefix `Text.isPrefixOf` value'

fnStrEndswith :: Monad m => Procedure m
fnStrEndswith = mkFn4 "endswith"
                  ("value", Nothing)
                  ("suffix", Nothing)
                  ("start", Just 0)
                  ("end", Just Nothing)
                  $ \value suffix start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ suffix `Text.isSuffixOf` value'

fnStrEncode :: Monad m => Procedure m
fnStrEncode = mkFn3 "encode"
                ("value", Nothing)
                ("encoding", Just "utf-8")
                ("errors", Just ("strict" :: Text))
                $ \value encoding _errors -> do
  func <- case Text.filter isAlphaNum . Text.toCaseFold $ encoding of
    "ascii" -> pure encodeASCII
    "utf8" -> pure Text.encodeUtf8
    "utf16le" -> pure Text.encodeUtf16LE
    "utf16be" -> pure Text.encodeUtf16BE
    "utf32le" -> pure Text.encodeUtf32LE
    "utf32be" -> pure Text.encodeUtf32BE
    _ -> throwError $ ArgumentError (Just "encode") (Just "encoding") (Just "valid encoding") (Just encoding)
  pure $ func value
  where
    encodeASCII =
      BS.pack .
      map fromIntegral .
      filter (<= 127) .
      map ord .
      Text.unpack

fnStrCount :: Monad m => Procedure m
fnStrCount = mkFn4 "count"
              ("value", Nothing)
              ("sub", Nothing)
              ("start", Just 0)
              ("end", Just Nothing)
              $ \value sub start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ Text.count sub value'

fnCenter :: Monad m => Procedure m
fnCenter = mkFn3 "center"
            ("value", Nothing)
            ("width", Just 80)
            ("fillchar", Just " ")
            $ \value width fillchar' -> do
    let fillchar = Text.take 1 . (<> " ") $ fillchar'
    let paddingTotal = max 0 $ fromInteger width - Text.length value
        paddingLeft = paddingTotal `div` 2
        paddingRight = paddingTotal - paddingLeft
    pure $
      Text.replicate paddingLeft fillchar <>
      value <>
      Text.replicate paddingRight fillchar

fnBatch :: forall m. Monad m => Procedure m
fnBatch = mkFn3 "batch"
            ("value", Nothing)
            ("linecount", Nothing)
            ("fill_with", Just Nothing)
            $ \value linecount fillWithMay -> do
  pure $ chunksOf fillWithMay linecount value
  where
    chunksOf :: Maybe (Value m) -> Int -> [Value m] -> [[Value m]]
    chunksOf _ _ [] = []
    chunksOf fillMay n xs =
      case take n xs of
        xs' | length xs' < n ->
          let paddingLength = n - length xs'
              padding = case (fillMay, paddingLength) of
                          (Just fill, p) | p > 0 ->
                            replicate paddingLength fill
                          _ -> []
          in [xs' ++ padding]
        xs' -> xs' : chunksOf fillMay n (drop n xs)

fnFirst :: forall m. Monad m => Procedure m
fnFirst = mkFn1 "first"
            ("value", Nothing :: Maybe (Value m))
  $ \case
    ListV (x:_) -> pure x
    ListV [] -> pure NoneV
    StringV txt -> pure $ StringV $ Text.take 1 txt
    EncodedV (Encoded txt) -> pure $ EncodedV . Encoded $ Text.take 1 txt
    BytesV arr -> pure . toValue $ BS.indexMaybe arr 0
    x -> throwError $ ArgumentError (Just "first") (Just "value") (Just "list or string") (Just . tagNameOf $ x)
      
fnLast :: forall m. Monad m => Procedure m
fnLast = mkFn1 "first"
            ("value", Nothing :: Maybe (Value m))
  $ \case
    ListV [] -> pure NoneV
    ListV xs -> pure (last xs)
    StringV txt -> pure $ StringV $ Text.takeEnd 1 txt
    BytesV arr -> pure . toValue $ BS.indexMaybe arr (BS.length arr - 1)
    EncodedV (Encoded txt) -> pure $ EncodedV . Encoded $ Text.takeEnd 1 txt
    x -> throwError $ ArgumentError (Just "first") (Just "value") (Just "list or string") (Just . tagNameOf $ x)

isUpperVal :: Value m -> Value m
isUpperVal (StringV txt) = BoolV (Text.all isUpper txt)
isUpperVal (EncodedV (Encoded txt)) = BoolV (Text.all isUpper txt)
isUpperVal _ = FalseV

isLowerVal :: Value m -> Value m
isLowerVal (StringV txt) = BoolV (Text.all isLower txt)
isLowerVal (EncodedV (Encoded txt)) = BoolV (Text.all isLower txt)
isLowerVal _ = FalseV

isBoolean :: Bool -> Value m -> Value m
isBoolean b1 (BoolV b2) = BoolV (b1 == b2)
isBoolean _ _ = FalseV

isNone :: Value m -> Value m
isNone NoneV = TrueV
isNone _ = FalseV

--------------------------------------------------------------------------------
-- Text conversion
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

allEitherBool :: [(Either a Bool)] -> Either a Bool
allEitherBool [] = Right True
allEitherBool (Right True : xs) = allEitherBool xs
allEitherBool (x : _) = x

--------------------------------------------------------------------------------
-- Attribute and item helpers
--------------------------------------------------------------------------------

getAttrRaw :: Monad m
        => Value m
        -> Identifier
        -> m (Either RuntimeError (Maybe (Value m)))
getAttrRaw (NativeV n) v =
  Right <$> nativeObjectGetAttribute n v
getAttrRaw (StringV s) v =
  case (Map.lookup v builtinStringAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib s
getAttrRaw (BoolV x) v =
  case (Map.lookup v builtinBoolAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (IntV x) v =
  case (Map.lookup v builtinIntAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (FloatV x) v =
  case (Map.lookup v builtinFloatAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (ListV xs) v =
  case (Map.lookup v builtinListAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib xs
getAttrRaw (DictV xs) v =
  case (Map.lookup v builtinDictAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib xs
getAttrRaw _ _ = pure $ Right Nothing

getItemRaw :: Monad m
           => Value m
           -> Value m
           -> m (Maybe (Value m))
getItemRaw a b = case a of
  DictV m -> case b of
    ScalarV k -> pure $ toValue <$> k `Map.lookup` m
    _ -> pure Nothing
  ListV xs -> case b of
    IntV i -> pure . fmap toValue . listToMaybe . drop (fromInteger i) $ xs
    _ -> pure Nothing
  StringV str -> case b of
    IntV i -> pure
              . fmap (toValue . Text.singleton)
              . listToMaybe
              . Text.unpack
              . Text.take 1
              . Text.drop (fromInteger i)
              $ str
    _ -> pure Nothing
  NativeV n -> case b of
    ScalarV k -> nativeObjectGetField n k
    _ -> pure Nothing
  _ -> pure Nothing

--------------------------------------------------------------------------------
-- Method and property conversion helpers
--------------------------------------------------------------------------------

nativeMethod :: Procedure m -> Value m -> Value m
nativeMethod (NativeProcedure f) self =
  ProcedureV . NativeProcedure $ \args -> f ((Just "value", self) : args)
nativeMethod (GingerProcedure env argSpec body) self =
  ProcedureV $ GingerProcedure env' (drop 1 argSpec) body
  where
    env' = env { envVars = Map.insert "value" self (envVars env) }

nativePureMethod :: Monad m
                 => (Value m -> Either RuntimeError (Value m))
                 -> Value m
                 -> Value m
nativePureMethod = nativeMethod . pureNativeFunc

toNativeMethod :: ToNativeProcedure m a
               => a
               -> Value m
               -> Value m
toNativeMethod f = nativeMethod (NativeProcedure $ toNativeProcedure f)

pureAttrib :: Applicative m => (s -> a) -> s -> m (Either RuntimeError a)
pureAttrib f x = pure . Right $ f x

textBuiltin :: (Monad m, ToValue a m)
            => (Text -> a)
            -> Value m
textBuiltin f =
  ProcedureV .
  pureNativeFunc .
  textFunc $
  (Right . f)

intBuiltin :: (Monad m, ToValue a m)
            => (Integer -> a)
            -> Value m
intBuiltin f =
  ProcedureV .
  pureNativeFunc .
  intFunc $
  (Right . f)

numericBuiltin :: (Monad m)
            => (Integer -> Integer)
            -> (Double -> Double)
            -> Value m
numericBuiltin f g =
  ProcedureV .
  pureNativeFunc $
  numericFunc f g

anyBuiltin :: (Monad m, FromValue a m, ToValue b m)
            => (a -> b)
            -> Value m
anyBuiltin f =
  ProcedureV .
  nativeFunc $ \x -> runExceptT $
    toValue . f <$> eitherExceptM (fromValue x)


boolProp :: (Monad m, ToValue a m)
         => (Bool -> a)
         -> Bool
         -> m (Either RuntimeError (Value m))
boolProp f t = pure . Right . toValue $ f t

boolAttrib :: (Monad m, ToValue a m)
           => (Bool -> a)
           -> Bool
           -> m (Either RuntimeError (Value m))
boolAttrib f =
  pureAttrib $ nativePureMethod (boolFunc f) . BoolV

boolNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => (Value m -> a)
                -> Bool
                -> m (Either RuntimeError (Value m))
boolNProcAttrib f =
  pureAttrib $ toNativeMethod f . BoolV

boolProcAttrib :: Monad m
               => Procedure m
               -> Bool
               -> m (Either RuntimeError (Value m))
boolProcAttrib f =
  pureAttrib $ nativeMethod f . BoolV


intProp :: (Monad m, ToValue a m)
         => (Integer -> a)
         -> Integer
         -> m (Either RuntimeError (Value m))
intProp f t = pure . Right . toValue $ f t

intAttrib :: (Monad m, ToValue a m)
           => (Integer -> a)
           -> Integer
           -> m (Either RuntimeError (Value m))
intAttrib f =
  pureAttrib $ nativePureMethod (intFunc (pure . f)) . IntV

intNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => (Value m -> a)
                -> Integer
                -> m (Either RuntimeError (Value m))
intNProcAttrib f =
  pureAttrib $ toNativeMethod f . IntV

intProcAttrib :: Monad m
               => Procedure m
               -> Integer
               -> m (Either RuntimeError (Value m))
intProcAttrib f =
  pureAttrib $ nativeMethod f . IntV


floatProp :: (Monad m, ToValue a m)
         => (Double -> a)
         -> Double
         -> m (Either RuntimeError (Value m))
floatProp f t = pure . Right . toValue $ f t

floatAttrib :: (Monad m, ToValue a m)
           => (Double -> a)
           -> Double
           -> m (Either RuntimeError (Value m))
floatAttrib f =
  pureAttrib $ nativePureMethod (floatFunc (pure . f)) . FloatV

floatNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => (Value m -> a)
                -> Double
                -> m (Either RuntimeError (Value m))
floatNProcAttrib f =
  pureAttrib $ toNativeMethod f . FloatV

floatProcAttrib :: Monad m
               => Procedure m
               -> Double
               -> m (Either RuntimeError (Value m))
floatProcAttrib f =
  pureAttrib $ nativeMethod f . FloatV


textProp :: (Monad m, ToValue a m)
         => (Text -> a)
         -> Text
         -> m (Either RuntimeError (Value m))
textProp f t = pure . Right . toValue $ f t

textAttrib :: (Monad m, ToValue a m)
           => (Text -> a)
           -> Text
           -> m (Either RuntimeError (Value m))
textAttrib f =
  pureAttrib $ nativePureMethod (textFunc (pure . f)) . StringV

textNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => (Value m -> a)
                -> Text
                -> m (Either RuntimeError (Value m))
textNProcAttrib f =
  pureAttrib $ toNativeMethod f . StringV

textProcAttrib :: Monad m
               => Procedure m
               -> Text
               -> m (Either RuntimeError (Value m))
textProcAttrib f =
  pureAttrib $ nativeMethod f . StringV

builtinNotImplemented :: Monad m => Text -> Value m
builtinNotImplemented name = ProcedureV $ NativeProcedure $ \_ _ ->
  pure . Left $ NotImplementedError (Just name)

fnMaybeArg :: Monad m => Text -> Text -> Maybe b -> ExceptT RuntimeError m b
fnMaybeArg context name =
  maybe
    (throwError $
        ArgumentError
          (Just context)
          (Just name)
          (Just "argument")
          (Just "end of arguments")
    )
    pure

fnArg :: (Monad m, FromValue a m)
      => Text
      -> Identifier
      -> Map Identifier (Value m)
      -> ExceptT RuntimeError m a
fnArg context name argValues = do
  argV <- fnMaybeArg context (identifierName name) $ Map.lookup name argValues
  eitherExceptM $ fromValue argV

mkFn0' :: ( Monad m
         , ToValue r m
         )
      => Text
      -> (Context m -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn0' funcName f =
  NativeProcedure $ \args ctx -> runExceptT $ do
    _ <- eitherExcept $
      resolveArgs
        (Just funcName)
        []
        args
    toValue <$> f ctx

mkFn0 :: ( Monad m
         , ToValue r m
         )
      => Text
      -> (ExceptT RuntimeError m r)
      -> Procedure m
mkFn0 funcName f =
  mkFn0' funcName (const f)

mkFn1' :: ( Monad m
         , ToValue a m
         , FromValue a m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a)
      -> (Context m -> a -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn1' funcName (argname1, default1) f =
  NativeProcedure $ \args ctx -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    toValue <$> f ctx arg1

mkFn1 :: ( Monad m
         , ToValue a m
         , FromValue a m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a)
      -> (a -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn1 funcName a f =
  mkFn1' funcName a (const f)

mkFn2' :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (Context m -> a1 -> a2 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn2' funcName
    (argname1, default1)
    (argname2, default2)
    f =
  NativeProcedure $ \args ctx -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        , (argname2, toValue <$> default2)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    arg2 <- fnArg funcName argname2 argValues
    toValue <$> f ctx arg1 arg2

mkFn2 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (a1 -> a2 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn2 funcName a b f =
  mkFn2' funcName a b (const f)

mkFn3' :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (Identifier, Maybe a3)
      -> (Context m -> a1 -> a2 -> a3 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn3' funcName
    (argname1, default1)
    (argname2, default2)
    (argname3, default3)
    f =
  NativeProcedure $ \args ctx -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        , (argname2, toValue <$> default2)
        , (argname3, toValue <$> default3)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    arg2 <- fnArg funcName argname2 argValues
    arg3 <- fnArg funcName argname3 argValues
    toValue <$> f ctx arg1 arg2 arg3

mkFn3 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (Identifier, Maybe a3)
      -> (a1 -> a2 -> a3 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn3 funcName a b c f =
  mkFn3' funcName a b c (const f)

mkFn4' :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue a4 m
         , FromValue a4 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (Identifier, Maybe a3)
      -> (Identifier, Maybe a4)
      -> (Context m -> a1 -> a2 -> a3 -> a4 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn4' funcName
    (argname1, default1)
    (argname2, default2)
    (argname3, default3)
    (argname4, default4)
    f =
  NativeProcedure $ \args ctx -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        , (argname2, toValue <$> default2)
        , (argname3, toValue <$> default3)
        , (argname4, toValue <$> default4)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    arg2 <- fnArg funcName argname2 argValues
    arg3 <- fnArg funcName argname3 argValues
    arg4 <- fnArg funcName argname4 argValues
    toValue <$> f ctx arg1 arg2 arg3 arg4

mkFn4 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue a4 m
         , FromValue a4 m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a1)
      -> (Identifier, Maybe a2)
      -> (Identifier, Maybe a3)
      -> (Identifier, Maybe a4)
      -> (a1 -> a2 -> a3 -> a4 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn4 funcName a b c d f =
  mkFn4' funcName a b c d (const f)
