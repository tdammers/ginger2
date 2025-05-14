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

type BuiltinAttribs a m = Map Identifier (a -> m (Either RuntimeError (Value m)))

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

builtinIntAttribs :: forall m. Monad m => BuiltinAttribs Integer m
builtinIntAttribs = Map.fromList
  [
  ]

builtinFloatAttribs :: Monad m => BuiltinAttribs Double m
builtinFloatAttribs = Map.fromList
  [
  ]

builtinBoolAttribs :: Monad m => BuiltinAttribs Bool m
builtinBoolAttribs = Map.fromList
  [
  ]

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
  -- , ("dictsort", undefined)
  -- , ("escape", undefined)
  , ("even", intBuiltin even)
  -- , ("filesizeformat", undefined)
  -- , ("first", undefined)
  , ("float", ProcedureV fnToFloat)
  -- , ("forceescape", undefined)
  -- , ("format", undefined)
  -- , ("groupby", undefined)
  -- , ("indent", undefined)
  , ("int", ProcedureV fnToInt)
  -- , ("items", undefined)
  , ("join", ProcedureV fnJoin)
  -- , ("last", undefined)
  -- , ("length", undefined)
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
  -- , ("round", undefined)
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

builtinNotImplemented :: Monad m => Text -> Value m
builtinNotImplemented name = ProcedureV $ NativeProcedure $ \_ ->
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

mkFn1 :: ( Monad m
         , ToValue a m
         , FromValue a m
         , ToValue r m
         )
      => Text
      -> (Identifier, Maybe a)
      -> (a -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn1 funcName (argname1, default1) f =
  NativeProcedure $ \args -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    toValue <$> f arg1

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
mkFn2 funcName
    (argname1, default1)
    (argname2, default2)
    f =
  NativeProcedure $ \args -> runExceptT $ do
    argValues <- eitherExcept $
      resolveArgs
        (Just funcName)
        [ (argname1, toValue <$> default1)
        , (argname2, toValue <$> default2)
        ]
        args
    arg1 <- fnArg funcName argname1 argValues
    arg2 <- fnArg funcName argname2 argValues
    toValue <$> f arg1 arg2

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
mkFn3 funcName
    (argname1, default1)
    (argname2, default2)
    (argname3, default3)
    f =
  NativeProcedure $ \args -> runExceptT $ do
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
    toValue <$> f arg1 arg2 arg3

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
mkFn4 funcName
    (argname1, default1)
    (argname2, default2)
    (argname3, default3)
    (argname4, default4)
    f =
  NativeProcedure $ \args -> runExceptT $ do
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
    toValue <$> f arg1 arg2 arg3 arg4

fnToFloat :: forall m. Monad m => Procedure m
fnToFloat = mkFn1 "float"
              ("value", Nothing :: Maybe (Value m))
  $ \case
      IntV i -> pure $ fromIntegral i
      FloatV f -> pure f
      NoneV -> pure 0
      StringV s ->
        maybe
          (throwError $
            ArgumentError
              (Just "float")
              (Just "value")
              (Just "float-like string")
              (Just s))
          pure $
          readMaybe (Text.unpack s)
      x -> 
          throwError $
            ArgumentError
              (Just "float")
              (Just "value")
              (Just "numeric value")
              (Just . tagNameOf $ x)

fnToInt :: forall m. Monad m => Procedure m
fnToInt = mkFn1 "int"
              ("value", Nothing :: Maybe (Value m))
  $ \case
      IntV i -> pure i
      FloatV f -> pure $ round f
      NoneV -> pure 0
      StringV s ->
        maybe
          (throwError $
            ArgumentError
              (Just "int")
              (Just "value")
              (Just "int-like string")
              (Just s))
          pure $
          readMaybe (Text.unpack s)
      x -> 
          throwError $
            ArgumentError
              (Just "float")
              (Just "value")
              (Just "numeric value")
              (Just . tagNameOf $ x)

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
  NativeV n -> nativeObjectGetField n b
  _ -> pure Nothing

