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
import Language.Ginger.Interpret.Type

import Control.Monad (filterM)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Char (isUpper, isLower, isAlphaNum, isPrint, isSpace, isAlpha, isDigit, ord)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, isJust)
import Text.Read (readMaybe)
import Data.Bits (popCount)
import Data.List (sortBy)
import Text.Printf (printf)
import qualified Text.Regex.TDFA as RE
import qualified Data.Array as Array
import qualified Data.Aeson as JSON
import Data.Time
        ( TimeZone (..)
        , ZonedTime (..)
        , TimeOfDay (..)
        , LocalTime (..)
        , parseTimeM
        , defaultTimeLocale
        , utc
        , fromGregorian
        , utcToZonedTime
        , zonedTimeToUTC
        , formatTime
        )
import Data.Foldable (asum)

--------------------------------------------------------------------------------
-- Builtins
--------------------------------------------------------------------------------

type BuiltinAttribs a m = Map Identifier (a -> m (Either RuntimeError (Value m)))

builtinGlobals :: forall m. Monad m
                 => (Expr -> GingerT m (Value m))
                 -> Map Identifier (Value m)
builtinGlobals evalE = Map.fromList $
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
  , ("date", ProcedureV fnDateFormat)
  , ("dateformat", ProcedureV fnDateFormat)
  , ("dictsort", ProcedureV fnDictsort)
  , ("escape", ProcedureV fnEscape)
  , ("even", intBuiltin even)
  , ("filesizeformat", ProcedureV fnFilesizeFormat)
  , ("first", ProcedureV fnFirst)
  , ("float", ProcedureV fnToFloat)
  -- , ("forceescape", undefined)
  -- , ("format", undefined)
  -- , ("groupby", undefined)
  -- , ("indent", undefined)
  , ("int", ProcedureV fnToInt)
  , ("items", ProcedureV fnItems)
  , ("join", ProcedureV fnJoin)
  , ("json", ProcedureV fnToJSON)
  , ("last", ProcedureV fnLast)
  , ("length", ProcedureV fnLength)
  , ("list", ProcedureV fnToList)
  , ("lower", textBuiltin Text.toLower)
  , ("map", FilterV . NativeFilter $ fnMap evalE)
  -- , ("max", undefined)
  -- , ("min", undefined)
  , ("odd", intBuiltin odd)
  -- , ("pprint", undefined)
  -- , ("random", undefined)
  , ("regex", regexModule)
  -- , ("rejectattr", undefined)
  -- , ("reject", undefined)
  , ("replace", ProcedureV fnStrReplace)
  , ("reverse", ProcedureV fnReverse)
  , ("round", ProcedureV fnRound)
  -- , ("safe", undefined)
  -- , ("selectattr", undefined)
  , ("select", FilterV . NativeFilter $ fnSelect evalE)
  -- , ("slice", undefined)
  , ("sort", ProcedureV fnSort)
  , ("split", ProcedureV fnStrSplit)
  , ("string", ProcedureV fnToString)
  , ("strip", ProcedureV fnStrStrip)
  -- , ("striptags", undefined)
  -- , ("sum", undefined)
  , ("title", textBuiltin Text.toTitle)
  , ("tojson", ProcedureV fnToJSON)
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

regexModule :: forall m. Monad m => Value m
regexModule = dictV
  [ ("match", ProcedureV fnReMatch)
  , ("matches", ProcedureV fnReMatches)
  , ("test", ProcedureV fnReTest)
  ]

runReWith :: forall a m. Monad m
          => (RE.Regex -> Text -> a)
          -> Text
          -> Text
          -> Text
          -> ExceptT RuntimeError m a
runReWith matchFunc regexText haystack optsText = do
    let opts = parseCompOpts optsText
    let regex = RE.makeRegexOpts opts RE.defaultExecOpt (Text.unpack regexText)
    pure $ matchFunc regex haystack

fnReMatch :: forall m. Monad m => Procedure m
fnReMatch = mkFn3 "regex.match"
              ("regex", Nothing)
              ("haystack", Nothing)
              ("opts", Just "")
  $ runReWith (\r h -> convertMatchOnceText $ RE.matchOnceText r h)

fnReMatches :: forall m. Monad m => Procedure m
fnReMatches = mkFn3 "regex.match"
              ("regex", Nothing)
              ("haystack", Nothing)
              ("opts", Just "")
  $ runReWith (\r h -> fmap convertMatchText $ RE.matchAllText r h)

fnReTest :: forall m. Monad m => Procedure m
fnReTest = mkFn3 "regex.match"
              ("regex", Nothing)
              ("haystack", Nothing)
              ("opts", Just "")
  $ runReWith (\r h -> isJust $ RE.matchOnceText r h)

parseCompOpts :: Text -> RE.CompOption
parseCompOpts = do
  Text.foldl'
    (\x ->
      \case
        'i' -> x { RE.caseSensitive = False }
        'm' -> x { RE.multiline = True }
        _ -> x
    )
    RE.blankCompOpt

convertMatchOnceText :: Maybe (Text, RE.MatchText Text, Text) -> Maybe [Text]
convertMatchOnceText Nothing = Nothing
convertMatchOnceText (Just (_, m, _)) = Just (convertMatchText m)

convertMatchText :: RE.MatchText Text -> [Text]
convertMatchText matches =
  map fst $ Array.elems matches

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
              "length"
              "value"
              "iterable"
              (tagNameOf x)

fnEscape :: forall m. Monad m => Procedure m
fnEscape = mkFn1' "escape"
              ("value", Nothing)
  $ \ctx value ->
        (EncodedV @m) <$>
          encodeWith ctx value

fnToList :: forall m. Monad m => Procedure m
fnToList = mkFn1 "list"
              ("value", Nothing :: Maybe (Value m))
  $ \case
    ListV xs -> pure xs
    DictV xs -> pure (Map.elems xs)
    StringV txt ->
      pure $ map (toValue . Text.singleton) $ Text.unpack txt
    EncodedV (Encoded txt) ->
      pure $ map (toValue . Text.singleton) $ Text.unpack txt
    NativeV obj ->
      native (Right <$> nativeObjectAsList obj) >>=
        maybe
          (throwError $
            ArgumentError
              "list"
              "value"
              "iterable"
              "non-iterable native object"
          )
          pure
    BytesV bytes ->
      pure $ map toValue $ BS.unpack bytes
    x -> throwError $
            ArgumentError
              "list"
              "value"
              "iterable"
              (tagNameOf x)


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

fnToString :: forall m. Monad m => Procedure m
fnToString = mkFn1 "string"
              ("value", Nothing :: Maybe (Value m))
  $ \value ->
    stringify value

fnReverse :: forall m. Monad m => Procedure m
fnReverse = mkFn1 "reverse"
              ("value", Nothing)
  $ \case
      Left t -> pure $ StringV (Text.reverse t)
      Right xs -> pure $ ListV (reverse (xs :: [Value m]))

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
  fromValue (StringV x) = pure . Left $ TagError "conversion to dictsort target" "'key' or 'value'" ("string " <> Text.show x)
  fromValue x = pure . Left $ TagError "conversion to dictsort target" "string" (tagNameOf x)

fnSort :: forall m. Monad m => Procedure m
fnSort = mkFn4 "sort"
              ("value", Nothing :: Maybe [Value m])
              ("reverse", Just False)
              ("case_sensitive", Just False)
              ("attribute", Just Nothing :: Maybe (Maybe (Value m)))
  $ \value reverseSort caseSensitive attributeMay -> do
    let cmp a b = if caseSensitive then
                    compare (fst a) (fst b)
                  else
                    compare (Text.toCaseFold (fst a)) (Text.toCaseFold (fst b))
        cmp' = if reverseSort then flip cmp else cmp
        proj x = do
          sk <- case attributeMay of
                  Nothing ->
                    stringify x
                  Just a -> do
                    v <- fmap (fromMaybe NoneV) $ eitherExceptM $ getItemOrAttrRaw x a
                    stringify v
          pure (sk, x)
    (items' :: [(Text, Value m)]) <- mapM proj value
    pure $ map snd $ sortBy cmp' items'



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

fnSelect :: forall m. Monad m
         => (Expr -> GingerT m (Value m))
         -> FilterFunc m
fnSelect evalE scrutineeE args ctx env = runExceptT $ do
  -- This one is quite a monster, because it accepts arguments in so many
  -- different ways.
  -- Specifically:
  --
  -- @scrutinee|map('foobar', args...)@ - interpret the string @'foobar'@ as
  -- the name of a filter (or procedure), and pass @args...@ on to that filter.
  --
  -- @scrutinee|map(foobar, args...)@ - interpret @foobar@ as a filter (or a
  -- procedure), and pass @args...@ on to that filter.
  --
  -- @scrutinee|map(attribute='foobar', {default=value})@ - interpret @'foobar'
  -- as the name of an attribute in each list element, extract that list
  -- element, use the @default=@ value if the attribute is absent.
  let funcName = "select"
  argValues <- eitherExcept $
    resolveArgs
      funcName
      []
      args
  varargs <- fnArg funcName "varargs" argValues
  (kwargs :: Map Scalar (Value m)) <- fnArg funcName "kwargs" argValues
  (scrutinee :: Value m) <- eitherExceptM $ runGingerT (evalE scrutineeE) ctx env
  (xs :: [Value m]) <- eitherExceptM $ fromValue scrutinee

  case varargs of
    [] ->
      -- No argument = error.
      throwError $
        ArgumentError
          funcName
          "attribute/callee"
          "attribute=identifier or callable"
          "no argument"
    (test:varargs') -> do
      -- Re-pack the remaining arguments
      let args' = zip (repeat Nothing) varargs' ++
                  Map.toList (Map.mapKeys toIdentifier kwargs)

      -- Determine how to handle each list element.
      let apply' testV x =
            case testV of
              StringV name -> do
                -- If it's a string, we interpret it as a filter name, and
                -- try to look up the corresponding filter in the current
                -- scope.
                testV' <- eitherExceptM $
                  runGingerT
                    (scoped $ do
                        scopify "jinja-tests"
                        evalE (VarE $ Identifier name)
                    )
                    ctx env
                apply' testV' x
              DictV m -> do
                -- If it's a dict, try to find a @"__call__"@ item.
                case Map.lookup "__call__" m of
                  Nothing -> throwError $
                                NonCallableObjectError
                                  (tagNameOf test <> " " <> Text.show testV)
                  Just v -> apply' v x
              NativeV obj -> do
                -- If it's a native object, use its @nativeObjectCall@
                -- method, if available.
                case nativeObjectCall obj of
                  Nothing -> throwError $
                                NonCallableObjectError
                                  "non-callable native object"
                  Just f -> eitherExceptM (f obj args') >>=
                            eitherExcept . asBoolVal
              TestV f -> do
                -- If it's a test, we apply it as such, mapping the
                -- current list element to the variable "@" (which cannot
                -- be used as a normal identifier, because the syntax
                -- doesn't allow it). We need to bind it, because
                -- 'runFilter' takes an unevaluated expression as its
                -- scrutinee argument, but we have an already-evaluated
                -- value.
                let env' = env { envVars = Map.insert "@" x $ envVars env }
                eitherExceptM $ runTest f (VarE "@") args' ctx env'
              ProcedureV (NativeProcedure f) -> do
                -- If it's a native procedure, we can just call it without
                -- binding anything.
                eitherExceptM (f ((Nothing, x):args') ctx) >>=
                  eitherExcept . asBoolVal
              ProcedureV (GingerProcedure env' argSpecs body) -> do
                -- If it's a ginger procedure, we need to prepend the
                -- current list element to the argument list (so it becomes
                -- the first positional argument), and then resolve and
                -- bind all the arguments into the environment where we
                -- then run the ginger procedure.
                args'' <- eitherExcept $
                            resolveArgs
                              "map callback"
                              argSpecs
                              ((Nothing, x):args')
                eitherExceptM (runGingerT (setVars args'' >> evalE body) ctx env') >>=
                    eitherExcept . asBoolVal
              _ ->
                -- Not something we can call.
                  throwError $
                    NonCallableObjectError
                      (tagNameOf test <> " " <> Text.show testV)

          apply = apply' test

      ListV <$> filterM apply xs
  where
    toIdentifier :: Scalar -> Maybe Identifier
    toIdentifier (StringScalar s) = Just $ Identifier s
    toIdentifier (IntScalar i) = Just $ Identifier (Text.show i)
    toIdentifier (FloatScalar f) = Just $ Identifier (Text.show f) -- dubious
    toIdentifier _ = Nothing

fnMap :: forall m. Monad m
      => (Expr -> GingerT m (Value m))
      -> FilterFunc m
fnMap evalE scrutineeE args ctx env = runExceptT $ do
  -- This one is quite a monster, because it accepts arguments in so many
  -- different ways.
  -- Specifically:
  --
  -- @scrutinee|map('foobar', args...)@ - interpret the string @'foobar'@ as
  -- the name of a filter (or procedure), and pass @args...@ on to that filter.
  --
  -- @scrutinee|map(foobar, args...)@ - interpret @foobar@ as a filter (or a
  -- procedure), and pass @args...@ on to that filter.
  --
  -- @scrutinee|map(attribute='foobar', {default=value})@ - interpret @'foobar'
  -- as the name of an attribute in each list element, extract that list
  -- element, use the @default=@ value if the attribute is absent.
  let funcName = "map"
  argValues <- eitherExcept $
    resolveArgs
      funcName
      []
      args
  varargs <- fnArg funcName "varargs" argValues
  (kwargs :: Map Scalar (Value m)) <- fnArg funcName "kwargs" argValues
  (scrutinee :: Value m) <- eitherExceptM $ runGingerT (evalE scrutineeE) ctx env
  (xs :: [Value m]) <- eitherExceptM $ fromValue scrutinee

  -- First, let's see if an attribute was specified.
  let attributeMay = Map.lookup "attribute" kwargs
  case attributeMay of
    Just attribute -> do
      -- Attribute was specified, so let's extract that.
      -- We also need to find the default value.
      let defVal = fromMaybe NoneV (Map.lookup "default" kwargs) :: Value m
      ListV <$> mapM
        (\x -> do
          attributeIdent <- Identifier <$> eitherExceptM (fromValue attribute)
          fromMaybe defVal <$>
            eitherExceptM
              (getAttrOrItemRaw x attributeIdent)
        )
        xs
    Nothing ->
      -- Attribute was not specified, so we will use the first of the
      -- positional arguments as a mapping function/filter.
      case varargs of
        [] ->
          -- No argument = error.
          throwError $
            ArgumentError
              funcName
              "attribute/callee"
              "attribute=identifier or callable"
              "no argument"
        (callee:varargs') -> do
          -- Re-pack the remaining arguments
          let args' = zip (repeat Nothing) varargs' ++
                      Map.toList (Map.mapKeys toIdentifier kwargs)

          -- Determine how to handle each list element.
          let apply' filterV x =
                case filterV of
                  StringV name -> do
                    -- If it's a string, we interpret it as a filter name, and
                    -- try to look up the corresponding filter in the current
                    -- scope.
                    filterV' <- eitherExceptM $
                      runGingerT
                        (scoped $ do
                            scopify "jinja-filters"
                            evalE (VarE $ Identifier name)
                        )
                        ctx env
                    apply' filterV' x
                  DictV m -> do
                    -- If it's a dict, try to find a @"__call__"@ item.
                    case Map.lookup "__call__" m of
                      Nothing -> throwError $
                                    NonCallableObjectError
                                      (tagNameOf callee <> " " <> Text.show filterV)
                      Just v -> apply' v x
                  NativeV obj -> do
                    -- If it's a native object, use its @nativeObjectCall@
                    -- method, if available.
                    case nativeObjectCall obj of
                      Nothing -> throwError $
                                    NonCallableObjectError
                                      "non-callable native object"
                      Just f -> eitherExceptM $ f obj args'
                  FilterV f -> do
                    -- If it's a filter, we apply it as such, mapping the
                    -- current list element to the variable "@" (which cannot
                    -- be used as a normal identifier, because the syntax
                    -- doesn't allow it). We need to bind it, because
                    -- 'runFilter' takes an unevaluated expression as its
                    -- scrutinee argument, but we have an already-evaluated
                    -- value.
                    let env' = env { envVars = Map.insert "@" x $ envVars env }
                    eitherExceptM $ runFilter f (VarE "@") args' ctx env'
                  ProcedureV (NativeProcedure f) -> do
                    -- If it's a native procedure, we can just call it without
                    -- binding anything.
                    eitherExceptM $ f ((Nothing, x):args') ctx
                  ProcedureV (GingerProcedure env' argSpecs body) -> do
                    -- If it's a ginger procedure, we need to prepend the
                    -- current list element to the argument list (so it becomes
                    -- the first positional argument), and then resolve and
                    -- bind all the arguments into the environment where we
                    -- then run the ginger procedure.
                    args'' <- eitherExcept $
                                resolveArgs
                                  "map callback"
                                  argSpecs
                                  ((Nothing, x):args')
                    eitherExceptM $
                      runGingerT (setVars args'' >> evalE body) ctx env'
                  _ ->
                    -- Not something we can call.
                      throwError $
                        NonCallableObjectError
                          (tagNameOf callee <> " " <> Text.show filterV)

              apply = apply' callee

          ListV <$> mapM apply xs
  where
    toIdentifier :: Scalar -> Maybe Identifier
    toIdentifier (StringScalar s) = Just $ Identifier s
    toIdentifier (IntScalar i) = Just $ Identifier (Text.show i)
    toIdentifier (FloatScalar f) = Just $ Identifier (Text.show f) -- dubious
    toIdentifier _ = Nothing

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
    x -> throwError $ ArgumentError "round" "method" "one of 'common', 'floor', 'ceil'" x
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

fnToJSON :: forall m. Monad m => Procedure m
fnToJSON = mkFn2 "tojson"
              ("value", Nothing :: Maybe (Value m))
              ("indent", Just (Nothing :: Maybe Int))
  $ \value _indentMay ->
    pure . Text.decodeUtf8 . LBS.toStrict $ JSON.encode value

fnJoin :: forall m. Monad m => Procedure m
fnJoin = mkFn3 "join"
                ("iterable", Nothing :: Maybe [Value m])
                ("d", Just "" :: Maybe Text)
                ("attr", Just Nothing :: Maybe (Maybe Text))
                $ \iterable d attrMay -> do
  iterable' <- case attrMay of
    Nothing -> pure iterable
    Just attr ->
      catMaybes <$>
        mapM
          (\x -> eitherExceptM $ getAttrOrItemRaw x (Identifier attr))
          iterable
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
                    "split"
                    "sep"
                    "non-empty string"
                    "empty string"
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
    _ -> throwError $ ArgumentError "encode" "encoding" "valid encoding" encoding
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

newtype FileSize = FileSize Integer

instance ToValue FileSize m where
  toValue (FileSize i) = IntV i

instance Monad m => FromValue FileSize m where
  fromValue (IntV i) =
    pure . Right $ FileSize i
  fromValue (FloatV f) =
    pure . Right $ FileSize (round f)
  fromValue (StringV txt) =
    case readMaybe (Text.unpack txt) of
      Nothing ->
        case readMaybe (Text.unpack txt) of
          Nothing ->
            pure . Left $ ArgumentError "int" "value" "numeric value" (Text.show txt)
          Just (f :: Double) ->
            pure . Right $ FileSize (round f)
      Just i ->
        pure . Right $ FileSize i
  fromValue x =
    pure . Left $ ArgumentError "int" "value" "numeric value" (tagNameOf x)

fnFilesizeFormat :: Monad m => Procedure m
fnFilesizeFormat = mkFn2 "filesizeformat"
                    ("value", Nothing)
                    ("binary", Just False)
  $ \(FileSize value) binary -> do
      let (multiplier, units) =
            if binary then
              (1024, ["ki", "Mi", "Gi", "Ti", "Pi"])
            else
              (1000, ["k", "M", "G", "T", "P"])
      let str = go multiplier units value
      pure $ str <> "B"
  where
    go :: Integer -> [Text] -> Integer -> Text
    go _ [] value
      = Text.show value
    go multiplier units value | value < 0
      = "-" <> go multiplier units (abs value)
    go multiplier (unit:units) value
      | value < multiplier
      = Text.show value
      | value < multiplier * multiplier || null units
      = Text.pack $
          printf "%i.%01i%s"
            (value `div` multiplier)
            ((value * 10 `div` multiplier) `mod` 10)
            unit
      | otherwise
      = go multiplier units (value `div` multiplier)

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
    x -> throwError $ ArgumentError "first" "value" "list or string" (tagNameOf x)
      
fnLast :: forall m. Monad m => Procedure m
fnLast = mkFn1 "first"
            ("value", Nothing :: Maybe (Value m))
  $ \case
    ListV [] -> pure NoneV
    ListV xs -> pure (last xs)
    StringV txt -> pure $ StringV $ Text.takeEnd 1 txt
    BytesV arr -> pure . toValue $ BS.indexMaybe arr (BS.length arr - 1)
    EncodedV (Encoded txt) -> pure $ EncodedV . Encoded $ Text.takeEnd 1 txt
    x -> throwError $ ArgumentError "first" "value" "list or string" (tagNameOf x)

autoParseDate :: TimeZone -> Text -> Maybe ZonedTime
autoParseDate defTZ input =
  asum [ parse t (Text.unpack input) | (parse, t) <- formats ]
  where
    ztparse :: String -> String -> Maybe ZonedTime
    ztparse fmt = parseTimeM True defaultTimeLocale fmt
    utcparse :: String -> String -> Maybe ZonedTime
    utcparse fmt i = do
        lt <- parseTimeM True defaultTimeLocale fmt i
        return $ ZonedTime lt defTZ
    formats =
        [ (ztparse, "%Y-%m-%dT%H:%M:%S%Q%Z")
        , (utcparse, "%Y-%m-%d %H:%M:%S%Q")
        , (ztparse, "%Y-%m-%d %H:%M:%S%Q%z")
        , (ztparse, "%Y-%m-%d %H:%M:%S%Q%Z")
        , (utcparse, "%Y-%m-%d")
        ]

dateFromParts :: forall m. Monad m
              => TimeZone
              -> [Value m]
              -> Maybe ZonedTime
dateFromParts defTZ parts = do
  year <- case parts of
            (IntV y : _) -> Just y
            (StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _) -> Nothing
            _ -> Just 2000
  month <- case parts of
            (_ : IntV m : _) -> Just m
            (_ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _) -> Nothing
            _ -> Just 1
  day <- case parts of
            (_ : _ : IntV d : _) -> Just d
            (_ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _) -> Nothing
            _ -> Just 1
  hour <- case parts of
            (_ : _ : _ : IntV h : _) -> Just h
            (_ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  minute <- case parts of
            (_ : _ : _ : _ : IntV m : _) -> Just m
            (_ : _ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  second <- case parts of
            (_ : _ : _ : _ : _ : IntV s : _) -> Just s
            (_ : _ : _ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  tz <- case parts of
            (_ : _ : _ : _ : _ : _ : v : _) -> parseTZ v
            _ -> Just defTZ
  pure $ ZonedTime
          (LocalTime
            (fromGregorian year (fromInteger month) (fromInteger day))
            (TimeOfDay (fromInteger hour) (fromInteger minute) (fromInteger second)))
          tz

parseTZ :: Value m -> Maybe TimeZone
parseTZ (StringV s) =
  parseTimeM True defaultTimeLocale "%z" $ Text.unpack s
parseTZ (IntV i) =
  Just $ TimeZone (fromInteger i) False ""
parseTZ (ListV [IntV minutes, BoolV summerOnly, StringV name]) =
  Just $ TimeZone (fromInteger minutes) summerOnly (Text.unpack name)
parseTZ _ = Nothing

convertTZ :: Maybe TimeZone -> ZonedTime -> ZonedTime
convertTZ Nothing = id
convertTZ (Just tz) = utcToZonedTime tz . zonedTimeToUTC

fnDateFormat :: forall m. Monad m => Procedure m
fnDateFormat = mkFn4 "dateformat"
                ("date", Nothing :: Maybe (Either Text [Value m]))
                ("format", Just "%c")
                ("tz", Just Nothing :: Maybe (Maybe (Value m)))
                ("locale", Just Nothing :: Maybe (Maybe Text))
    $ \dateRaw fmt tzVal _localeMay -> do
      let tzMay = parseTZ =<< tzVal
          defTZ = fromMaybe utc tzMay
          locale = defaultTimeLocale -- TODO: use getlocale
      date <- maybe
                (throwError $
                  ArgumentError
                    "dateformat"
                    "date"
                    "date string or date array"
                    (Text.show dateRaw)
                )
                pure $
                case dateRaw of
                  Left str -> autoParseDate defTZ str
                  Right parts -> dateFromParts defTZ parts
      pure . Text.pack . formatTime locale (Text.unpack fmt) . convertTZ tzMay $ date

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

getAttrOrItemRaw :: Monad m
                 => Value m
                 -> Identifier
                 -> m (Either RuntimeError (Maybe (Value m)))
getAttrOrItemRaw a i = runExceptT $ do
  xMay <- eitherExceptM $ getAttrRaw a i
  case xMay of
    Just x -> pure . Just $ x
    Nothing -> lift $ getItemRaw a (StringV . identifierName $ i)

getItemOrAttrRaw :: Monad m
                 => Value m
                 -> Value m
                 -> m (Either RuntimeError (Maybe (Value m)))
getItemOrAttrRaw a b = runExceptT $ do
  xMay <- lift $ getItemRaw a b
  case xMay of
    Just x -> pure . Just $ x
    Nothing -> case b of
      StringV i -> eitherExceptM $ getAttrRaw a (Identifier $ i)
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
  pure . Left $ NotImplementedError name

fnMaybeArg :: Monad m => Text -> Text -> Maybe b -> ExceptT RuntimeError m b
fnMaybeArg context name =
  maybe
    (throwError $
        ArgumentError
          context
          name
          "argument"
          "end of arguments"
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
        funcName
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
        funcName
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
        funcName
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
        funcName
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
        funcName
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
