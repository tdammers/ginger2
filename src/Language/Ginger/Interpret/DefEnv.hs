{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Language.Ginger.Interpret.DefEnv
where

import Language.Ginger.AST
import Language.Ginger.Interpret.Builtins
import Language.Ginger.Interpret.Eval
import Language.Ginger.Interpret.Type
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as V

defEnv :: Monad m => Env m
defEnv =
  emptyEnv
    { envVars = mempty
    }

defContext :: Monad m => Context m
defContext =
  emptyContext
    { contextVars = defVars
    , contextEncode = pure . htmlEncode
    }

htmlEncoder :: Monad m => Encoder m
htmlEncoder txt = do
  pure $ htmlEncode txt

htmlEncode :: Text -> Encoded
htmlEncode txt =
  (Encoded . LText.toStrict . Builder.toLazyText . Text.foldl' f mempty) txt
  where
    f :: Builder -> Char -> Builder
    f lhs c = lhs <> encodeChar c

    encodeChar :: Char -> Builder
    encodeChar '&' = "&amp;"
    encodeChar '<' = "&lt;"
    encodeChar '>' = "&gt;"
    encodeChar '"' = "&quot;"
    encodeChar '\'' = "&apos;"
    encodeChar c = Builder.singleton c

defVarsCommon :: forall m. Monad m
              => Map Identifier (Value m)
defVarsCommon = Map.fromList
  [ ( "__jinja__"
    , dictV
      [ ( "tests"
        , dictV
            [ ("defined", TestV $ NativeTest isDefined)
            , ("undefined", TestV $ NativeTest isUndefined)
            , ("boolean", fnToValue
                            "builtin:test:boolean"
                            (Just ProcedureDoc
                              { procedureDocName = "boolean"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a boolean."
                              }
                            )
                            (isBool @m))
            , ("callable", fnToValue
                            "builtin:test:callable"
                            (Just ProcedureDoc
                              { procedureDocName = "callable"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is callable."
                              }
                            )
                            (isCallable @m))
            , ("filter", TestV $ NativeTest isFilter)
            , ("float", fnToValue
                            "builtin:test:float"
                            (Just ProcedureDoc
                              { procedureDocName = "float"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a float."
                              }
                            )
                            (isFloat @m))
            , ("integer", fnToValue
                            "builtin:test:integer"
                            (Just ProcedureDoc
                              { procedureDocName = "integer"
                              , procedureDocArgs = mempty
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is an integer."
                              }
                            )
                            (isInteger @m))
            , ("iterable", fnToValue
                            "builtin:test:iterable"
                            (Just ProcedureDoc
                              { procedureDocName = "iterable"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is iterable.\n"
                                  <> "Lists and list-like native objects are iterable."
                              }
                            )
                            (isIterable @m))
            , ("mapping", fnToValue
                            "builtin:test:mapping"
                            (Just ProcedureDoc
                              { procedureDocName = "mapping"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a mapping.\n"
                                  <> "Mappings are dicts and dict-like native objects."
                              }
                            )
                            (isMapping @m))
            , ("number", fnToValue
                            "builtin:test:number"
                            (Just ProcedureDoc
                              { procedureDocName = "number"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a number (integer or float)."
                              }
                            )
                            (isNumber @m))
            , ("sequence", fnToValue
                            "builtin:test:sequence"
                            (Just ProcedureDoc
                              { procedureDocName = "sequence"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a sequence (i.e., a list)."
                              }
                            )
                            (isSequence @m))
            , ("string", fnToValue
                            "builtin:test:string"
                            (Just ProcedureDoc
                              { procedureDocName = "string"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is a string."
                              }
                            )
                            (isString @m))
            , ("test", TestV $ NativeTest isTest)
            , ("upper", fnToValue
                            "builtin:test:upper"
                            (Just ProcedureDoc
                              { procedureDocName = "upper"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is an all-uppercase string."
                              }
                            )
                            (isUpperVal @m))
            , ("eq", TestV $ NativeTest isEqual)
            , ("escaped", builtinNotImplemented @m "escaped")
            , ("false", fnToValue
                            "builtin:test:false"
                            (Just ProcedureDoc
                              { procedureDocName = "false"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is boolean `false`"
                              }
                            )
                            (isBoolean False :: Value m -> Value m))
            , ("ge", gingerBinopTest BinopGTE)
            , ("gt", gingerBinopTest BinopGT)
            , ("in", gingerBinopTest BinopIn)
            , ("le", gingerBinopTest BinopLTE)
            , ("lower", fnToValue
                            "builtin:test:lower"
                            (Just ProcedureDoc
                              { procedureDocName = "lower"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is an all-lowercase string"
                              }
                            )
                            (isLowerVal @m))
            , ("lt", gingerBinopTest BinopLT)
            , ("sameas", builtinNotImplemented @m "sameas")
            , ("true", fnToValue
                            "builtin:test:true"
                            (Just ProcedureDoc
                              { procedureDocName = "true"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is boolean `true`"
                              }
                            )
                            (isBoolean True :: Value m -> Value m))
            , ("none", fnToValue
                            "builtin:test:none"
                            (Just ProcedureDoc
                              { procedureDocName = "none"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether value is the `none` value"
                              }
                            )
                            (isNone :: Value m -> Value m))
            ]
        )
      , ( "filters"
        , dictV
            [ ("default", FilterV $ NativeFilter defaultFilter)
            , ("d", FilterV $ NativeFilter defaultFilter)
            ]
        )
      , ( "globals"
        , DictV . Map.mapKeys toScalar $ builtinGlobals evalE
        )
      ]
    )
  ]
  <> builtinGlobals evalE

defVarsCompat :: forall m. Monad m
              => Map Identifier (Value m)
defVarsCompat = defVarsCommon

defVars :: forall m. Monad m
        => Map Identifier (Value m)
defVars = defVarsCommon
        <> builtinGlobalsNonJinja evalE
        <> Map.fromList
           [ ( "__ginger__"
             , dictV
               [ ( "globals"
                 , DictV . Map.mapKeys toScalar $ builtinGlobalsNonJinja evalE
                 )
               ]
             )
           ]

isCallable' :: Monad m => Value m -> Bool
isCallable' (ProcedureV {}) = True
isCallable' (NativeV n) =
  isJust (nativeObjectCall n)
isCallable' (DictV d) =
  maybe (False) isCallable' (Map.lookup "__call__" d)
isCallable' _ = False

isCallable :: Monad m => Value m -> Value m
isCallable = BoolV . isCallable'

isFilter :: Monad m => TestFunc m
isFilter expr _ ctx env = do
  result <- runGingerT (evalE expr) ctx env
  case result of
    Right (StringV name) -> do
      let exists =
            isJust (Map.lookup (Identifier name) (envVars env)) ||
            isJust (Map.lookup (Identifier name) (contextVars ctx))
      existsExt <-
        runGingerT
          (asBool ""
              =<< eval
                  (InE (StringLitE name) (DotE (VarE "__jinja__") "filters")))
          ctx env
      pure $ (exists ||) <$> existsExt
    Right a ->
      pure . Left $ TagError "filter name" "string" (tagNameOf a)
    Left err ->
      pure $ Left err

isMapping :: Monad m => Value m -> Value m
isMapping (NativeV {}) = TrueV
isMapping (DictV {}) = TrueV
isMapping _ = FalseV

isIterable :: Monad m => Value m -> Value m
isIterable (NativeV {}) = TrueV
isIterable (DictV {}) = TrueV
isIterable (ListV {}) = TrueV
isIterable _ = FalseV

isSequence :: Monad m => Value m -> Value m
isSequence (NativeV {}) = TrueV
isSequence (ListV {}) = TrueV
isSequence _ = FalseV

isTest :: Monad m => TestFunc m
isTest expr _ ctx env = do
  result <- runGingerT (evalE expr) ctx env
  case result of
    Right NoneV -> pure . Right $ True
    Right BoolV {} -> pure . Right $ True
    Right (StringV name) -> do
      let testsVars = case Map.lookup "__jinja__" (contextVars ctx) of
            Just (DictV xs) ->
              case Map.lookup "tests" xs of
                Just (DictV ts) -> ts
                _ -> mempty
            _ -> mempty
      let vars =
            Map.mapKeys (toScalar . identifierName) (contextVars ctx) <>
            Map.mapKeys (toScalar . identifierName) (envVars env) <>
            testsVars
      let existing = Map.lookup (toScalar name) vars
      case existing of
        Just a -> pure . Right $ isCallable' a
        _ -> pure . Right $ False
        
    Right a ->
      pure . Left $ TagError "test name" "string" (tagNameOf a)
    Left err ->
      pure $ Left err

isEscaped :: Monad m => Value m -> Value m
isEscaped (EncodedV {}) = TrueV
isEscaped _ = FalseV

isBool :: Monad m => Value m -> Value m
isBool (BoolV {}) = TrueV
isBool _ = FalseV

isInteger :: Monad m => Value m -> Value m
isInteger (IntV {}) = TrueV
isInteger _ = FalseV

isFloat :: Monad m => Value m -> Value m
isFloat (FloatV {}) = TrueV
isFloat _ = FalseV

isNumber :: Monad m => Value m -> Value m
isNumber (IntV {}) = TrueV
isNumber (FloatV {}) = TrueV
isNumber _ = FalseV

isString :: Monad m => Value m -> Value m
isString (StringV {}) = TrueV
isString _ = FalseV

defaultFilter :: Monad m => FilterFunc m
defaultFilter expr args ctx env = do
  calleeEither <- runGingerT (evalE expr) ctx env
  let resolvedArgsEither = resolveArgs
                            "default"
                            [("default_value", Just (StringV "")), ("boolean", Just FalseV)]
                            args
  case (calleeEither, resolvedArgsEither) of
    (_, Left err) ->
      pure $ Left err
    (Right val, Right rargs) ->
      let defval = fromJust $ Map.lookup "default_value" rargs
          boolean = fromJust $ Map.lookup "boolean" rargs
      in case val of
        NoneV -> pure . Right $ defval
        FalseV -> pure . Right $ if boolean == TrueV then defval else FalseV
        a -> pure . Right $ a
    (Left NotInScopeError {}, Right rargs) ->
      pure . Right . fromJust $ Map.lookup "default_value" rargs
    (Left err, _) ->
      pure . Left $ err

isDefined :: Monad m => TestFunc m
isDefined _ (_:_) _ _ = pure $ Left $ ArgumentError "defined" "0" "end of arguments" "argument"
isDefined (PositionedE _ e) [] ctx env =
  isDefined e [] ctx env
isDefined (VarE name) [] ctx env =
  pure . Right $
    name `Map.member` (envVars env) ||
    name `Map.member` (contextVars ctx)
isDefined NoneE [] _ _ = pure . Right $ True
isDefined BoolE {} [] _ _ = pure . Right $ True
isDefined StringLitE {} [] _ _ = pure . Right $ True
isDefined IntLitE {} [] _ _ = pure . Right $ True
isDefined FloatLitE {} [] _ _ = pure . Right $ True
isDefined (SliceE slicee startMay endMay) [] ctx env = do
  definedSlicee <- isDefined slicee [] ctx env
  definedStart <- maybe (pure . Right $ True) (\start -> isDefined start [] ctx env) startMay
  definedEnd <- maybe (pure . Right $ True) (\end -> isDefined end [] ctx env) endMay
  pure $ allEitherBool [ definedSlicee, definedStart, definedEnd ]
isDefined (IndexE parent selector) [] ctx env = do
  definedParent <- isDefined parent [] ctx env
  case definedParent of
    Right True -> do
      result <- runGingerT (evalE (InE selector parent)) ctx env
      case result of
        Left (NotInScopeError {}) -> pure . Right $ False
        Left err -> pure . Left $ err
        Right (BoolV b) -> pure . Right $ b
        Right _ -> pure . Left $ FatalError "Evaluating an 'in' expression produced non-boolean result"
    x -> pure x
isDefined (UnaryE _ a) [] ctx env =
  isDefined a [] ctx env
isDefined (BinaryE _ a b) [] ctx env = do
  definedA <- isDefined a [] ctx env
  definedB <- isDefined b [] ctx env
  pure $ (&&) <$> definedA <*> definedB
isDefined (DotE a _b) [] ctx env = do
  isDefined a [] ctx env
isDefined (TernaryE c a b) [] ctx env = do
  definedA <- isDefined a [] ctx env
  definedB <- isDefined b [] ctx env
  definedC <- isDefined c [] ctx env
  pure $ allEitherBool [definedA, definedB, definedC]
isDefined (ListE v) [] ctx env =
  case V.uncons v of
    Nothing -> pure . Right $ True
    Just (x, xs) -> do
      definedX <- isDefined x [] ctx env
      definedXS <- isDefined (ListE xs) [] ctx env
      pure $ allEitherBool [definedX, definedXS]
isDefined (DictE []) [] _ _ = pure . Right $ True
isDefined (DictE ((k, v):xs)) [] ctx env = do
  definedK <- isDefined k [] ctx env
  definedV <- isDefined v [] ctx env
  definedXS <- isDefined (DictE xs) [] ctx env
  pure $ allEitherBool [definedK, definedV, definedXS]
isDefined (IsE {}) [] _ _ = pure . Right $ True
isDefined (StatementE {}) [] _ _ = pure . Right $ True
isDefined (FilterE posArg0 callee posArgs kwArgs) [] ctx env = do
  definedPosArg0 <- isDefined posArg0 [] ctx env
  definedCallee <- isDefined callee [] ctx env
  definedPosArgs <- allEitherBool <$> mapM (\x -> isDefined x [] ctx env) posArgs
  definedKWArgs <- allEitherBool <$> mapM (\(_, x) -> isDefined x [] ctx env) kwArgs
  pure $ allEitherBool [definedPosArg0, definedCallee, definedPosArgs, definedKWArgs]
isDefined (CallE callee posArgs kwArgs) [] ctx env = do
  definedCallee <- isDefined callee [] ctx env
  definedPosArgs <- allEitherBool <$> mapM (\x -> isDefined x [] ctx env) posArgs
  definedKWArgs <- allEitherBool <$> mapM (\(_, x) -> isDefined x [] ctx env) kwArgs
  pure $ allEitherBool [definedCallee, definedPosArgs, definedKWArgs]

isUndefined :: Monad m => TestFunc m
isUndefined expr args ctx env = do
  defined <- isDefined expr args ctx env
  pure $ not <$> defined

isEqual :: Monad m => TestFunc m
isEqual expr args ctx env = runGingerT go ctx env
  where
    go = do
      definedLHS <- native $ isDefined expr args ctx env
      if definedLHS then do
        val <- eval expr
        equals <- mapM (valuesEqual val . snd) args
        pure $ all id equals
      else
        pure False

gingerBinopTest :: forall m. Monad m
                => BinaryOperator
                -> Value m
gingerBinopTest op =
  TestV . NativeTest $ f
  where
    f :: TestFunc m
    f expr args ctx env = runGingerT (go expr args) ctx env

    go expr args = scoped $ do
      setVar "#args" (ListV . V.fromList $ map snd args)
      eval (BinaryE op expr (VarE "#args")) >>= \case
        TrueV -> pure True
        _ -> pure False

fnEither :: Monad m => Either a b -> ExceptT a m b
fnEither = either throwError pure

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
