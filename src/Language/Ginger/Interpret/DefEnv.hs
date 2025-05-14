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
{-# LANGUAGE LambdaCase #-}

module Language.Ginger.Interpret.DefEnv
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value
import Language.Ginger.Interpret.Type
import Language.Ginger.Interpret.Eval
import Language.Ginger.Interpret.Builtins

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)

defEnv :: Monad m => Env m
defEnv =
  emptyEnv
    { envVars = defEnvVars
    }

defEnvVars :: forall m. Monad m => Map Identifier (Value m)
defEnvVars = Map.fromList
  [ ( "jinja-tests"
    , dictV
        [ ("defined", TestV $ NativeTest isDefined)
        , ("undefined", TestV $ NativeTest isUndefined)
        , ("boolean", toValue (isBool @m))
        , ("callable", toValue (isCallable @m))
        , ("filter", TestV $ NativeTest isFilter)
        , ("float", toValue (isFloat @m))
        , ("integer", toValue (isInteger @m))
        , ("iterable", toValue (isIterable @m))
        , ("mapping", toValue (isMapping @m))
        , ("number", toValue (isNumber @m))
        , ("sequence", toValue (isSequence @m))
        , ("string", toValue (isString @m))
        , ("test", TestV $ NativeTest isTest)
        , ("upper", toValue (isUpperVal @m))
        , ("eq", TestV $ NativeTest isEqual)
        , ("escaped", builtinNotImplemented @m "escaped")
        , ("false", toValue (isBoolean False :: Value m -> Value m))
        , ("ge", gingerBinopTest BinopGTE)
        , ("gt", gingerBinopTest BinopGT)
        , ("in", gingerBinopTest BinopIn)
        , ("le", gingerBinopTest BinopLTE)
        , ("lower", toValue (isLowerVal @m))
        , ("lt", gingerBinopTest BinopLT)
        , ("sameas", builtinNotImplemented @m "sameas")
        , ("true", toValue (isBoolean True :: Value m -> Value m))
        , ("none", toValue (isNone :: Value m -> Value m))
        ]
    )
  , ( "jinja-filters"
    , dictV
        [ ("default", FilterV $ NativeFilter defaultFilter)
        ]
    )
  ]
  <> builtinFunctions

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
      exists <-
        pure . isJust $ Map.lookup (Identifier name) (envVars env)
      existsExt <-
        runGingerT
          (asBool "" =<< eval (InE (StringLitE name) (VarE "jinja-filters")))
          ctx env
      pure $ (exists ||) <$> existsExt
    Right a ->
      pure . Left $ TagError (Just "filter name") (Just "string") (Just . tagNameOf $ a)
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
      let testsVars = case Map.lookup "jinja-tests" (envVars env) of
            Just (DictV xs) -> xs
            _ -> mempty
      let vars = Map.mapKeys (toScalar . identifierName) (envVars env) <> testsVars
      let existing = Map.lookup (toScalar name) vars
      case existing of
        Just a -> pure . Right $ isCallable' a
        _ -> pure . Right $ False
        
    Right a ->
      pure . Left $ TagError (Just "test name") (Just "string") (Just . tagNameOf $ a)
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
                            (Just "default")
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
isDefined _ (_:_) _ _ = pure $ Left $ ArgumentError (Just "defined") (Just "0") (Just "end of arguments") (Just "argument")
isDefined (VarE name) [] _ env =
  pure . Right $ name `Map.member` (envVars env)
isDefined NoneE [] _ _ = pure . Right $ True
isDefined BoolE {} [] _ _ = pure . Right $ True
isDefined StringLitE {} [] _ _ = pure . Right $ True
isDefined IntLitE {} [] _ _ = pure . Right $ True
isDefined FloatLitE {} [] _ _ = pure . Right $ True
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
isDefined (TernaryE c a b) [] ctx env = do
  definedA <- isDefined a [] ctx env
  definedB <- isDefined b [] ctx env
  definedC <- isDefined c [] ctx env
  pure $ allEitherBool [definedA, definedB, definedC]
isDefined (ListE []) [] _ _ = pure . Right $ True
isDefined (ListE (x:xs)) [] ctx env = do
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
      setVar "#args" (ListV $ map snd args)
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
          (Just context)
          (Just name)
          (Just "argument")
          (Just "end of arguments")
    )
    pure
