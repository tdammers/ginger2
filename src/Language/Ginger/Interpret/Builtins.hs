{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.Interpret.Builtins
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value
import Language.Ginger.Interpret.Type
import Language.Ginger.Interpret.Eval

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

defEnv :: Monad m => Env m
defEnv =
  emptyEnv
    { envVars = defEnvVars
    }

defEnvVars :: forall m. Monad m => Map Identifier (Value m)
defEnvVars = Map.fromList
  [ ("defined", TestV $ NativeTest isDefined)
  , ("undefined", TestV $ NativeTest isUndefined)
  , ("callable", toValue (isCallable @m))
  , ("boolean", toValue (isBool @m))
  , ("integer", toValue (isInteger @m))
  , ("float", toValue (isFloat @m))
  , ("number", toValue (isNumber @m))
  , ("string", toValue (isString @m))
  , ("test", toValue (isTest @m))
  , ("filter", toValue (isCallable @m))
  , ("iterable", toValue (isIterable @m))
  , ("mapping", toValue (isMapping @m))
  , ("sequence", toValue (isSequence @m))
  ]

isCallable :: Monad m => Value m -> Value m
isCallable (ProcedureV {}) = TrueV
isCallable (NativeV n) =
  BoolV $ isJust (nativeObjectCall n)
isCallable (DictV d) =
  maybe (FalseV) isCallable (Map.lookup "__call__" d)
isCallable _ = FalseV

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

isTest :: Monad m => Value m -> Value m
isTest (TestV {}) = TrueV
isTest x = isCallable x

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
isDefined (CallE callee posArgs kwArgs) [] ctx env = do
  definedCallee <- isDefined callee [] ctx env
  definedPosArgs <- allEitherBool <$> mapM (\x -> isDefined x [] ctx env) posArgs
  definedKWArgs <- allEitherBool <$> mapM (\(_, x) -> isDefined x [] ctx env) kwArgs
  pure $ allEitherBool [definedCallee, definedPosArgs, definedKWArgs]

isUndefined :: Monad m => TestFunc m
isUndefined expr args ctx env = do
  defined <- isDefined expr args ctx env
  pure $ not <$> defined

allEitherBool :: [(Either a Bool)] -> Either a Bool
allEitherBool [] = Right True
allEitherBool (Right True : xs) = allEitherBool xs
allEitherBool (x : _) = x


