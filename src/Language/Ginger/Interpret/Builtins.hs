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

module Language.Ginger.Interpret.Builtins
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad.Trans (lift)
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isUpper, isLower)

type BuiltinAttribs a m = Map Identifier (a -> m (Either RuntimeError (Value m)))

nativeMethod :: Procedure m -> Value m -> Procedure m
nativeMethod (NativeProcedure f) self =
  NativeProcedure $ \args -> f ((Just "value", self) : args)
nativeMethod (GingerProcedure env argSpec body) self =
  GingerProcedure env' (drop 1 argSpec) body
  where
    env' = env { envVars = Map.insert "value" self (envVars env) }

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

builtinStringAttribs :: Monad m => BuiltinAttribs Text m
builtinStringAttribs = Map.fromList
  [ ("center",
        \s -> pure . Right $
          ProcedureV $ nativeMethod fnCenter (StringV s))
  , ("capitalize",
        \s -> pure . Right $
          ProcedureV $ nativeMethod (pureNativeFunc $ textFunc (pure . Text.toUpper)) (StringV s))
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

fnEitherM :: Monad m => m (Either a b) -> ExceptT a m b
fnEitherM = (>>= fnEither) . lift

fnTextArg :: Monad m
          => Text
          -> Identifier
          -> Map Identifier (Value m)
          -> ExceptT RuntimeError m Text
fnTextArg = fnArg asTextVal

fnIntArg :: Monad m
          => Text
          -> Identifier
          -> Map Identifier (Value m)
          -> ExceptT RuntimeError m Integer
fnIntArg = fnArg asIntVal

fnListArg :: Monad m
          => Text
          -> Identifier
          -> Map Identifier (Value m)
          -> ExceptT RuntimeError m [Value m]
fnListArg = fnArgM asListVal

fnArg :: Monad m
      => (Value m -> Either RuntimeError a)
      -> Text
      -> Identifier
      -> Map Identifier (Value m)
      -> ExceptT RuntimeError m a
fnArg convert context name argValues = do
  argV <- fnMaybeArg context (identifierName name) $ Map.lookup name argValues
  fnEither $ convert argV

fnArgM :: Monad m
       => (Value m -> m (Either RuntimeError a))
       -> Text
       -> Identifier
       -> Map Identifier (Value m)
       -> ExceptT RuntimeError m a
fnArgM convert context name argValues = do
  argV <- fnMaybeArg context (identifierName name) $ Map.lookup name argValues
  fnEitherM $ convert argV


fnCenter :: Monad m => Procedure m
fnCenter = NativeProcedure $ \args -> runExceptT $ do
    argValues <- fnEither $
                  resolveArgs
                    (Just "center")
                    [("value", Nothing), ("width", Just $ IntV 80)]
                    args
    value <- fnTextArg "center" "value" argValues
    width <- fnIntArg "center" "width" argValues
    let paddingTotal = max 0 $ fromInteger width - Text.length value
        paddingLeft = paddingTotal `div` 2
        paddingRight = paddingTotal - paddingLeft
    pure . StringV $
      Text.replicate paddingLeft " " <>
      value <>
      Text.replicate paddingRight " "

fnBatch :: Monad m => Procedure m
fnBatch = NativeProcedure $ \args -> runExceptT $ do
  argValues <- fnEither $
                resolveArgs
                  (Just "batch")
                  [ ("value", Nothing)
                  , ("linecount", Nothing)
                  , ("fill_with", Just NoneV)
                  ]
                  args
  value <- fnListArg "batch" "value" argValues
  linecount <- fnIntArg "batch" "linecount" argValues
  fillWith <- fnArg Right "batch" "fill_with" argValues
  let fillWithMay = if fillWith == NoneV then Nothing else Just fillWith
  pure . ListV . map ListV $ chunksOf fillWithMay (fromInteger linecount) value
  where
    chunksOf :: Maybe a -> Int -> [a] -> [[a]]
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
