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
import Language.Ginger.Render
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad.Except
import Control.Monad.Random (MonadRandom)
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

defContext :: MonadRandom m => Context m
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

defVarsCommon :: forall m. MonadRandom m
              => Map Identifier (Value m)
defVarsCommon = Map.fromList
  [ ( "__jinja__"
    , dictV
      [ ( "tests"
        , DictV . Map.mapKeys toScalar $ builtinTests
        )
      , ( "filters"
        , DictV . Map.mapKeys toScalar $ builtinFilters
        )
      , ( "globals"
        , DictV . Map.mapKeys toScalar $ builtinGlobals evalE
        )
      ]
    )
  ]
  <> builtinGlobals evalE

defVarsCompat :: forall m. MonadRandom m
              => Map Identifier (Value m)
defVarsCompat = defVarsCommon

defVars :: forall m. MonadRandom m
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

builtinFilters :: forall m. MonadRandom m
             => Map Identifier (Value m)
builtinFilters = Map.fromList
            [ ("default", FilterV $ defaultFilter)
            , ("d", FilterV $ defaultFilter)
            ]

builtinTests :: forall m. MonadRandom m
             => Map Identifier (Value m)
builtinTests = Map.fromList
            [ ("defined", TestV $
                            NativeTest
                              (Just ProcedureDoc
                                { procedureDocName = "defined"
                                , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                                , procedureDocReturnType = Just $ TypeDocSingle "bool"
                                , procedureDocDescription =
                                    "Test whether a variable is defined."
                                }
                              )
                              isDefined)
            , ("undefined", TestV $
                              NativeTest
                                (Just ProcedureDoc
                                  { procedureDocName = "defined"
                                  , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                                  , procedureDocReturnType = Just $ TypeDocSingle "bool"
                                  , procedureDocDescription =
                                      "Test whether a variable is undefined."
                                  }
                                )
                                isUndefined)
            , ("boolean", fnToValue
                            "builtin:test:boolean"
                            (Just ProcedureDoc
                              { procedureDocName = "boolean"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is a boolean."
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
                                  "Test whether `value` is callable."
                              }
                            )
                            (isCallable @m))
            , ("filter", TestV $
                          NativeTest
                          (Just ProcedureDoc
                              { procedureDocName = "filter"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is a filter."
                              }
                          )
                          isFilter)
            , ("float", fnToValue
                            "builtin:test:float"
                            (Just ProcedureDoc
                              { procedureDocName = "float"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is a float."
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
                                  "Test whether `value` is an integer."
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
                                  "Test whether `value` is iterable.\n"
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
                                  "Test whether `value` is a mapping.\n"
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
                                  "Test whether `value` is a number (integer or float)."
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
                                  "Test whether `value` is a sequence (i.e., a list)."
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
                                  "Test whether `value` is a string."
                              }
                            )
                            (isString @m))
            , ("test", TestV $
                          NativeTest
                          (Just ProcedureDoc
                              { procedureDocName = "test"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is a test."
                              }
                          )
                          isTest)
            , ("upper", fnToValue
                            "builtin:test:upper"
                            (Just ProcedureDoc
                              { procedureDocName = "upper"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is an all-uppercase string."
                              }
                            )
                            (isUpperVal @m))
            , ("eq", TestV $
                          NativeTest
                          (Just ProcedureDoc
                              { procedureDocName = "eq"
                              , procedureDocArgs =
                                  [ ArgumentDoc "value" (Just TypeDocAny) Nothing ""
                                  , ArgumentDoc "other" (Just TypeDocAny) Nothing ""
                                  ]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is equal to `other`."
                              }
                          )
                          isEqual)
            , ("equalto", TestV $
                          NativeTest
                          (Just ProcedureDoc
                              { procedureDocName = "eq"
                              , procedureDocArgs =
                                  [ ArgumentDoc "value" (Just TypeDocAny) Nothing ""
                                  , ArgumentDoc "other" (Just TypeDocAny) Nothing ""
                                  ]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is equal to `other`."
                              }
                          )
                          isEqual)
            , ("escaped", builtinNotImplemented @m "escaped")
            , ("false", fnToValue
                            "builtin:test:false"
                            (Just ProcedureDoc
                              { procedureDocName = "false"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is boolean `false`"
                              }
                            )
                            (isBoolean False :: Value m -> Value m))
            , ("ge", gingerBinopTest BinopGTE)
            , ("gt", gingerBinopTest BinopGT)
            , ("greaterthan", gingerBinopTest BinopGT)
            , ("in", gingerBinopTest BinopIn)
            , ("le", gingerBinopTest BinopLTE)
            , ("lower", fnToValue
                            "builtin:test:lower"
                            (Just ProcedureDoc
                              { procedureDocName = "lower"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is an all-lowercase string"
                              }
                            )
                            (isLowerVal @m))
            , ("lt", gingerBinopTest BinopLT)
            , ("lessthan", gingerBinopTest BinopLT)
            , ("sameas", builtinNotImplemented @m "sameas")
            , ("true", fnToValue
                            "builtin:test:true"
                            (Just ProcedureDoc
                              { procedureDocName = "true"
                              , procedureDocArgs = [ArgumentDoc "value" (Just TypeDocAny) Nothing ""]
                              , procedureDocReturnType = Just $ TypeDocSingle "bool"
                              , procedureDocDescription =
                                  "Test whether `value` is boolean `true`"
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
                                  "Test whether `value` is the `none` value"
                              }
                            )
                            (isNone :: Value m -> Value m))
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

isFilter :: MonadRandom m => TestFunc m
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

isTest :: MonadRandom m => TestFunc m
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

defaultFilter :: MonadRandom m => Filter m
defaultFilter =
  NativeFilter
    (Just $ ProcedureDoc
      { procedureDocName = "default"
      , procedureDocArgs =
          [ ArgumentDoc "value" (Just TypeDocAny) Nothing ""
          , ArgumentDoc "default" (Just TypeDocAny) Nothing ""
          ]
      , procedureDocReturnType = Just $ TypeDocAny
      , procedureDocDescription =
          "Return `default` if `value` is `false`, `none`, or undefined, " <>
          "`value` otherwise."
      }
    ) $
    \expr args ctx env -> do
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

isDefined :: forall m. MonadRandom m => TestFunc m
isDefined _ (_:_) _ _ = pure $ Left $ ArgumentError "defined" "0" "end of arguments" "argument"
isDefined value [] ctx env = go value
  where
    go :: Expr -> m (Either RuntimeError Bool)
    go (PositionedE _ e) =
      go e
    go (VarE name) =
      pure . Right $
        name `Map.member` (envVars env) ||
        name `Map.member` (contextVars ctx)
    go NoneE = pure . Right $ True
    go BoolE {} = pure . Right $ True
    go StringLitE {} = pure . Right $ True
    go IntLitE {} = pure . Right $ True
    go FloatLitE {} = pure . Right $ True
    go (SliceE slicee startMay endMay) = do
      definedSlicee <- go slicee
      definedStart <- maybe (pure . Right $ True) (\start -> go start) startMay
      definedEnd <- maybe (pure . Right $ True) (\end -> go end) endMay
      pure $ allEitherBool [ definedSlicee, definedStart, definedEnd ]
    go (IndexE parent selector) = do
      definedParent <- go parent
      case definedParent of
        Right True -> do
          result <- runGingerT (evalE (InE selector parent)) ctx env
          case result of
            Left (NotInScopeError {}) -> pure . Right $ False
            Left err -> pure . Left $ err
            Right (BoolV b) -> pure . Right $ b
            Right _ -> pure . Left $ FatalError "Evaluating an 'in' expression produced non-boolean result"
        x -> pure x
    go (UnaryE _ a) =
      go a
    go (BinaryE _ a b) = do
      definedA <- go a
      definedB <- go b
      pure $ (&&) <$> definedA <*> definedB
    go (DotE a _b) = do
      go a
    go (TernaryE c a b) = do
      definedA <- go a
      definedB <- go b
      definedC <- go c
      pure $ allEitherBool [definedA, definedB, definedC]
    go (ListE v) =
      case V.uncons v of
        Nothing -> pure . Right $ True
        Just (x, xs) -> do
          definedX <- go x
          definedXS <- go (ListE xs)
          pure $ allEitherBool [definedX, definedXS]
    go (DictE []) = pure . Right $ True
    go (DictE ((k, v):xs)) = do
      definedK <- go k
      definedV <- go v
      definedXS <- go (DictE xs)
      pure $ allEitherBool [definedK, definedV, definedXS]
    go (IsE {}) = pure . Right $ True
    go (StatementE {}) = pure . Right $ True
    go (FilterE posArg0 callee posArgs kwArgs) = do
      definedPosArg0 <- go posArg0
      definedCallee <- go callee
      definedPosArgs <- allEitherBool <$> mapM (\x -> go x) posArgs
      definedKWArgs <- allEitherBool <$> mapM (\(_, x) -> go x) kwArgs
      pure $ allEitherBool [definedPosArg0, definedCallee, definedPosArgs, definedKWArgs]
    go (CallE callee posArgs kwArgs) = do
      definedCallee <- go callee
      definedPosArgs <- allEitherBool <$> mapM (\x -> go x) posArgs
      definedKWArgs <- allEitherBool <$> mapM (\(_, x) -> go x) kwArgs
      pure $ allEitherBool [definedCallee, definedPosArgs, definedKWArgs]

isUndefined :: MonadRandom m => TestFunc m
isUndefined expr args ctx env = do
  defined <- isDefined expr args ctx env
  pure $ not <$> defined

isEqual :: MonadRandom m => TestFunc m
isEqual expr args ctx env =
  runGingerT go ctx env
  where
    go = do
      definedLHS <- native $ isDefined expr args ctx env
      if definedLHS then do
        val <- eval expr
        equals <- mapM (valuesEqual val . snd) args
        pure $ all id equals
      else
        pure False

gingerBinopTest :: forall m. MonadRandom m
                => BinaryOperator
                -> Value m
gingerBinopTest op =
  TestV $ NativeTest
    (Just ProcedureDoc
        { procedureDocName = opName
        , procedureDocArgs =
            [ ArgumentDoc "expr" (Just TypeDocAny) Nothing ""
            , ArgumentDoc "arg" (Just TypeDocAny) Nothing ""
            ]
        , procedureDocReturnType = Just $ TypeDocAny
        , procedureDocDescription =
            "Apply the '" <> opName <> "' operation to the value of `expr` " <>
            "and  the `arg`, and use the result as a boolean condition."
        })
    f
  where
    opName :: Text
    opName = renderSyntaxText op

    f :: TestFunc m
    f expr args ctx env = runGingerT (go expr args) ctx env

    go :: Expr -> [(Maybe Identifier, Value m)] -> GingerT m Bool
    go _ [] = throwError $ ArgumentError opName "2" "any" "end of arguments"
    go expr (arg:_) = scoped $ do
      setVar "#arg" (snd arg)
      eval (BinaryE op expr (VarE "#arg")) >>= \case
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
