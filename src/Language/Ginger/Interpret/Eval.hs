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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ginger.Interpret.Eval
( Eval (..)
, EvalState (..)
, evalE
, evalS
, evalSs
, evalT
, stringify
, valuesEqual
, asBool
, asTruth
, getAttr
, getAttrRaw
, getItem
, getItemRaw
, loadTemplate
, splitRNG
)
where

import Language.Ginger.AST
import Language.Ginger.Interpret.Builtins
import Language.Ginger.Interpret.Type
import Language.Ginger.Parse (parseGinger)
import qualified Language.Ginger.Parse as Parse
import Language.Ginger.RuntimeError
import Language.Ginger.SourcePosition
import Language.Ginger.StringFormatting
import Language.Ginger.Value

import Control.Monad (foldM, forM, void)
import Control.Monad.Except
  ( MonadError (..)
  , throwError
  )
import Control.Monad.Reader (ask , asks, local, MonadReader (..))
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (lift, MonadTrans (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified System.Random as R

hashShow :: Show a => a -> Text
hashShow = Text.pack . showDigest . sha256 . LBS.fromStrict . encodeUtf8 . Text.show

loadTemplate :: Monad m => Text -> GingerT m LoadedTemplate
loadTemplate name = do
  sMay <- loadTemplateMaybe name
  case sMay of
    Nothing -> throwError $ TemplateFileNotFoundError name
    Just s -> pure s

loadTemplateMaybe :: Monad m => Text -> GingerT m (Maybe LoadedTemplate)
loadTemplateMaybe name = do
  loader <- asks contextLoadTemplateFile
  srcMay <- lift (loader name)
  case srcMay of
    Nothing -> pure Nothing
    Just src -> do
      let result = parseGinger Parse.template (Text.unpack name) src
      case result of
        Left err ->
          throwError $ TemplateParseError name (Text.pack err)
        Right t -> do
          parent <- forM (templateParent t) loadTemplate
          let body = templateBody t
          pure . Just $ LoadedTemplate parent body

mapArgs :: forall m. Monad m
        => Text
        -> [(Identifier, Maybe (Value m))]
        -> [(Maybe Identifier, Value m)]
        -> GingerT m (Map Identifier (Value m))
mapArgs context spec args =
  go spec posArgs kwArgs
  where
    posArgs = [ v | (Nothing, v) <- args ]
    kwArgs = Map.fromList [ (k, v) | (Just k, v) <- args ]

    go :: [(Identifier, Maybe (Value m))]
       -> [Value m]
       -> Map Identifier (Value m)
       -> GingerT m (Map Identifier (Value m))
    go ((name, defEMay):specs) ps kw = do
      case Map.lookup name kw of
        Just val -> do
          -- Found keyword argument
          let cur = Map.singleton name val
          rest <- go specs ps (Map.delete name kw)
          pure $ cur <> rest
        Nothing ->
          -- No keyword argument found, look for positional argument
          case ps of
            (val:ps') -> do
              let cur = Map.singleton name val
              rest <- go specs ps' kw
              pure $ cur <> rest
            [] -> do
              -- No positional argument found, see if we have a default
              case defEMay of
                Just defE -> do
                  let cur = Map.singleton name defE
                  rest <- go specs ps kw
                  pure $ cur <> rest
                Nothing ->
                  throwError $ ArgumentError context (identifierName name) "argument" "end of arguments"
    go [] _ _ =
      pure mempty
  
evalCallArgs :: Monad m => [Expr] -> [(Identifier, Expr)] -> GingerT m [(Maybe Identifier, Value m)]
evalCallArgs posArgsExpr namedArgsExpr = do
  posArgs <- mapM evalE posArgsExpr
  namedArgs <- mapM evalNamedArg namedArgsExpr
  pure $ zip (repeat Nothing) posArgs ++ namedArgs

splitRNG :: Monad m => GingerT m SomePRNG
splitRNG = do
  rng <- gets evalPRNG
  let (rngL, rngR) = R.splitGen rng
  modify (\e -> e { evalPRNG = rngL })
  pure rngR

callTest :: Monad m => Value m -> Expr -> [Expr] -> [(Identifier, Expr)] -> GingerT m (Value m)
callTest testV scrutinee posArgsExpr namedArgsExpr = do
  case testV of
    TestV t -> do
      args <- evalCallArgs posArgsExpr namedArgsExpr
      ctx <- ask
      env <- gets evalEnv
      rng <- splitRNG
      BoolV <$> native (runTest t scrutinee args ctx env rng)

    ScalarV {} -> do
      BoolV <$> (valuesEqual testV =<< evalE scrutinee)
      `catchError` \err -> case err of
        NotInScopeError {} -> pure FalseV
        _ -> throwError err
      
    x -> do
      call Nothing x (scrutinee : posArgsExpr) namedArgsExpr

callFilter :: Monad m => Value m -> Expr -> [Expr] -> [(Identifier, Expr)] -> GingerT m (Value m)
callFilter filterV scrutinee posArgsExpr namedArgsExpr = do
  case filterV of
    FilterV f -> do
      args <- evalCallArgs posArgsExpr namedArgsExpr
      ctx <- ask
      env <- gets evalEnv
      rng <- splitRNG
      native (runFilter f scrutinee args ctx env rng)

    ScalarV {} -> do
      BoolV <$> (valuesEqual filterV =<< evalE scrutinee)
      `catchError` \err -> case err of
        NotInScopeError {} -> pure FalseV
        _ -> throwError err
      
    x -> do
      call Nothing x (scrutinee : posArgsExpr) namedArgsExpr


call :: Monad m => Maybe (Value m) -> Value m -> [Expr] -> [(Identifier, Expr)] -> GingerT m (Value m)
call callerMay callable posArgsExpr namedArgsExpr = do
  args <- evalCallArgs posArgsExpr namedArgsExpr
  case callable of
    ProcedureV (NativeProcedure _ _ f) ->
      withEnv mempty $ do
        ctx <- ask
        rng <- splitRNG
        native $ f args ctx rng
    ProcedureV (GingerProcedure env argsSig f) -> do
      withEnv env $ do
        maybe (pure ()) (setVar "caller") callerMay
        argDict <- mapArgs "macro" argsSig args
        scoped $ do
          setVars argDict
          evalE f
    ProcedureV NamespaceProcedure -> do
      refID <- allocMutable (DictV mempty)
      pure $ MutableRefV refID
    DictV m -> do
      let callable' = Map.lookup "__call__" m
      case callable' of
        Nothing -> throwError $ NonCallableObjectError "dict"
        Just c -> call callerMay c posArgsExpr namedArgsExpr
    NativeV obj -> do
      case nativeObjectCall obj of
        Just f -> native $ f obj args
        Nothing -> throwError $ NonCallableObjectError "native object"
    x ->
      throwError $ NonCallableObjectError (tagNameOf x)

-- | 'Eval' represents types that can be evaluated in some 'GingerT m' monadic
-- context.
class Eval m a where
  eval :: a -> GingerT m (Value m)

instance Monad m => Eval m Expr where
  eval = evalE

instance Monad m => Eval m Statement where
  eval = evalS

instance Monad m => Eval m Template where
  eval = evalT

-- | Evaluate an expression, dereferencing mutable refs.
evalE :: Monad m => Expr -> GingerT m (Value m)
evalE expr =
  evalE' expr >>= \case
    MutableRefV refID -> derefMutable refID
    v -> pure v

-- | Evaluate an expression without dereferencing mutable refs.
evalE' :: Monad m => Expr -> GingerT m (Value m)
evalE' (PositionedE pos e) = do
  evalE e `catchError` decorateError pos
evalE' NoneE = pure NoneV
evalE' (BoolE b) = pure (BoolV b)
evalE' (StringLitE s) = pure (StringV s)
evalE' (IntLitE i) = pure (IntV i)
evalE' (FloatLitE d) = pure (FloatV d)
evalE' (ListE xs) = ListV <$> V.mapM evalE xs
evalE' (DictE xs) =
  DictV . Map.fromList <$> mapM evalKV xs
evalE' (UnaryE op expr) = do
  v <- evalE expr
  evalUnary op v
evalE' (BinaryE op aExpr bExpr) = do
  a <- evalE aExpr
  b <- evalE bExpr
  evalBinary op a b
evalE' (DotE aExpr b) = do
  a <- evalE aExpr
  attrMay <- getAttr a b
  case attrMay of
    Just attr -> pure attr
    Nothing -> do
      itemMay <- getItem a (StringV . identifierName $ b)
      case itemMay of
        Just item -> pure item
        Nothing -> throwError $ NotInScopeError (Text.show a <> "." <> Text.show b)
evalE' (SliceE sliceeE beginEMay endEMay) = do
  slicee <- evalE sliceeE
  beginMay <- mapM evalE beginEMay
  endMay <- mapM evalE endEMay
  sliceValue slicee beginMay endMay
evalE' (CallE callableExpr posArgsExpr namedArgsExpr) = do
  callable <- evalE callableExpr
  call Nothing callable posArgsExpr namedArgsExpr
evalE' (FilterE scrutinee filterE args kwargs) = do
  f <- withJinjaFilters (eval filterE)
  callFilter f scrutinee args kwargs
evalE' (TernaryE condExpr yesExpr noExpr) = do
  cond <- evalE condExpr >>= asTruth "condition"
  evalE (if cond then yesExpr else noExpr)
evalE' (VarE name) =
  lookupVar name
evalE' (StatementE statement) = do
  evalS statement
evalE' (IsE scrutinee testE args kwargs) = do
  t <- withJinjaTests (evalE testE)
  callTest t scrutinee args kwargs

evalKV :: Monad m => (Expr, Expr) -> GingerT m (Scalar, Value m)
evalKV (kExpr, vExpr) = do
  kVal <- evalE kExpr
  kScalar <- case kVal of
    ScalarV s -> pure s
    x -> throwError $ TagError "dict key" "scalar" (tagNameOf x)
  vVal <- evalE vExpr
  return (kScalar, vVal)

evalNamedArg :: Monad m => (Identifier, Expr) -> GingerT m (Maybe Identifier, Value m)
evalNamedArg (kIdent, vExpr) = do
  vVal <- evalE vExpr
  return (Just kIdent, vVal)

sliceVector :: Vector a -> Maybe Int -> Maybe Int -> Vector a
sliceVector xs startMay endMay =
  let start = case startMay of
                Nothing -> 0
                Just n | n < 0 -> V.length xs + n
                Just n -> n
      end = case endMay of
                Nothing -> V.length xs - start
                Just n | n < 0 -> V.length xs - start + n
                Just n -> n
  in V.take end . V.drop start $ xs

sliceText :: Text -> Maybe Int -> Maybe Int -> Text
sliceText xs startMay endMay =
  let start = case startMay of
                Nothing -> 0
                Just n | n < 0 -> Text.length xs + n
                Just n -> n
      end = case endMay of
                Nothing -> Text.length xs - start
                Just n | n < 0 -> Text.length xs - start + n
                Just n -> n
  in Text.take end . Text.drop start $ xs

sliceByteString :: ByteString -> Maybe Int -> Maybe Int -> ByteString
sliceByteString xs startMay endMay =
  let start = case startMay of
                Nothing -> 0
                Just n | n < 0 -> ByteString.length xs + n
                Just n -> n
      end = case endMay of
                Nothing -> ByteString.length xs - start
                Just n | n < 0 -> ByteString.length xs - start + n
                Just n -> n
  in ByteString.take end . ByteString.drop start $ xs

sliceValue :: Monad m
           => Value m
           -> Maybe (Value m)
           -> Maybe (Value m)
           -> GingerT m (Value m)
sliceValue (ListV xs) startValMay endValMay = do
  startMay <- mapM (native . pure . asIntVal "slice start") startValMay
  endMay <- mapM (native . pure . asIntVal "slice end") endValMay
  pure . ListV $ sliceVector xs (fromIntegral <$> startMay) (fromIntegral <$> endMay)
sliceValue (StringV xs) startValMay endValMay = do
  startMay <- mapM (native . pure . asIntVal "slice start") startValMay
  endMay <- mapM (native . pure . asIntVal "slice end") endValMay
  pure . StringV $ sliceText xs (fromIntegral <$> startMay) (fromIntegral <$> endMay)
sliceValue (BytesV xs) startValMay endValMay = do
  startMay <- mapM (native . pure . asIntVal "slice start") startValMay
  endMay <- mapM (native . pure . asIntVal "slice end") endValMay
  pure . BytesV $ sliceByteString xs (fromIntegral <$> startMay) (fromIntegral <$> endMay)
sliceValue (EncodedV (Encoded xs)) startValMay endValMay = do
  startMay <- mapM (native . pure . asIntVal "slice start") startValMay
  endMay <- mapM (native . pure . asIntVal "slice end") endValMay
  pure . EncodedV . Encoded $ sliceText xs (fromIntegral <$> startMay) (fromIntegral <$> endMay)
sliceValue x _ _ =
  throwError $
    TagError "slicee" "list or string" (tagNameOf x)

numericBinop :: Monad m
             => (Integer -> Integer -> Integer)
             -> (Double -> Double -> Double)
             -> Value m
             -> Value m
             -> GingerT m (Value m)
numericBinop f g a b = native . pure $ numericFunc2 f g a b

numericBinopCatch :: Monad m
                  => (Integer -> Integer -> Either RuntimeError Integer)
                  -> (Double -> Double -> Either RuntimeError Double)
                  -> Value m
                  -> Value m
                  -> GingerT m (Value m)
numericBinopCatch f g a b = native . pure $ numericFunc2Catch f g a b

intBinop :: Monad m
         => (Integer -> Integer -> Either RuntimeError Integer)
         -> Value m
         -> Value m
         -> GingerT m (Value m)
intBinop f a b = native . pure $ intFunc2 f a b

floatBinop :: Monad m
         => (Double -> Double -> Either RuntimeError Double)
         -> Value m
         -> Value m
         -> GingerT m (Value m)
floatBinop f a b = native . pure $ floatFunc2 f a b

boolBinop :: Monad m
         => (Bool -> Bool -> Bool)
         -> Value m
         -> Value m
         -> GingerT m (Value m)
boolBinop f a b = native . pure $ boolFunc2 f a b

valuesEqual :: Monad m
            => Value m
            -> Value m
            -> GingerT m Bool
valuesEqual NoneV NoneV = pure True
valuesEqual (IntV a) (IntV b) = pure (a == b)
valuesEqual (FloatV a) (FloatV b) = pure (a == b)
valuesEqual (StringV a) (StringV b) = pure (a == b)
valuesEqual (BoolV a) (BoolV b) = pure (a == b)
valuesEqual (BytesV a) (BytesV b) = pure (a == b)
valuesEqual (EncodedV a) (EncodedV b) = pure (a == b)
valuesEqual (ListV a) (ListV b)
  | V.length a /= V.length b
  = pure False
  | otherwise
  = V.and <$> V.zipWithM valuesEqual a b
valuesEqual (DictV a) (DictV b) = dictsEqual a b
valuesEqual (NativeV a) (NativeV b) =
  native $ a --> nativeObjectEq b
valuesEqual a b = pure (a == b)

compareValues :: Monad m => Value m -> Value m -> GingerT m Ordering
compareValues NoneV NoneV = pure $ EQ
compareValues (BoolV a) (BoolV b) = pure $ compare a b
compareValues (IntV a) (IntV b) = pure $ compare a b
compareValues (FloatV a) (FloatV b) = pure $ compare a b
compareValues (IntV a) (FloatV b) = pure $ compare (fromInteger a) b
compareValues (FloatV a) (IntV b) = pure $ compare a (fromInteger b)
compareValues (StringV a) (StringV b) = pure $ compare a b
compareValues (EncodedV a) (EncodedV b) = pure $ compare a b
compareValues a b = throwError $ TagError "comparison" "comparable types" (tagNameOf a <> ", " <> tagNameOf b)

valueComparison :: Monad m => (Ordering -> Bool) -> Value m -> Value m -> GingerT m (Value m)
valueComparison f a b = do
  ordering <- compareValues a b
  pure $ BoolV (f ordering)

printfValues :: Monad m => Text -> Value m -> GingerT m (Value m)
printfValues fmtText (ListV args) = do
  pure . StringV . Text.pack $ printfList (Text.unpack fmtText) (V.toList args)
printfValues fmtText x = do
  pure . StringV . Text.pack $ printfList (Text.unpack fmtText) [x]

dictsEqual :: forall m. Monad m
           => Map Scalar (Value m)
           -> Map Scalar (Value m)
           -> GingerT m Bool
dictsEqual m1 m2 =
  and <$> mapM (\k -> (valuesEqual (toValue $ Map.lookup k m1) (toValue $ Map.lookup k m2))) keys
  where
    keys = Set.toList (Map.keysSet m1 <> Map.keysSet m2)

evalUnary :: Monad m => UnaryOperator -> Value m -> GingerT m (Value m)
evalUnary UnopNot (BoolV b) = pure (BoolV $ not b)
evalUnary UnopNot x = throwError $ TagError "not" "boolean" (tagNameOf x)
evalUnary UnopNegate (IntV x) = pure (IntV $ negate x)
evalUnary UnopNegate (FloatV x) = pure (FloatV $ negate x)
evalUnary UnopNegate x = throwError $ TagError "unary -" "number" (tagNameOf x)

evalBinary :: Monad m => BinaryOperator -> Value m -> Value m -> GingerT m (Value m)
evalBinary BinopPlus a b = numericBinop (+) (+) a b
evalBinary BinopMinus a b = numericBinop (-) (-) a b
evalBinary BinopDiv a b = floatBinop safeDiv a b
evalBinary BinopIntDiv a b = intBinop safeIntDiv a b
evalBinary BinopMod (StringV a) b = printfValues a b
evalBinary BinopMod a b = intBinop safeIntMod a b
evalBinary BinopMul a b = numericBinop (*) (*) a b
evalBinary BinopPower a b = numericBinopCatch safeIntPow (\x y -> Right (x ** y)) a b
evalBinary BinopEqual a b = BoolV <$> valuesEqual a b
evalBinary BinopNotEqual a b = BoolV . not <$> valuesEqual a b
evalBinary BinopGT a b = valueComparison (== GT) a b
evalBinary BinopGTE a b = valueComparison (/= LT) a b
evalBinary BinopLT a b = valueComparison (== LT) a b
evalBinary BinopLTE a b = valueComparison (/= GT) a b

evalBinary BinopAnd a b = boolBinop (&&) a b
evalBinary BinopOr a b = boolBinop (||) a b
evalBinary BinopIn a b = case b of
  DictV m -> case a of
    ScalarV k -> pure . BoolV $ k `Map.member` m
    x -> throwError $ TagError "in" "scalar" (tagNameOf x)
  ListV v -> case V.uncons v of
    Nothing ->
      pure FalseV
    Just (x, xs) -> do
      found <- valuesEqual a x
      if found then
        pure . BoolV $ True
      else
        evalBinary BinopIn a (ListV xs)
  x -> throwError $ TagError "in" "list or dict" (tagNameOf x)
evalBinary BinopIndex a b = do
  itemMay <- getItem a b
  case itemMay of
    Just item -> pure item
    Nothing -> do
      attrMay <- case b of
        StringV s -> getAttr a (Identifier s)
        _ -> pure Nothing
      case attrMay of
        Just attr -> pure attr
        Nothing -> pure NoneV
evalBinary BinopConcat a b = concatValues a b

getItem :: Monad m
        => Value m
        -> Value m
        -> GingerT m (Maybe (Value m))
getItem a b = lift $ getItemRaw a b


getAttr :: Monad m
        => Value m
        -> Identifier
        -> GingerT m (Maybe (Value m))
getAttr a b = native $ getAttrRaw a b

safeIntPow :: Integer -> Integer -> Either RuntimeError Integer
safeIntPow _ b | b < 0 = Left (NumericError "**" "negative exponent")
safeIntPow a b = Right (a ^ b)

safeIntDiv :: Integer -> Integer -> Either RuntimeError Integer
safeIntDiv _ 0 = Left (NumericError "//" "division by zero")
safeIntDiv a b = Right (a `div` b)

safeIntMod :: Integer -> Integer -> Either RuntimeError Integer
safeIntMod _ 0 = Left (NumericError "%" "modulo by zero")
safeIntMod a b = Right (a `mod` b)

safeDiv :: Double -> Double -> Either RuntimeError Double
safeDiv a b =
  case a / b of
    c | isNaN c -> Left (NumericError "/" "not a number")
    c | isInfinite c -> Left (NumericError "/" ("division by zero"))
    c -> Right c

concatValues :: Monad m => (Value m) -> (Value m) -> GingerT m (Value m)
concatValues a b = case (a, b) of
  -- Strings, blobs and encoded values concatenate directly
  (StringV x, StringV y) -> pure $ StringV $ x <> y
  (BytesV x, BytesV y) -> pure $ BytesV $ x <> y
  (EncodedV (Encoded x), EncodedV (Encoded y)) -> pure . EncodedV . Encoded $ x <> y

  -- None is a neutral element
  (NoneV, y) -> pure $ y
  (x, NoneV) -> pure $ x

  -- Anything involving encoded values yields encoded results
  (EncodedV x, y) -> do
    yEnc <- encode y
    pure $ EncodedV (x <> yEnc)
  (x, EncodedV y) -> do
    xEnc <- encode x
    pure $ EncodedV (xEnc <> y)

  -- Anything else is cast to and concatenated as strings
  (x, y) -> do
    xStr <- stringify x
    yStr <- stringify y
    pure . StringV $ xStr <> yStr

evalT :: Monad m => Template -> GingerT m (Value m)
evalT t = do
  case templateParent t of
    Nothing ->
      evalS (templateBody t)
    Just parentName -> do
      parent <- loadTemplate parentName
      hush_ $ evalS (templateBody t)
      evalLT parent

evalLT :: Monad m => LoadedTemplate -> GingerT m (Value m)
evalLT t = do
  case loadedTemplateParent t of
    Nothing ->
      evalS (loadedTemplateBody t)
    Just parent -> do
      hush_ $ evalS (loadedTemplateBody t)
      evalLT parent

evalS :: Monad m => Statement -> GingerT m (Value m)
evalS (PositionedS pos s) = do
  evalS s `catchError` decorateError pos
evalS (ImmediateS enc) = pure (EncodedV enc)
evalS (InterpolationS expr) = whenOutputPolicy $ do
  evalE expr
evalS (CommentS _) = pure NoneV
evalS (ForS loopKeyMay loopName itereeE loopCondMay recursivity bodyS elseSMay) = do
  iteree <- evalE itereeE
  evalLoop loopKeyMay loopName iteree loopCondMay recursivity bodyS elseSMay 0
evalS (IfS condE yesS noSMay) = do
  cond <- evalE condE >>= asTruth "condition"
  if cond then evalS yesS else maybe (pure NoneV) evalS noSMay
evalS (MacroS name argsSig body) = do
  env <- gets evalEnv
  argsSig' <- mapM (\(argname, defEMay) -> do
                  defMay <- maybe (pure Nothing) (fmap Just . evalE) defEMay
                  pure (argname, defMay)
                )
                argsSig
  setVar name . ProcedureV $ GingerProcedure env argsSig' (StatementE body)
  pure NoneV

evalS (CallS name posArgsExpr namedArgsExpr bodyS) = whenOutputPolicy $ do
  callee <- lookupVar name
  callerVal <- eval bodyS
  srcPosMay <- gets evalSourcePosition
  let callerID =
        objectIDFromContext "caller" callerVal srcPosMay
  let caller =
        ProcedureV $
          NativeProcedure
            callerID
            (Just ProcedureDoc
              { procedureDocName = "caller"
              , procedureDocArgs = mempty
              , procedureDocReturnType = Just $ TypeDocSingle "markup"
              , procedureDocDescription =
                  "Runs the body of the {% call %} statement that called the " <>
                  "current macro."
              }
            )
            (const . const . const . pure . Right $ callerVal)
  call (Just caller) callee posArgsExpr namedArgsExpr
evalS (FilterS name posArgsExpr namedArgsExpr bodyS) = whenOutputPolicy $ do
  callee <- lookupVar name
  let posArgsExpr' = StatementE bodyS : posArgsExpr
  call Nothing callee posArgsExpr' namedArgsExpr

evalS (SetS target valE) = do
  val <- evalE' valE
  case target of
    SetVar name -> setVar name val
    SetMutable name attr -> setMutable name attr val
  pure NoneV
evalS (SetBlockS target bodyS filterEMaybe) = do
  body <- case filterEMaybe of
            Nothing ->
              evalS bodyS
            Just filterE -> case filterE of
              CallE callee posArgs kwArgs ->
                evalE (CallE callee (StatementE bodyS : posArgs) kwArgs)
              callee ->
                evalE (CallE callee [StatementE bodyS] mempty)
  case target of
    SetVar name -> setVar name body
    SetMutable name path -> setMutable name path body
  pure NoneV
evalS (IncludeS nameE missingPolicy contextPolicy) = do
  name <- eval nameE >>= (eitherExcept . asTextVal)
  templateMay <- case missingPolicy of
    RequireMissing -> Just <$> loadTemplate name
    IgnoreMissing -> loadTemplateMaybe name
  case templateMay of
    Nothing ->
      pure NoneV
    Just template -> do
      withScopeModifier (contextPolicy == WithContext) $ evalLT template
evalS (ImportS srcE nameMay identifiers missingPolicy contextPolicy) = hush $ do
  src <- eval srcE >>= (eitherExcept . asTextVal)
  templateMay <- case missingPolicy of
    RequireMissing -> Just <$> loadTemplate src
    IgnoreMissing -> loadTemplateMaybe src
  case templateMay of
    Nothing ->
      pure NoneV
    Just template -> do
      e' <- scoped . withScopeModifier (contextPolicy == WithContext) $ do
              void $ evalLT template
              gets evalEnv
      let vars = case identifiers of
            Nothing ->
              case nameMay of
                Nothing -> envVars e'
                Just name -> Map.singleton name (DictV . Map.mapKeys toScalar $ envVars e')
            Just importees -> Map.fromList . catMaybes $
              [ (fromMaybe varName alias,) <$> Map.lookup varName (envVars e')
              | (varName, alias) <- importees
              ]
      setVars vars
      pure NoneV
evalS (BlockS name block) =
  evalBlock name block
evalS (WithS varEs bodyS) = do
  vars <- Map.fromList <$> mapM (\(k, valE) -> (k,) <$> evalE valE) varEs
  scoped $ do
    setVars vars
    evalS bodyS
evalS (GroupS xs) = evalSs xs

objectIDFromContext :: Show a
                    => Text
                    -> a
                    -> Maybe SourcePosition
                    -> ObjectID
objectIDFromContext prefix x posMay =
  ObjectID $
    prefix <> ":" <> maybe (hashShow x) hashShow posMay

hush :: Monad m => GingerT m a -> GingerT m a
hush = local (\c -> c { contextOutput = Quiet })

hush_ :: Monad m => GingerT m a -> GingerT m ()
hush_ = void . hush

whenOutputPolicy :: Monad m => GingerT m (Value m) -> GingerT m (Value m)
whenOutputPolicy action = do
  outputPolicy <- asks contextOutput
  if outputPolicy == Output then
    action
  else
    pure NoneV

withScopeModifier :: Monad m => Bool -> GingerT m a -> GingerT m a
withScopeModifier policy inner = do
  let scopeModifier = if policy then id else withoutContext
  scopeModifier inner

evalBlock :: Monad m => Identifier -> Block -> GingerT m (Value m)
evalBlock name block = do
  lblock <- setBlock name block
  super <- makeSuper (loadedBlockParent lblock)
  whenOutputPolicy .
    withScopeModifier (is $ lblockScoped lblock) .
      scoped $ do
        setVar "super" super
        evalS (blockBody . loadedBlock $ lblock)

lblockScoped :: LoadedBlock -> Scoped
lblockScoped lb =
  case loadedBlockParent lb of
    Nothing -> blockScoped (loadedBlock lb)
    Just parent -> lblockScoped parent

makeSuper :: Monad m => Maybe LoadedBlock -> GingerT m (Value m)
makeSuper Nothing = pure NoneV
makeSuper (Just lblock) = do
  ctx <- ask
  env <- gets evalEnv
  rng <- splitRNG
  parent <- makeSuper (loadedBlockParent lblock)
  pure $ dictV
    [ "__call__" .=
        ProcedureV
          (mkFn0 "super()"
              "Evaluate the parent template"
              Nothing $
              eitherExceptM $
                runGingerT
                  (evalS . blockBody . loadedBlock $ lblock)
                  ctx
                  env
                  rng
          )
    , "super" .= parent
    ]

asBool :: Monad m => Text -> Value m -> GingerT m Bool
asBool context x = either throwError pure $ asBoolVal context x

asTruth :: Monad m => Text -> Value m -> GingerT m Bool
asTruth context x = either throwError pure $ asTruthVal context x

evalLoop :: forall m. Monad m
         => Maybe Identifier
         -> Identifier
         -> Value m
         -> Maybe Expr
         -> Recursivity
         -> Statement
         -> Maybe Statement
         -> Int
         -> GingerT m (Value m)
evalLoop loopKeyMay loopName iteree loopCondMay recursivity bodyS elseSMay recursionLevel = do
  -- First, convert the iteree into a plain list.

  itemPairs <- case iteree of
    ListV items -> pure (V.zip (fmap IntV [0..]) items)
    DictV dict -> (pure . V.fromList) [ (ScalarV k, v) | (k, v) <- Map.toList dict ]
    NoneV -> pure mempty
    x -> throwError $ TagError "iteree" "list or dict" (tagNameOf x)

  filtered <- maybe (pure itemPairs) (goFilter itemPairs) loopCondMay

  if null filtered then
    case elseSMay of
      Nothing -> pure NoneV
      Just elseS -> evalS elseS
  else
    go 0 (length filtered) Nothing filtered
  where
    goFilter :: Vector (Value m, Value m) -> Expr -> GingerT m (Vector (Value m, Value m))
    goFilter pairs condE =
      case V.uncons pairs of
        Nothing ->
          pure mempty
        Just ((k, v), xs) -> do
          keep <- scoped $ do
            -- Bind key and value
            maybe (pure ()) (\loopKey -> setVar loopKey k) loopKeyMay
            setVar loopName v
            asTruth "loop condition" =<< evalE condE
          rest <- goFilter xs condE
          if keep then
            pure $ V.cons (k, v) rest
          else
            pure rest

    go :: Int -> Int -> Maybe (Value m) -> Vector (Value m, Value m) -> GingerT m (Value m)
    go n num prevVal pairs = do
      case V.uncons pairs of
        Nothing -> pure NoneV
        Just ((k, v), xs) -> do
          (prevVal', body) <- scoped $ do
            -- Bind key and value
            maybe (pure ()) (\loopKey -> setVar loopKey k) loopKeyMay
            setVar loopName v
            env <- gets evalEnv
            srcPosMay <- gets evalSourcePosition
            let recurFuncID =
                  objectIDFromContext
                    "loop.recur" bodyS srcPosMay
            let cycleFuncID =
                  objectIDFromContext
                    "loop.cycle" bodyS srcPosMay
            setVar "loop" $
              dictV
                [ "index" .= (n + 1)
                , "index0" .= n
                , "revindex" .= (num - n)
                , "revindex0" .= (num - n - 1)
                , "first" .= (n == 0)
                , "last" .= (n == num - 1)
                , "length" .= num
                , "cycle" .= cycleFunc cycleFuncID n
                , "depth" .= (recursionLevel + 1)
                , "depth0" .= recursionLevel
                , "previtem" .= prevVal
                , "nextitem" .= (snd <$> xs V.!? 0)
                , "changed" .= changedFunc env v
                , "__call__" .= if is recursivity then Just (recurFunc recurFuncID env) else Nothing
                ]
            body <- evalS bodyS
            pure (Just v, body)

          rest <- go (succ n) num prevVal' xs
          concatValues body rest

    changedFunc :: Env m -> Value m -> Value m
    changedFunc env v = ProcedureV $ GingerProcedure env [("val", Just v)] $
      EqualE (IndexE (VarE "loop") (StringLitE "previtem")) (VarE "val")

    recurFunc :: ObjectID -> Env m -> Value m
    recurFunc oid env =
      ProcedureV .
        NativeProcedure
          oid
          (Just ProcedureDoc
            { procedureDocName = "loop.recur"
            , procedureDocArgs = mempty
            , procedureDocReturnType = Just $ TypeDocSingle "markup"
            , procedureDocDescription =
                "Recurse one level deeper into the iteree"
            }
          )
          $ \args ctx rng -> do
                case args of
                  [(_, iteree')] ->
                    runGingerT
                      (evalLoop
                        loopKeyMay
                        loopName
                        iteree'
                        loopCondMay
                        recursivity
                        bodyS
                        elseSMay
                        (succ recursionLevel))
                      ctx
                      env
                      rng
                  [] -> pure . Left $
                          ArgumentError "loop()" "1" "argument" "end of arguments"
                  _ -> pure . Left $
                          ArgumentError "loop()" "2" "end of arguments" "argument"
      

    cycleFunc :: ObjectID -> Int -> Value m
    cycleFunc oid n =
      ProcedureV .
        NativeProcedure
          oid
          (Just ProcedureDoc
            { procedureDocName = "loop.cycle"
            , procedureDocArgs =
                [ ArgumentDoc
                    "items"
                    (Just $ TypeDocSingle "list<any>")
                    Nothing
                    ""
                ]
            , procedureDocReturnType = Just TypeDocAny
            , procedureDocDescription =
                "Cycle through 'items': on the n-th iteration of the loop, " <>
                "cycle(items) will return items[n % length(items)]."
            }
          )
          $ \args _ctx _rng -> do
              case args of
                [(_, items)] ->
                  case items of
                    ListV [] ->
                      pure . Right $ NoneV
                    ListV xs -> do
                      let n' = n `mod` V.length xs
                      pure . Right . toValue $ xs V.!? n'
                    _ ->
                      pure . Right $ NoneV
                _ -> pure . Left $
                        ArgumentError "cycle()" "1" "end of arguments" "argument"


evalSs :: Monad m => [Statement] -> GingerT m (Value m)
evalSs stmts = mapM evalS stmts >>= foldM concatValues NoneV
