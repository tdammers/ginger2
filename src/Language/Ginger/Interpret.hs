{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.Ginger.Interpret
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Exception (catch)
import Control.Monad.State
  ( StateT (..)
  , MonadState (..)
  , MonadTrans (..)
  , evalStateT
  , get
  , gets
  , modify
  )
import Control.Monad.Reader
  ( ReaderT
  , MonadReader
  , runReaderT
  , asks
  )
import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (..)
  , runExceptT
  , throwError
  )
import qualified Data.ByteString.Base64 as Base64
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (listToMaybe)

newtype Env m =
  Env
    { envVars :: Map Identifier (Value m)
    }

emptyEnv :: Env m
emptyEnv = Env mempty

data Context m =
  Context
    { contextRunNative :: forall a. m a -> m (Either RuntimeError a)
    , contextEncode :: Text -> m Encoded
    }

defContext :: Applicative m => Context m
defContext =
  Context
    { contextRunNative = fmap Right
    , contextEncode = pure . Encoded
    }

defContextIO :: Context IO
defContextIO =
  defContext
    { contextRunNative = \action ->
        fmap Right action `catch` (pure . Left)
    }

newtype GingerT m a =
  GingerT { unGingerT :: ReaderT (Context m) (StateT (Env m) (ExceptT RuntimeError m)) a }
  deriving (Functor, Applicative, Monad)

runGingerT :: Monad m => GingerT m a -> Context m -> Env m -> m (Either RuntimeError a)
runGingerT g ctx env = runExceptT (evalStateT (runReaderT (unGingerT g) ctx) env)

native :: Monad m => m a -> GingerT m a
native action = do
  runNative <- asks contextRunNative
  lift (runNative action) >>= either throwError pure

deriving instance Monad m => MonadState (Env m) (GingerT m)
deriving instance Monad m => MonadReader (Context m) (GingerT m)
deriving instance Monad m => MonadError RuntimeError (GingerT m)

instance MonadTrans GingerT where
  lift = GingerT . lift . lift . lift

lookupVar :: Monad m
          => Identifier
          -> GingerT m (Value m)
lookupVar name =
  lookupVarMaybe name >>= maybe (throwError $ NotInScopeError (Just $ identifierName name)) pure

lookupVarMaybe :: Monad m
               => Identifier
               -> GingerT m (Maybe (Value m))
lookupVarMaybe name = gets (Map.lookup name . envVars)

setVar :: Monad m
       => Identifier
       -> Value m
       -> GingerT m ()
setVar name val = modify (Env . Map.insert name val . envVars)

setVars :: Monad m
        => Map Identifier (Value m)
        -> GingerT m ()
setVars vars = modify (Env . (<> vars) . envVars)

scoped :: Monad m
       => GingerT m a
       -> GingerT m a
scoped action = do
  s <- get
  retval <- action
  put s
  return retval

stringify :: Monad m => Value m -> GingerT m Text
stringify NoneV = pure ""
stringify (BoolV True) = pure "true"
stringify (BoolV False) = pure ""
stringify (StringV str) = pure str
stringify (BytesV b) =
  pure . decodeUtf8 . Base64.encode $ b
stringify (EncodedV (Encoded e)) =
  pure e
stringify (IntV i) = pure $ Text.show i
stringify (FloatV f) = pure $ Text.show f
stringify (ScalarV s) = pure . Text.show $ s
stringify (ListV xs) = do
  elems <- mapM stringify xs
  pure $ Text.intercalate ", " elems
stringify (DictV m) = do
  elems <- mapM stringifyKV $ Map.toAscList m
  pure $ Text.intercalate ", " elems
stringify (NativeV n) =
  native (nativeObjectStringified n)
stringify (ProcedureV _) =
  pure "[[procedure]]"

stringifyKV :: Monad m => (Scalar, Value m) -> GingerT m Text
stringifyKV (k, v) = do
  kStr <- stringify (ScalarV k)
  vStr <- stringify v
  pure $ kStr <> ": " <> vStr

encodeText :: Monad m => Text -> GingerT m Encoded
encodeText str = do
  encoder <- asks contextEncode
  native (encoder str)

encode :: Monad m => Value m -> GingerT m Encoded
encode (EncodedV e) = pure e
encode (NativeV n) = native (nativeObjectEncoded n)
encode (ProcedureV _) = pure $ Encoded "[[procedure]]"
encode v = encodeText =<< stringify v

mapArgs :: forall m. Monad m
        => [(Identifier, Maybe Expr)]
        -> [(Maybe Identifier, Value m)]
        -> GingerT m (Map Identifier (Value m))
mapArgs spec args =
  go spec posArgs kwArgs
  where
    posArgs = [ v | (Nothing, v) <- args ]
    kwArgs = Map.fromList [ (k, v) | (Just k, v) <- args ]

    go :: [(Identifier, Maybe Expr)]
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
                  cur <- Map.singleton name <$> eval defE
                  rest <- go specs ps kw
                  pure $ cur <> rest
                Nothing ->
                  throwError $ ArgumentError Nothing (Just $ identifierName name) (Just "argument") (Just "end of arguments")
    go [] _ _ =
      pure mempty
  
call :: Monad m => Value m -> [Expr] -> [(Identifier, Expr)] -> GingerT m (Value m)
call callable posArgsExpr namedArgsExpr = do
  posArgs <- mapM eval posArgsExpr
  namedArgs <- mapM evalNamedArg namedArgsExpr
  let args = zip (repeat Nothing) posArgs ++ namedArgs
  case callable of
    ProcedureV (NativeProcedure f) ->
      native $ f args
    ProcedureV (GingerProcedure argsSig f) -> do
      argDict <- mapArgs argsSig args
      scoped $ do
        setVars argDict
        eval f
    x ->
      throwError $ NonCallableObjectError (Just . tagNameOf $ x)

eval :: Monad m => Expr -> GingerT m (Value m)
eval NoneE = pure NoneV
eval (BoolE b) = pure (BoolV b)
eval (StringLitE s) = pure (StringV s)
eval (IntLitE i) = pure (IntV i)
eval (FloatLitE d) = pure (FloatV d)
eval (ListE xs) = ListV <$> mapM eval xs
eval (DictE xs) =
  DictV . Map.fromList <$> mapM evalKV xs
eval (BinaryE op aExpr bExpr) = do
  a <- eval aExpr
  b <- eval bExpr
  evalBinary op a b
eval (CallE callableExpr posArgsExpr namedArgsExpr) = do
  callable <- eval callableExpr
  call callable posArgsExpr namedArgsExpr

eval (TernaryE condExpr yesExpr noExpr) = do
  cond <- eval condExpr
  case cond of
    BoolV True -> eval yesExpr
    BoolV False -> eval noExpr
    x -> throwError $ TagError (Just "condition") (Just "boolean") (Just . tagNameOf $ x)
eval (VarE name) =
  lookupVar name
eval (StatementE statement) = do
  body <- interpretStatement statement
  return $ EncodedV body

evalKV :: Monad m => (Expr, Expr) -> GingerT m (Scalar, Value m)
evalKV (kExpr, vExpr) = do
  kVal <- eval kExpr
  kScalar <- case kVal of
    ScalarV s -> pure s
    x -> throwError $ TagError Nothing (Just "scalar") (Just . tagNameOf $ x)
  vVal <- eval vExpr
  return (kScalar, vVal)

evalNamedArg :: Monad m => (Identifier, Expr) -> GingerT m (Maybe Identifier, Value m)
evalNamedArg (kIdent, vExpr) = do
  vVal <- eval vExpr
  return (Just kIdent, vVal)

numericBinop :: Monad m
             => (Integer -> Integer -> Integer)
             -> (Double -> Double -> Double)
             -> Value m
             -> Value m
             -> GingerT m (Value m)
numericBinop f _ (IntV a) (IntV b) = pure . IntV $ a `f` b
numericBinop _ f (FloatV a) (FloatV b) = pure . FloatV $ a `f` b
numericBinop _ f (IntV a) (FloatV b) = pure . FloatV $ fromInteger a `f` b
numericBinop _ f (FloatV a) (IntV b) = pure . FloatV $ a `f` fromInteger b
numericBinop _ _ (FloatV _) b = throwError (TagError Nothing (Just "number") (Just . tagNameOf $ b))
numericBinop _ _ (IntV _) b = throwError (TagError Nothing (Just "number") (Just . tagNameOf $ b))
numericBinop _ _ b _ = throwError (TagError Nothing (Just "number") (Just . tagNameOf $ b))

intBinop :: Monad m
         => (Integer -> Integer -> Integer)
         -> Value m
         -> Value m
         -> GingerT m (Value m)
intBinop f (IntV a) (IntV b) = pure . IntV $ a `f` b
intBinop _ (IntV _) b = throwError (TagError Nothing (Just "int") (Just . tagNameOf $ b))
intBinop _ b _ = throwError (TagError Nothing (Just "int") (Just . tagNameOf $ b))

boolBinop :: Monad m
         => (Bool -> Bool -> Bool)
         -> Value m
         -> Value m
         -> GingerT m (Value m)
boolBinop f (BoolV a) (BoolV b) = pure . BoolV $ a `f` b
boolBinop _ (BoolV _) b = throwError (TagError Nothing (Just "bool") (Just . tagNameOf $ b))
boolBinop _ b _ = throwError (TagError Nothing (Just "bool") (Just . tagNameOf $ b))

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
valuesEqual (ListV []) (ListV []) = pure True
valuesEqual (ListV []) (ListV _) = pure False
valuesEqual (ListV _) (ListV []) = pure False
valuesEqual (ListV (x:xs)) (ListV (y:ys)) =
    (&&) <$> valuesEqual x y
         <*> valuesEqual (ListV xs) (ListV ys)
valuesEqual (DictV a) (DictV b) = dictsEqual a b
valuesEqual _ _ = pure False

compareValues :: Monad m => Value m -> Value m -> GingerT m Ordering
compareValues NoneV NoneV = pure $ EQ
compareValues (BoolV a) (BoolV b) = pure $ compare a b
compareValues (IntV a) (IntV b) = pure $ compare a b
compareValues (FloatV a) (FloatV b) = pure $ compare a b
compareValues (IntV a) (FloatV b) = pure $ compare (fromInteger a) b
compareValues (FloatV a) (IntV b) = pure $ compare a (fromInteger b)
compareValues (StringV a) (StringV b) = pure $ compare a b
compareValues (EncodedV a) (EncodedV b) = pure $ compare a b
compareValues a b = throwError $ TagError (Just "comparison") (Just "comparable types") (Just $ tagNameOf a <> ", " <> tagNameOf b)

valueComparison :: Monad m => (Ordering -> Bool) -> Value m -> Value m -> GingerT m (Value m)
valueComparison f a b = do
  ordering <- compareValues a b
  pure $ BoolV (f ordering)

dictsEqual :: forall m. Monad m
           => Map Scalar (Value m)
           -> Map Scalar (Value m)
           -> GingerT m Bool
dictsEqual m1 m2 =
  and <$> mapM (\k -> (valuesEqual (toValue $ Map.lookup k m1) (toValue $ Map.lookup k m2))) keys
  where
    keys = Set.toList (Map.keysSet m1 <> Map.keysSet m2)

evalBinary :: Monad m => BinaryOperator -> Value m -> Value m -> GingerT m (Value m)
evalBinary BinopPlus a b = numericBinop (+) (+) a b
evalBinary BinopMinus a b = numericBinop (-) (-) a b
evalBinary BinopDiv a b = numericBinop (div) (/) a b
evalBinary BinopIntDiv a b = intBinop div a b
evalBinary BinopMod a b = intBinop mod a b
evalBinary BinopMul a b = numericBinop (*) (*) a b
evalBinary BinopPower a b = numericBinop (^) (**) a b
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
    x -> throwError $ TagError (Just "in") (Just "scalar") (Just . tagNameOf $ x)
  ListV [] -> pure . BoolV $ False
  ListV (x:xs) -> do
    found <- valuesEqual a x
    if found then
      pure . BoolV $ True
    else
      evalBinary BinopIn (ListV xs) b
  x -> throwError $ TagError (Just "in") (Just "list or dict") (Just . tagNameOf $ x)
evalBinary BinopIndex a b = case b of
  DictV m -> case a of
    ScalarV k -> pure . toValue $ k `Map.lookup` m
    x -> throwError $ TagError (Just "in") (Just "scalar") (Just . tagNameOf $ x)
  ListV xs -> case a of
    IntV i -> pure . toValue . listToMaybe . drop (fromInteger i) $ xs
    x -> throwError $ TagError (Just "in") (Just "int") (Just . tagNameOf $ x)
  StringV str -> case a of
    IntV i -> pure . toValue . Text.take 1 . Text.drop (fromInteger i) $ str
    x -> throwError $ TagError (Just "in") (Just "int") (Just . tagNameOf $ x)
  x -> throwError $ TagError (Just "in") (Just "list or dict") (Just . tagNameOf $ x)
evalBinary BinopConcat a b = case (a, b) of
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

interpretStatement :: Monad m => Statement -> GingerT m Encoded
interpretStatement (ImmediateS enc) = pure enc
interpretStatement (InterpolationS expr) = encode =<< eval expr
interpretStatement (CommentS _) = pure mempty
interpretStatement (ForS _loopKeyMay _loopName _iteree _loopCondMay _recursivity _bodyS _elseSMay) =
  throwError $ NotImplementedError (Just "for")
interpretStatement (IfS condE yesS noSMay) = do
  condV <- eval condE
  case condV of
    BoolV cond -> do
      if cond then
        interpretStatement yesS
      else
        maybe (pure mempty) interpretStatement $ noSMay
    x -> throwError $ TagError (Just "condition") (Just "boolean") (Just . tagNameOf $ x)
interpretStatement (MacroS name argsSig body) = do
  setVar name . ProcedureV $ GingerProcedure argsSig (StatementE body)
  pure mempty

interpretStatement (CallS name posArgsExpr namedArgsExpr) = do
  callee <- lookupVar name
  encode =<< call callee posArgsExpr namedArgsExpr
interpretStatement (FilterS name posArgsExpr namedArgsExpr bodyS) = do
  callee <- lookupVar name
  let posArgsExpr' = StatementE bodyS : posArgsExpr
  encode =<< call callee posArgsExpr' namedArgsExpr

interpretStatement (SetS name valE) = do
  val <- eval valE
  setVar name val
  pure mempty
interpretStatement (SetBlockS name bodyS filterEMaybe) = do
  body <- case filterEMaybe of
            Nothing ->
              EncodedV <$> interpretStatement bodyS
            Just filterE -> case filterE of
              CallE callee posArgs kwArgs ->
                eval (CallE callee (StatementE bodyS : posArgs) kwArgs)
              callee ->
                eval (CallE callee [StatementE bodyS] mempty)
  setVar name body
  pure mempty
interpretStatement (IncludeS _nameE) = do
  throwError $ NotImplementedError (Just "include")
interpretStatement (ExtendsS _nameE) = do
  throwError $ NotImplementedError (Just "extends")
interpretStatement (BlockS _name _bodyS _scopedness _requiredness) = do
  throwError $ NotImplementedError (Just "block")
interpretStatement (WithS varEs bodyS) = do
  vars <- Map.fromList <$> mapM (\(k, valE) -> (k,) <$> eval valE) varEs
  scoped $ do
    setVars vars
    interpretStatement bodyS

interpretStatements :: Monad m => [Statement] -> GingerT m Encoded
interpretStatements = fmap mconcat . mapM interpretStatement
