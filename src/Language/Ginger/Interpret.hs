{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.Interpret
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad (foldM)
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
    { contextEncode :: Text -> m Encoded
    }

defContext :: Applicative m => Context m
defContext =
  Context
    { contextEncode = pure . Encoded
    }

newtype GingerT m a =
  GingerT { unGingerT :: ReaderT (Context m) (StateT (Env m) (ExceptT RuntimeError m)) a }
  deriving (Functor, Applicative, Monad)

runGingerT :: Monad m => GingerT m a -> Context m -> Env m -> m (Either RuntimeError a)
runGingerT g ctx env = runExceptT (evalStateT (runReaderT (unGingerT g) ctx) env)

native :: Monad m => m (Either RuntimeError a) -> GingerT m a
native action =
  lift action >>= either throwError pure

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
  native (Right <$> nativeObjectStringified n)
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
  native (Right <$> encoder str)

encode :: Monad m => Value m -> GingerT m Encoded
encode (EncodedV e) = pure e
encode (NativeV n) = native (Right <$> nativeObjectEncoded n)
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
                  cur <- Map.singleton name <$> evalE defE
                  rest <- go specs ps kw
                  pure $ cur <> rest
                Nothing ->
                  throwError $ ArgumentError Nothing (Just $ identifierName name) (Just "argument") (Just "end of arguments")
    go [] _ _ =
      pure mempty
  
call :: Monad m => Value m -> [Expr] -> [(Identifier, Expr)] -> GingerT m (Value m)
call callable posArgsExpr namedArgsExpr = do
  posArgs <- mapM evalE posArgsExpr
  namedArgs <- mapM evalNamedArg namedArgsExpr
  let args = zip (repeat Nothing) posArgs ++ namedArgs
  case callable of
    ProcedureV (NativeProcedure f) ->
      native $ f args
    ProcedureV (GingerProcedure argsSig f) -> do
      argDict <- mapArgs argsSig args
      scoped $ do
        setVars argDict
        evalE f
    DictV m -> do
      let callable' = Map.lookup "__call__" m
      case callable' of
        Nothing -> throwError $ NonCallableObjectError (Just "dict")
        Just c -> call c posArgsExpr namedArgsExpr
    NativeV obj -> do
      native $ nativeObjectCall obj obj args
    x ->
      throwError $ NonCallableObjectError (Just . tagNameOf $ x)

evalE :: Monad m => Expr -> GingerT m (Value m)
evalE NoneE = pure NoneV
evalE (BoolE b) = pure (BoolV b)
evalE (StringLitE s) = pure (StringV s)
evalE (IntLitE i) = pure (IntV i)
evalE (FloatLitE d) = pure (FloatV d)
evalE (ListE xs) = ListV <$> mapM evalE xs
evalE (DictE xs) =
  DictV . Map.fromList <$> mapM evalKV xs
evalE (BinaryE op aExpr bExpr) = do
  a <- evalE aExpr
  b <- evalE bExpr
  evalBinary op a b
evalE (CallE callableExpr posArgsExpr namedArgsExpr) = do
  callable <- evalE callableExpr
  call callable posArgsExpr namedArgsExpr

evalE (TernaryE condExpr yesExpr noExpr) = do
  cond <- evalE condExpr >>= asBool "condition"
  evalE (if cond then yesExpr else noExpr)
evalE (VarE name) =
  lookupVar name
evalE (StatementE statement) = do
  evalS statement

evalKV :: Monad m => (Expr, Expr) -> GingerT m (Scalar, Value m)
evalKV (kExpr, vExpr) = do
  kVal <- evalE kExpr
  kScalar <- case kVal of
    ScalarV s -> pure s
    x -> throwError $ TagError Nothing (Just "scalar") (Just . tagNameOf $ x)
  vVal <- evalE vExpr
  return (kScalar, vVal)

evalNamedArg :: Monad m => (Identifier, Expr) -> GingerT m (Maybe Identifier, Value m)
evalNamedArg (kIdent, vExpr) = do
  vVal <- evalE vExpr
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
evalBinary BinopIndex a b = case a of
  DictV m -> case b of
    ScalarV k -> pure . toValue $ k `Map.lookup` m
    x -> throwError $ TagError (Just "in") (Just "scalar") (Just . tagNameOf $ x)
  ListV xs -> case b of
    IntV i -> pure . toValue . listToMaybe . drop (fromInteger i) $ xs
    x -> throwError $ TagError (Just "in") (Just "int") (Just . tagNameOf $ x)
  StringV str -> case b of
    IntV i -> pure . toValue . Text.take 1 . Text.drop (fromInteger i) $ str
    x -> throwError $ TagError (Just "in") (Just "int") (Just . tagNameOf $ x)
  x -> throwError $ TagError (Just "in") (Just "list or dict") (Just . tagNameOf $ x)
evalBinary BinopConcat a b = concatValues a b

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

evalS :: Monad m => Statement -> GingerT m (Value m)
evalS (ImmediateS enc) = pure (EncodedV enc)
evalS (InterpolationS expr) = evalE expr
evalS (CommentS _) = pure NoneV
evalS (ForS loopKeyMay loopName itereeE loopCondMay recursivity bodyS elseSMay) = do
  iteree <- evalE itereeE
  evalLoop loopKeyMay loopName iteree loopCondMay recursivity bodyS elseSMay 0
evalS (IfS condE yesS noSMay) = do
  cond <- evalE condE >>= asBool "condition"
  if cond then evalS yesS else maybe (pure NoneV) evalS noSMay
evalS (MacroS name argsSig body) = do
  setVar name . ProcedureV $ GingerProcedure argsSig (StatementE body)
  pure NoneV

evalS (CallS name posArgsExpr namedArgsExpr) = do
  callee <- lookupVar name
  call callee posArgsExpr namedArgsExpr
evalS (FilterS name posArgsExpr namedArgsExpr bodyS) = do
  callee <- lookupVar name
  let posArgsExpr' = StatementE bodyS : posArgsExpr
  call callee posArgsExpr' namedArgsExpr

evalS (SetS name valE) = do
  val <- evalE valE
  setVar name val
  pure NoneV
evalS (SetBlockS name bodyS filterEMaybe) = do
  body <- case filterEMaybe of
            Nothing ->
              evalS bodyS
            Just filterE -> case filterE of
              CallE callee posArgs kwArgs ->
                evalE (CallE callee (StatementE bodyS : posArgs) kwArgs)
              callee ->
                evalE (CallE callee [StatementE bodyS] mempty)
  setVar name body
  pure NoneV
evalS (IncludeS _nameE) = do
  throwError $ NotImplementedError (Just "include")
evalS (ExtendsS _nameE) = do
  throwError $ NotImplementedError (Just "extends")
evalS (BlockS _name _bodyS _scopedness _requiredness) = do
  throwError $ NotImplementedError (Just "block")
evalS (WithS varEs bodyS) = do
  vars <- Map.fromList <$> mapM (\(k, valE) -> (k,) <$> evalE valE) varEs
  scoped $ do
    setVars vars
    evalS bodyS

asBool :: Monad m => Text -> Value m -> GingerT m Bool
asBool _ (BoolV b) = pure b
asBool _ NoneV = pure False
asBool context x = throwError $ TagError (Just context) (Just "boolean") (Just . tagNameOf $ x)

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
evalLoop loopKeyMay loopName iteree loopCondMay _recursivity bodyS elseSMay recursionLevel = do
  -- First, convert the iteree into a plain list.

  itemPairs <- case iteree of
    ListV items -> pure (zip (map IntV [0..]) items)
    DictV dict -> pure [ (ScalarV k, v) | (k, v) <- Map.toList dict ]
    NoneV -> pure []
    x -> throwError $ TagError (Just "iteree") (Just "list or dict") (Just . tagNameOf $ x)

  filtered <- maybe (pure itemPairs) (goFilter itemPairs) loopCondMay

  if null filtered then
    case elseSMay of
      Nothing -> pure NoneV
      Just elseS -> evalS elseS
  else
    go 0 (length filtered) Nothing filtered
  where
    goFilter :: [(Value m, Value m)] -> Expr -> GingerT m [(Value m, Value m)]
    goFilter [] _ = pure []
    goFilter ((k, v):xs) condE = do
      keep <- scoped $ do
        -- Bind key and value
        maybe (pure ()) (\loopKey -> setVar loopKey k) loopKeyMay
        setVar loopName v
        asBool "loop condition" =<< evalE condE
      rest <- goFilter xs condE
      if keep then
        pure $ (k, v):rest
      else
        pure rest

    go :: Int -> Int -> Maybe (Value m) -> [(Value m, Value m)] -> GingerT m (Value m)
    go _ _ _ [] = pure NoneV
    go n num prevVal ((k, v):xs) = do
      (prevVal', body) <- scoped $ do
        -- Bind key and value
        maybe (pure ()) (\loopKey -> setVar loopKey k) loopKeyMay
        setVar loopName v
        setVar "loop" $
          dictV
            [ "index" .= (n + 1)
            , "index0" .= n
            , "revindex" .= (num - n)
            , "revindex0" .= (num - n - 1)
            , "first" .= (n == 0)
            , "last" .= (n == num - 1)
            , "length" .= num
            , "cycle" .= cycleFunc n
            , "depth" .= (recursionLevel + 1)
            , "depth0" .= recursionLevel
            , "previtem" .= prevVal
            , "nextitem" .= (snd <$> listToMaybe xs)
            , "changed" .= changedFunc
            -- TODO:
            -- , "__call__" .= if is recursivity then Just recurFunc else Nothing
            ]
        body <- evalS bodyS
        pure (Just v, body)

      rest <- go (succ n) num prevVal' xs
      concatValues body rest

    changedFunc :: Value m
    changedFunc = ProcedureV $ GingerProcedure [("val", Just (VarE loopName))] $
      BinaryE BinopEqual (BinaryE BinopIndex (VarE "loop") (StringLitE "previtem")) (VarE "val")

    cycleFunc :: Int -> Value m -> Value m
    cycleFunc n items =
      case items of
        ListV [] ->
          NoneV
        ListV xs -> do
          let n' = n `mod` length xs
          toValue $ listToMaybe $ drop n' xs
        _ ->
          NoneV


evalSs :: Monad m => [Statement] -> GingerT m (Value m)
evalSs stmts = mapM evalS stmts >>= foldM concatValues NoneV
