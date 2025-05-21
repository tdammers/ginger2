{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Language.Ginger.Interpret.Type
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

import Control.Monad (forM)
import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (..)
  , runExceptT
  , throwError
  )
import Control.Monad.Reader
  ( ReaderT
  , MonadReader
  , runReaderT
  , asks
  )
import Control.Monad.State
  ( StateT (..)
  , MonadState (..)
  , MonadTrans (..)
  , evalStateT
  , get
  , gets
  , modify
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

newtype GingerT m a =
  GingerT { unGingerT :: ReaderT (Context m) (StateT (EvalState m) (ExceptT RuntimeError m)) a }
  deriving (Functor, Applicative, Monad)

data EvalState m =
  EvalState
    { evalEnv :: !(Env m)
    , evalLoadedTemplates :: !(Map Text CachedTemplate)
    , evalBlocks :: !(Map Identifier LoadedBlock)
    }

data LoadedBlock =
  LoadedBlock
    { loadedBlock :: !Block
    , loadedBlockParent :: !(Maybe LoadedBlock)
    }
    deriving (Show)

instance Semigroup (EvalState m) where
  a <> b =
    EvalState
      { evalEnv = evalEnv a <> evalEnv b
      , evalLoadedTemplates = evalLoadedTemplates a <> evalLoadedTemplates b
      , evalBlocks = evalBlocks a <> evalBlocks b
      }

data CachedTemplate
  = CachedTemplate !LoadedTemplate
  | MissingTemplate

data LoadedTemplate =
  LoadedTemplate
    { loadedTemplateParent :: !(Maybe LoadedTemplate)
    , loadedTemplateBody :: !Statement
    }

runGingerT :: Monad m => GingerT m a -> Context m -> Env m -> m (Either RuntimeError a)
runGingerT g ctx env =
  runExceptT (evalStateT (runReaderT (unGingerT g) ctx) (EvalState env mempty mempty))

deriving instance Monad m => MonadState (EvalState m) (GingerT m)
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
lookupVarMaybe name = do
  valEnv <- gets (Map.lookup name . envVars . evalEnv)
  case valEnv of
    Nothing ->
      asks (Map.lookup name . contextVars)
    Just val ->
      pure $ Just val

modifyEnv :: Monad m
          => (Env m -> Env m)
          -> GingerT m ()
modifyEnv f =
  modify (\s -> s { evalEnv = f (evalEnv s) })

setVar :: Monad m
       => Identifier
       -> Value m
       -> GingerT m ()
setVar name val =
  modifyEnv (\e -> e { envVars = Map.insert name val $ envVars e })

setVars :: Monad m
        => Map Identifier (Value m)
        -> GingerT m ()
setVars vars = modifyEnv (\e -> e { envVars = envVars e <> vars})

setBlock :: Monad m
         => Identifier
         -> Block
         -> GingerT m LoadedBlock
setBlock name block = do
  mparent <- gets (Map.lookup name . evalBlocks)
  let lblock = LoadedBlock block Nothing
      lblock' = maybe lblock (appendLoadedBlock lblock) mparent
  modify (\s -> s { evalBlocks = Map.insert name lblock' (evalBlocks s) })
  pure lblock'

appendLoadedBlock :: LoadedBlock -> LoadedBlock -> LoadedBlock
appendLoadedBlock t h =
  case loadedBlockParent h of
    Nothing -> h { loadedBlockParent = Just t }
    Just p -> h { loadedBlockParent = Just (appendLoadedBlock t p) }

getBlock :: Monad m
         => Identifier
         -> GingerT m (Maybe LoadedBlock)
getBlock name = gets (Map.lookup name . evalBlocks)

scoped :: Monad m
       => GingerT m a
       -> GingerT m a
scoped action = do
  s <- get
  retval <- action
  modifyEnv $ const (evalEnv s)
  return retval

-- | Run a 'GingerT' action in a fresh environment; however, any globals set
-- by the invoked action will be ported back to the calling environment.
withoutContext :: Monad m
               => GingerT m a
               -> GingerT m a
withoutContext action = do
  e <- gets evalEnv
  modifyEnv envRoot
  retval <- action
  e' <- gets evalEnv
  modifyEnv $ const (e' <> e)
  return retval

withEnv :: Monad m
        => Env m
        -> GingerT m a
        -> GingerT m a
withEnv env action = do
  s <- get
  modifyEnv (const env)
  retval <- action
  modifyEnv (const $ evalEnv s)
  return retval

withExtEnv :: Monad m
        => Env m
        -> GingerT m a
        -> GingerT m a
withExtEnv env action = do
  s <- get
  modifyEnv (<> env)
  retval <- action
  modifyEnv (const $ evalEnv s)
  return retval

bind :: Monad m => (Env m -> a) -> GingerT m a
bind f = f <$> gets evalEnv

-- | Lift a dictionary value into the current scope, such that dictionary keys
-- become variables bound to the respective values in the dictionary.
scopify :: forall m. Monad m
        => Identifier
        -> GingerT m ()
scopify name = do
  lookupVar name >>= \case
    DictV items -> do
      items' <- forM (Map.toList items) $ \(k, v) -> do
        k' <- scalarToIdentifier k
        pure (k', v)
      setVars $ Map.fromList items'
    x -> throwError $ TagError (Just "liftScope") (Just "dict") (Just . tagNameOf $ x)
  where
    scalarToIdentifier :: Scalar -> GingerT m Identifier
    scalarToIdentifier (StringScalar txt) = pure $ Identifier txt
    scalarToIdentifier x = throwError $ TagError (Just "liftScope") (Just "string") (Just . tagNameOf $ ScalarV x)
