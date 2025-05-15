{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , evalLoadedTemplates :: !(Map Text (CachedTemplate m))
    }

data CachedTemplate m
  = CachedTemplate !(LoadedTemplate m)
  | MissingTemplate

data LoadedTemplate m =
  LoadedTemplate
    { loadedTemplateExports :: !(Map Identifier (Value m))
    , loadedTemplateMain :: !(LoadedTemplateMain m)
    }

data LoadedTemplateMain m
  = LoadedTemplateBody !Statement
  | LoadedTemplateParent !(LoadedTemplate m)

runGingerT :: Monad m => GingerT m a -> Context m -> Env m -> m (Either RuntimeError a)
runGingerT g ctx env = runExceptT (evalStateT (runReaderT (unGingerT g) ctx) (EvalState env mempty))

native :: Monad m => m (Either RuntimeError a) -> GingerT m a
native action =
  lift action >>= either throwError pure

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
lookupVarMaybe name = gets (Map.lookup name . envVars . evalEnv)

modifyEnv :: Monad m
          => (Env m -> Env m)
          -> GingerT m ()
modifyEnv f =
  modify (\s -> s { evalEnv = f (evalEnv s) })

setVar :: Monad m
       => Identifier
       -> Value m
       -> GingerT m ()
setVar name val = modifyEnv (\e -> e { envVars = Map.insert name val $ envVars e })

setVars :: Monad m
        => Map Identifier (Value m)
        -> GingerT m ()
setVars vars = modifyEnv (\e -> e { envVars = vars <> envVars e})

scoped :: Monad m
       => GingerT m a
       -> GingerT m a
scoped action = do
  s <- get
  retval <- action
  modifyEnv $ const (evalEnv s)
  return retval

withoutContext :: Monad m
               => GingerT m a
               -> GingerT m a
withoutContext action = do
  s <- get
  modifyEnv envRoot
  retval <- action
  modifyEnv $ const (evalEnv s)
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
