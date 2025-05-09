{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.Interpret.Type
where

import Language.Ginger.AST
import Language.Ginger.RuntimeError
import Language.Ginger.Value

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

withEnv :: Monad m
        => Map Identifier (Value m)
        -> GingerT m a
        -> GingerT m a
withEnv vars action = do
  s <- get
  put (s { envVars = vars })
  retval <- action
  put s
  return retval

