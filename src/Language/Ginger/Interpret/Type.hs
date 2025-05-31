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
import Language.Ginger.SourcePosition
import Language.Ginger.Value

import Control.Applicative ((<|>))
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | The Ginger interpreter monad. Provides error reporting / handling via
-- 'MonadError', an execution context ('Context'), and an evaluation state
-- ('EvalState').
newtype GingerT m a =
  GingerT { unGingerT :: ReaderT (Context m) (StateT (EvalState m) (ExceptT RuntimeError m)) a }
  deriving (Functor, Applicative, Monad)

-- | Evaluation state. This keeps track of variables in scope, as well as
-- loaded templates and blocks, and the current source position.
data EvalState m =
  EvalState
    { evalEnv :: !(Env m)
    , evalMutables :: !(Map RefID (Value m))
    , evalNextRefID :: !RefID
    , evalLoadedTemplates :: !(Map Text CachedTemplate)
    , evalBlocks :: !(Map Identifier LoadedBlock)
    , evalSourcePosition :: !(Maybe SourcePosition)
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
      , evalMutables = evalMutables a <> evalMutables b
      , evalNextRefID = max (evalNextRefID a) (evalNextRefID b)
      , evalLoadedTemplates = evalLoadedTemplates a <> evalLoadedTemplates b
      , evalBlocks = evalBlocks a <> evalBlocks b
      , evalSourcePosition = evalSourcePosition a <|> evalSourcePosition b
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
  runExceptT
    (evalStateT
      (runReaderT (unGingerT g) ctx)
      (EvalState env { envRootMay = Just env } mempty (RefID 0) mempty mempty Nothing)
    )

decorateError :: Monad m
              => SourcePosition
              -> RuntimeError
              -> GingerT m a
decorateError pos err =
  throwError (PositionedError pos err)

deriving instance Monad m => MonadState (EvalState m) (GingerT m)
deriving instance Monad m => MonadReader (Context m) (GingerT m)
deriving instance Monad m => MonadError RuntimeError (GingerT m)

instance MonadTrans GingerT where
  lift = GingerT . lift . lift . lift

lookupVar :: Monad m
          => Identifier
          -> GingerT m (Value m)
lookupVar name =
  lookupVarMaybe name >>= maybe (throwError $ NotInScopeError (identifierName name)) pure

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
setVars vars = modifyEnv (\e -> e { envVars = vars <> envVars e })

setMutable :: forall m. Monad m
           => Identifier
           -> Identifier
           -> Value m
           -> GingerT m ()
setMutable name attr val = do
  varVal <- lookupVar name
  refID <- case varVal of
    MutableRefV i -> pure i
    x -> throwError $ TagError (identifierName name) "mutable ref" (tagNameOf x)
  modifyMutable refID go
  where
    go :: Value m -> GingerT m (Value m)
    go (DictV m) = pure (DictV $ Map.insert (toScalar attr) val m)
    go x = throwError $
                TagError
                (identifierName name)
                "dict"
                (tagNameOf x)

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

bindMutable :: Monad m
            => Value m
            -> GingerT m RefID
bindMutable val = do
  refID <- gets evalNextRefID
  modify (\s ->
    s { evalNextRefID = succ (evalNextRefID s)
      , evalMutables = Map.insert refID val (evalMutables s)
      })
  pure refID

assignMutable :: Monad m
              => RefID
              -> Value m
              -> GingerT m ()
assignMutable refID val =
  modify (\s -> s { evalMutables = Map.insert refID val (evalMutables s) })

modifyMutable :: Monad m
              => RefID
              -> (Value m -> GingerT m (Value m))
              -> GingerT m ()
modifyMutable refID f = do
  mval <- derefMutable refID
  mval' <- f mval
  modify (\s -> s { evalMutables = Map.insert refID mval' (evalMutables s) })

derefMutableMaybe :: Monad m
                  => RefID
                  -> GingerT m (Maybe (Value m))
derefMutableMaybe refID =
  gets (Map.lookup refID . evalMutables)

derefMutable :: Monad m
             => RefID
             -> GingerT m (Value m)
derefMutable refID =
  derefMutableMaybe refID >>=
    maybe (throwError $ NotInScopeError ("ref#" <> Text.show refID)) pure

setSourcePosition :: Monad m
                  => SourcePosition
                  -> GingerT m ()
setSourcePosition pos = do
  modify (\s -> s { evalSourcePosition = Just pos })

clearSourcePosition :: Monad m
                    => GingerT m ()
clearSourcePosition =
  modify (\s -> s { evalSourcePosition = Nothing })


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

bind :: Monad m => (Env m -> a) -> GingerT m a
bind f = f <$> gets evalEnv

-- | Lift a dictionary value into the current scope, such that dictionary keys
-- become variables bound to the respective values in the dictionary.
scopify :: forall m. Monad m
        => Value m
        -> GingerT m ()
scopify = \case
    DictV items -> do
      items' <- forM (Map.toList items) $ \(k, v) -> do
        k' <- scalarToIdentifier k
        pure (k', v)
      setVars $ Map.fromList items'
    x -> throwError $ TagError "liftScope" "dict" (tagNameOf x)
  where
    scalarToIdentifier :: Scalar -> GingerT m Identifier
    scalarToIdentifier (StringScalar txt) = pure $ Identifier txt
    scalarToIdentifier x = throwError $ TagError "liftScope" "string" (tagNameOf $ ScalarV x)

withJinjaFilters :: (Monad m)
                 => GingerT m a
                 -> GingerT m a
withJinjaFilters = withJinjaKey "filters"

withJinjaTests :: (Monad m)
                 => GingerT m a
                 -> GingerT m a
withJinjaTests = withJinjaKey "tests"

withJinjaKey :: (Monad m)
             => Identifier
             -> GingerT m a
             -> GingerT m a
withJinjaKey key inner =
  scoped $ do
    jinjaFilters <-
      fmap (fromMaybe NoneV . Map.lookup (toScalar key)) $
      lookupVar "__jinja__" >>= eitherExceptM . asDictVal
    scopify jinjaFilters
    inner

