module Language.Ginger.RuntimeError
where

import Data.Text (Text)
import Control.Exception

data RuntimeError
  = ArgumentError 
      (Maybe Text) -- ^ Callee
      (Maybe Text) -- ^ Argument (position or name)
      (Maybe Text) -- ^ Expected argument
      (Maybe Text) -- ^ Actual argument
  | TagError
      (Maybe Text) -- ^ Identifier / object / context
      (Maybe Text) -- ^ Expected type(s)
      (Maybe Text) -- ^ Actual type
  | NonCallableObjectError 
      (Maybe Text) -- ^ Object that was attempted to be used as a callable
  | NotInScopeError
      (Maybe Text) -- ^ Identifier
  | NotImplementedError
      (Maybe Text) -- ^ The thing that isn't implemented
  deriving (Show, Eq)

instance Exception RuntimeError where
