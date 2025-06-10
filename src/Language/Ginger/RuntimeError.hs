module Language.Ginger.RuntimeError
where

import Data.Text (Text)
import Control.Exception
import Text.Printf

import Language.Ginger.SourcePosition

data RuntimeError
  = ArgumentError 
      Text -- ^ Callee
      Text -- ^ Argument (position or name)
      Text -- ^ Expected argument
      Text -- ^ Actual argument
  | TagError
      Text -- ^ Identifier / object / context
      Text -- ^ Expected type(s)
      Text -- ^ Actual type
  | NonCallableObjectError 
      Text -- ^ Object that was attempted to be used as a callable
  | NotInScopeError
      Text -- ^ Identifier
  | NotImplementedError
      Text -- ^ The thing that isn't implemented
  | NumericError
      Text -- ^ Identifier / object / context
      Text -- ^ Error description
  | TemplateFileNotFoundError
      Text -- ^ Template name
  | TemplateParseError
      Text -- ^ Template name
      Text -- ^ Error message
  | FatalError
      Text
  | GenericError
      Text
  | PositionedError
      !SourcePosition
      !RuntimeError
  deriving (Show, Eq)

instance Exception RuntimeError where

-- | Pretty-print a 'RuntimeError'. The output is meant to be useful as a
-- user-facing error message.
prettyRuntimeError :: RuntimeError -> String
prettyRuntimeError (NotImplementedError what) =
  printf "Not implemented: %s"
    what
prettyRuntimeError (ArgumentError callee argument expected actual) =
  printf "Argument error in argument '%s' to %s: expected %s, but got %s."
    argument callee expected actual
prettyRuntimeError (TagError context expected actual) =
  printf "Type error in %s: expected %s, but got %s."
    context expected actual
prettyRuntimeError (NonCallableObjectError obj) =
  printf "Non-callable error: %s is not callable."
    obj
prettyRuntimeError (NotInScopeError thing) =
  printf "Not in scope: %s"
    thing
prettyRuntimeError (NumericError context what) =
  printf "Numeric error in %s: %s"
    context what
prettyRuntimeError (TemplateFileNotFoundError name) =
  printf "Template file not found: %s"
    name
prettyRuntimeError (TemplateParseError name msg) =
  printf "Template parser error in %s:\n%s"
    name msg
prettyRuntimeError (GenericError what) =
  printf "Error: %s"
    what
prettyRuntimeError (FatalError what) =
  printf "FATAL ERROR: %s"
    what
prettyRuntimeError (PositionedError (SourcePosition file line column) err) =
  printf "In %s:%i:%i:\n%s"
    file line column (prettyRuntimeError err)

