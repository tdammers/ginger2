{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString (..))

newtype Identifier =
  Identifier { identifierName :: Text }
  deriving (Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . Text.pack

newtype Encoded =
  Encoded { encoded :: Text }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

data Statement
  = ImmediateS Encoded
  | InterpolationS Expr
  | CommentS Text
  | ForS
      (Maybe Identifier) -- optional loop key variable
      Identifier -- loop element variable
      Expr -- iteree
      (Maybe Expr) -- optional loop condition
      Recursivity -- enable recursion?
      Statement -- loop body
      (Maybe Statement) -- else branch in case iteree is empty
  | IfS
      Expr -- condition
      Statement -- true branch
      (Maybe Statement) -- false branch
  | MacroS
      Identifier -- macro name
      [MacroArg] -- arguments
      Statement -- body
  | CallS
      Identifier -- callee
      [Expr] -- positional args
      [(Identifier, Expr)] -- keyword args
  | FilterS
      Identifier -- name
      [Expr] -- positional args
      [(Identifier, Expr)] -- keyword args
      Statement -- body
  | SetS
      Identifier -- variable name
      Expr -- value
  | SetBlockS
      Identifier -- variable name
      Statement -- body
      (Maybe Expr) -- optional filter
  | IncludeS Expr
  | ExtendsS Expr
  | BlockS
      Identifier -- block name
      Statement -- body
      Scoped -- scoped block?
      Required -- required block?
  | WithS [(Identifier, Expr)] Statement
  deriving (Show)

class Boolish a where
  is :: a -> Bool

isNot :: Boolish a => a -> Bool
isNot = not . is

data Scoped = NotScoped | Scoped
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Boolish Scoped where
  is = (== Scoped)

data Required = Optional | Required
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Boolish Required where
  is = (== Required)

data Recursivity = NotRecursive | Recursive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Boolish Recursivity where
  is = (== Recursive)

type MacroArg = (Identifier, Maybe Expr)

data Expr
  = NoneE
  | BoolE Bool
  | StringLitE Text
  | IntLitE Integer
  | FloatLitE Double
  | StatementE Statement
  | ListE [Expr]
  | DictE [(Expr, Expr)]
  | BinaryE BinaryOperator Expr Expr
  | CallE Expr [Expr] [(Identifier, Expr)]
  | TernaryE Expr Expr Expr
  | VarE Identifier
  deriving (Show)

data BinaryOperator
    -- Math
  = BinopPlus
  | BinopMinus
  | BinopDiv
  | BinopIntDiv
  | BinopMod
  | BinopMul
  | BinopPower
    -- Comparison
  | BinopEqual
  | BinopNotEqual
  | BinopGT
  | BinopGTE
  | BinopLT
  | BinopLTE
    -- Boolean
  | BinopAnd
  | BinopOr

    -- List / dict membership
  | BinopIn
    -- List / dict indexing (@.@ / @[]@)
  | BinopIndex
    -- String concatenation
  | BinopConcat
  deriving (Show, Eq, Enum, Ord, Bounded)
