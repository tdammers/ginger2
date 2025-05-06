{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString (..))
import Test.Tasty.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC

import Debug.Trace

newtype Identifier =
  Identifier { identifierName :: Text }
  deriving (Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . Text.pack

instance Arbitrary Identifier where
  arbitrary = do
    x <- QC.oneof $ map pure identifierLeadChars
    xs <- QC.listOf (QC.oneof $ map pure identifierChars)
    pure $ Identifier $ Text.pack (x : xs)

identifierLeadChars :: [Char]
identifierLeadChars =
  [ 'a' .. 'z' ] ++ ['A' .. 'Z'] ++ ['_']

identifierChars :: [Char]
identifierChars =
  identifierLeadChars ++ ['0' .. '9']

newtype Encoded =
  Encoded { encoded :: Text }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance Arbitrary Encoded where
  arbitrary = Encoded . Text.pack <$> QC.listOf arbitrary

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

instance Arbitrary Statement where
  arbitrary = do
    fuel <- QC.getSize
    QC.oneof
      [ ImmediateS <$> QC.resize (fuel - 1) arbitrary
      ]

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

instance Arbitrary Expr where
  arbitrary = do
    fuel <- QC.getSize
    if fuel <= 1 then
      pure NoneE
    else
      QC.oneof
        [ pure NoneE
        , BoolE <$> arbitrary
        , StringLitE <$> QC.resize (fuel - 1) (Text.pack <$> QC.listOf arbitrary)
        , IntLitE <$> QC.resize (fuel - 1) arbitrary
        , FloatLitE <$> QC.resize (fuel - 1) arbitrary
        , StatementE <$> QC.resize (fuel - 1) arbitrary
        , ListE <$> fuelledList arbitrary
        , DictE <$> fuelledList arbitrary
        , QC.resize (max 0 $ fuel `div` 2 - 1) $
            BinaryE <$> arbitrary <*> arbitrary <*> arbitrary
        , QC.resize (fuel `div` 3) $
            CallE <$> arbitrary <*> fuelledList arbitrary <*> fuelledList arbitrary
        , QC.resize (fuel `div` 3) $
            TernaryE <$> arbitrary <*> arbitrary <*> arbitrary
        , QC.resize (fuel - 1) $
            VarE <$> arbitrary
        ]

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

instance Arbitrary BinaryOperator where
  arbitrary = QC.oneof (map pure [minBound .. maxBound])

fuelledList :: QC.Gen a -> QC.Gen [a]
fuelledList subGen = do
  fuel <- QC.getSize
  traceM $ "fuelledList, fuel = " ++ show fuel
  if fuel < 1 then
    pure []
  else do
    s <- QC.chooseInt (1, fuel)
    x <- QC.resize s subGen
    xs <- QC.resize (max 0 $ fuel - s - 10) (fuelledList subGen)
    pure $ x : xs
