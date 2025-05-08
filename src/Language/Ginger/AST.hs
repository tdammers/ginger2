{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger.AST
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString (..))
import Test.Tasty.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)

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
  = ImmediateS !Encoded
  | InterpolationS !Expr
  | CommentS !Text
  | ForS
      !(Maybe Identifier) -- optional loop key variable
      !Identifier -- loop element variable
      !Expr -- iteree
      !(Maybe Expr) -- optional loop condition
      !Recursivity -- enable recursion?
      !Statement -- loop body
      !(Maybe Statement) -- else branch in case iteree is empty
  | IfS
      !Expr -- condition
      !Statement -- true branch
      !(Maybe Statement) -- false branch
  | MacroS
      !Identifier -- macro name
      ![MacroArg] -- arguments
      !Statement -- body
  | CallS
      !Identifier -- callee
      ![Expr] -- positional args
      ![(Identifier, Expr)] -- keyword args
  | FilterS
      !Identifier -- name
      ![Expr] -- positional args
      ![(Identifier, Expr)] -- keyword args
      !Statement -- body
  | SetS
      !Identifier -- variable name
      !Expr -- value
  | SetBlockS
      !Identifier -- variable name
      !Statement -- body
      !(Maybe Expr) -- optional filter
  | IncludeS Expr
  | ExtendsS Expr
  | BlockS
      !Identifier -- block name
      !Statement -- body
      !Scoped -- scoped block?
      !Required -- required block?
  | WithS ![(Identifier, Expr)] Statement
  | GroupS ![Statement]
  deriving (Show, Eq)

instance Arbitrary Statement where
  arbitrary = arbitraryStatement mempty

arbitraryStatement :: Set Identifier -> QC.Gen Statement
arbitraryStatement defined = do
    fuel <- QC.getSize
    QC.resize (max 0 $ fuel - 1) $
      QC.oneof
        [ ImmediateS <$> arbitrary
        , InterpolationS <$> arbitraryExpr defined
        , CommentS . Text.pack <$> arbitrary
        , do
            let fuel' = fuel `div` 6
            keyNameMaybe <- arbitrary
            valName <- arbitrary
            iteree <- QC.resize fuel' $ arbitraryExpr defined
            let defined' = defined <> Set.singleton valName <> Set.fromList (maybeToList keyNameMaybe) <> Set.singleton "loop"
            filterMay <- QC.oneof [ pure Nothing, QC.resize fuel' $ Just <$> arbitraryExpr defined ]
            body <- QC.resize fuel' $ arbitraryStatement defined'
            elseBody <- QC.resize fuel' $ arbitrary
            recursive <- arbitrary

            pure $ ForS keyNameMaybe valName iteree filterMay recursive body elseBody
        , IfS <$> QC.resize (fuel `div` 3) (arbitraryExpr defined)
              <*> QC.resize (fuel `div` 3) (arbitraryStatement defined)
              <*> QC.resize (fuel `div` 3) (QC.oneof [ pure Nothing, Just <$> arbitraryStatement defined ])
        , do
            args <- QC.resize (fuel `div` 2) $
                      QC.listOf ((,) <$> arbitrary <*> QC.oneof [ pure Nothing, Just <$> arbitraryExpr mempty ])
            let defined' = defined <> Set.fromList (map fst args)
            body <- QC.resize (fuel `div` 2) (arbitraryStatement defined')
            name <- arbitrary
            pure $ MacroS name args body
        , CallS <$> arbitrary
                 <*> QC.resize (fuel `div` 2) (QC.listOf (arbitraryExpr defined))
                 <*> QC.resize (fuel `div` 2) (QC.listOf ((,) <$> arbitrary <*> arbitraryExpr defined))
        , FilterS <$> arbitrary
                  <*> QC.resize (fuel `div` 3) (QC.listOf (arbitraryExpr defined))
                  <*> QC.resize (fuel `div` 3) (QC.listOf ((,) <$> arbitrary <*> arbitraryExpr defined))
                  <*> QC.resize (fuel `div` 3) (arbitraryStatement defined)
        , SetS <$> arbitrary
               <*> QC.resize (fuel `div` 2) (arbitraryExpr defined)
        , SetBlockS <$> arbitrary
                    <*> QC.resize (fuel `div` 2) (arbitraryStatement defined)
                    <*> QC.resize (fuel `div` 2) (QC.oneof [ pure Nothing, Just <$> arbitraryExpr defined ])
        -- , IncludeS <$> arbitraryExpr defined
        -- , ExtendsS <$> arbitraryExpr defined
        -- , BlockS <$> arbitrary
        --          <*> QC.resize (fuel `div` 2) (arbitraryStatement defined)
        --          <*> arbitrary
        --          <*> arbitrary
        , do
            vars <- QC.listOf ((,) <$> arbitrary <*> QC.resize (fuel `div` 4) (arbitraryExpr defined))
            let defined' = defined <> Set.fromList (map fst vars)
            body <- QC.resize (fuel * 3 `div` 4) (arbitraryStatement defined')
            pure $ WithS vars body
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

instance Arbitrary Scoped where
  arbitrary = QC.oneof $ map pure [minBound..maxBound]

instance Arbitrary Required where
  arbitrary = QC.oneof $ map pure [minBound..maxBound]

instance Arbitrary Recursivity where
  arbitrary = QC.oneof $ map pure [minBound..maxBound]

type MacroArg = (Identifier, Maybe Expr)

data Expr
  = NoneE
  | BoolE !Bool
  | StringLitE !Text
  | IntLitE !Integer
  | FloatLitE !Double
  | StatementE !Statement
  | ListE ![Expr]
  | DictE ![(Expr, Expr)]
  | BinaryE !BinaryOperator !Expr !Expr
  | CallE !Expr ![Expr] ![(Identifier, Expr)]
  | TernaryE !Expr !Expr !Expr
  | VarE !Identifier
  deriving (Show, Eq)

instance Arbitrary Expr where
  arbitrary = arbitraryExpr mempty

arbitraryExpr :: Set Identifier -> QC.Gen Expr
arbitraryExpr defined = do
  fuel <- QC.getSize
  if fuel <= 1 then
    pure NoneE
  else do
    let baseOptions =
          [ (10, pure NoneE)
          , (100, BoolE <$> arbitrary)
          , (100, StringLitE <$> QC.resize (fuel - 1) (Text.pack <$> QC.listOf arbitrary))
          , (100, IntLitE <$> QC.resize (fuel - 1) arbitrary)
          , (100, FloatLitE <$> QC.resize (fuel - 1) arbitrary)
          , (100, StatementE <$> QC.resize (fuel - 1) (arbitraryStatement defined))
          , (100, ListE <$> fuelledList arbitrary)
          , (100, DictE <$> fuelledList arbitrary)
          , (100, QC.resize (max 0 $ fuel `div` 2 - 1) $
              BinaryE <$> arbitrary <*> arbitrary <*> arbitrary
            )
          , (100, QC.resize (fuel `div` 3) $
              CallE <$> arbitrary <*> fuelledList arbitrary <*> fuelledList arbitrary
            )
          , (100, QC.resize (fuel `div` 3) $
              TernaryE <$> arbitrary <*> arbitrary <*> arbitrary
            )
          , (10, QC.resize (fuel - 1) $
              VarE <$> arbitrary
            )
          ]
        extraOptions =
          if Set.null defined then
            []
          else
            [ (100, VarE <$> QC.oneof (map pure $ Set.toList defined))
            ]
        options = baseOptions <> extraOptions
    QC.frequency options

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
