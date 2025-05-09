{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    -- | @BinaryE op lhs rhs
  | BinaryE !BinaryOperator !Expr !Expr
    -- | @IsE scrutinee test args kwargs@
  | IsE !Expr !Expr ![Expr] ![(Identifier, Expr)]
    -- | @CallE callee args kwargs@
  | CallE !Expr ![Expr] ![(Identifier, Expr)]
    -- | @TernaryE cond yes no@
  | TernaryE !Expr !Expr !Expr
  | VarE !Identifier
  deriving (Show, Eq)

pattern TrueE :: Expr
pattern TrueE = BoolE True

pattern FalseE :: Expr
pattern FalseE = BoolE False

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
          , (100, ListE <$> fuelledList (arbitraryExpr defined))
          , (100, DictE <$> fuelledList ((,) <$> arbitraryExpr defined <*> arbitraryExpr defined))
          , (100, QC.resize (max 0 $ fuel `div` 2 - 1) $
              BinaryE <$> arbitrary
                      <*> arbitraryExpr defined
                      <*> arbitraryExpr defined
            )
          , (100, QC.resize (fuel `div` 3) $
              CallE <$> arbitraryExpr defined
                    <*> fuelledList (arbitraryExpr defined)
                    <*> fuelledList ((,) <$> arbitrary <*> arbitraryExpr defined)
            )
          , (100, QC.resize (fuel `div` 3) $
              TernaryE <$> arbitraryExpr defined
                       <*> arbitraryExpr defined
                       <*> arbitraryExpr defined
            )
          -- , (10, QC.resize (fuel - 1) $
          --     VarE <$> arbitrary
          --   )
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
    -- Test
  deriving (Show, Eq, Enum, Ord, Bounded)

pattern PlusE :: Expr -> Expr -> Expr
pattern PlusE a b = BinaryE BinopPlus a b

pattern MinusE :: Expr -> Expr -> Expr
pattern MinusE a b = BinaryE BinopMinus a b

pattern DivE :: Expr -> Expr -> Expr
pattern DivE a b = BinaryE BinopDiv a b

pattern IntDivE :: Expr -> Expr -> Expr
pattern IntDivE a b = BinaryE BinopIntDiv a b

pattern ModE :: Expr -> Expr -> Expr
pattern ModE a b = BinaryE BinopMod a b

pattern MulE :: Expr -> Expr -> Expr
pattern MulE a b = BinaryE BinopMul a b

pattern PowerE :: Expr -> Expr -> Expr
pattern PowerE a b = BinaryE BinopPower a b

pattern EqualE :: Expr -> Expr -> Expr
pattern EqualE a b = BinaryE BinopEqual a b

pattern NotEqualE :: Expr -> Expr -> Expr
pattern NotEqualE a b = BinaryE BinopNotEqual a b

pattern GT_E :: Expr -> Expr -> Expr
pattern GT_E a b = BinaryE BinopGT a b

pattern GTE_E :: Expr -> Expr -> Expr
pattern GTE_E a b = BinaryE BinopGTE a b

pattern LT_E :: Expr -> Expr -> Expr
pattern LT_E a b = BinaryE BinopLT a b

pattern LTE_E :: Expr -> Expr -> Expr
pattern LTE_E a b = BinaryE BinopLTE a b

pattern AndE :: Expr -> Expr -> Expr
pattern AndE a b = BinaryE BinopAnd a b

pattern OrE :: Expr -> Expr -> Expr
pattern OrE a b = BinaryE BinopOr a b

pattern InE :: Expr -> Expr -> Expr
pattern InE a b = BinaryE BinopIn a b

pattern IndexE :: Expr -> Expr -> Expr
pattern IndexE a b = BinaryE BinopIndex a b

pattern ConcatE :: Expr -> Expr -> Expr
pattern ConcatE a b = BinaryE BinopConcat a b

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
