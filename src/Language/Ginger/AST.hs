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
  shrink (Identifier i) =
    map (Identifier . Text.pack) . filter (not . null) $ shrink $ Text.unpack i


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
  arbitrary = Encoded . Text.replace "{" "{ " . Text.pack <$> QC.listOf arbitrary
  shrink (Encoded e) =
    map (Encoded . Text.pack) . filter (not . null) $ shrink $ Text.unpack e

data TemplateMain
  = TemplateBody !Statement
  | TemplateParent !Text
  deriving (Show, Eq)

data Template =
  Template
    { templateMain :: !TemplateMain
    , templateExports :: ![(Identifier, Expr)]
    , templateBlocks :: ![(Identifier, Block)]
    }
    deriving (Show, Eq)

data Block =
  Block
    { blockBody :: !Statement
    , blockScoped :: !Scoped
    , blockRequired :: !Required
    }
    deriving (Show, Eq)

data Statement
  = -- | Bare text written in the template, outside of any curly braces
    ImmediateS !Encoded
  | -- | An expression interpolation: @{{ expr }}@
    InterpolationS !Expr
  | -- | Comment: @{# comment text #}@
    CommentS !Text
  | -- | @@@
    --   {% for keyVar, valueVar in iteree if loopCondition recursive %}
    --   body
    --   {% else %}
    --   body if empty
    --   {% endfor %}
    --   @@@
    ForS
      !(Maybe Identifier) -- optional loop key variable
      !Identifier -- loop element variable
      !Expr -- iteree
      !(Maybe Expr) -- optional loop condition
      !Recursivity -- enable recursion?
      !Statement -- loop body
      !(Maybe Statement) -- else branch in case iteree is empty
  | -- | @{% if condition %}yes branch{% else %}no branch{% endif %}@
    IfS
      !Expr -- condition
      !Statement -- true branch
      !(Maybe Statement) -- false branch
  | -- | @{% macro name(args) %}body{% endmacro %}@
    MacroS
      !Identifier -- macro name
      ![MacroArg] -- arguments
      !Statement -- body
  | -- | @{% call macroName(args) %}body{% endcall %}@
    CallS
      !Identifier -- callee
      ![Expr] -- positional args
      ![(Identifier, Expr)] -- keyword args
      !Statement -- body (@caller()@)
  | -- | @{% filter filterName(args, kwargs) %}body{% endfilter %}@
    FilterS
      !Identifier -- name
      ![Expr] -- positional args
      ![(Identifier, Expr)] -- keyword args
      !Statement -- body
  | -- | @{% set name=expr %}@
    SetS
      !Identifier -- variable name
      !Expr -- value
  | -- | @{% set name %}body{% endset %}@
    SetBlockS
      !Identifier -- variable name
      !Statement -- body
      !(Maybe Expr) -- optional filter
  | -- | @{% include includee ignore missing with context %}@
    IncludeS
      !Expr
      !IncludeMissingPolicy
      !IncludeContextPolicy
  | -- | @{% import importee as localName item, other_item as other ignore missing with context %}@
    ImportS
      !Expr -- filename
      !(Maybe Identifier) -- local name
      ![(Identifier, Maybe Identifier)] -- [ (imported name, local name) ]
      !IncludeMissingPolicy !IncludeContextPolicy
  | -- | @{% extends expr %}@
    ExtendsS
      !Expr
  | -- | @{% block name with scope required %}body{% endblock %}@
    BlockS
      !Identifier -- block name
      !Block
  | -- | @{% with defs %}body{% endwith %}@
    WithS
      ![(Identifier, Expr)]
      !Statement
  | -- | Group of statements; not parsed, but needed for combining statements
    -- sequentially.
    GroupS ![Statement]
  deriving (Show, Eq)

data IncludeMissingPolicy
  = RequireMissing
  | IgnoreMissing
  deriving (Show, Eq, Ord, Enum, Bounded)

data IncludeContextPolicy
  = WithContext
  | WithoutContext
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary Statement where
  arbitrary = arbitraryStatement mempty
  shrink (GroupS xs) = map GroupS $ shrink xs
  shrink (ImmediateS txt) = map ImmediateS $ shrink txt
  shrink (InterpolationS e) = map InterpolationS $ shrink e
  shrink (CommentS txt) = map (CommentS . Text.pack) $ shrink $ Text.unpack txt
  shrink (ForS keyMay val iteree condMay recur body elseBranchMay) =
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> pure recur <*> pure body <*> shrink elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> pure recur <*> shrink body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> shrink recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> shrink condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> shrink iteree <*> pure condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> shrink val <*> pure iteree <*> pure condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> shrink keyMay <*> pure val <*> pure iteree <*> pure condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
    [body] ++
    maybe [] (:[]) elseBranchMay
  shrink (IfS cond yes noMay) =
    (IfS <$> shrink cond <*> pure yes <*> pure noMay) ++
    (IfS <$> pure cond <*> shrink yes <*> pure noMay) ++
    (IfS <$> pure cond <*> pure yes <*> shrink noMay) ++
    (pure yes) ++
    (maybeToList noMay)
  shrink (MacroS name args body) =
    (MacroS <$> pure name <*> pure args <*> shrink body) ++
    (MacroS <$> pure name <*> shrink args <*> pure body) ++
    (MacroS <$> shrink name <*> shrink args <*> shrink body) ++
    [body]
  shrink (CallS name args kwargs body) =
    (CallS <$> pure name <*> pure args <*> pure kwargs <*> shrink body) ++
    (CallS <$> pure name <*> pure args <*> shrink kwargs <*> shrink body) ++
    (CallS <$> pure name <*> shrink args <*> pure kwargs <*> pure body) ++
    (CallS <$> shrink name <*> shrink args <*> pure kwargs <*> shrink body) ++
    [body]
  shrink (FilterS name args kwargs body) =
    (FilterS <$> pure name <*> pure args <*> pure kwargs <*> shrink body) ++
    (FilterS <$> pure name <*> pure args <*> shrink kwargs <*> shrink body) ++
    (FilterS <$> pure name <*> shrink args <*> pure kwargs <*> pure body) ++
    (FilterS <$> shrink name <*> shrink args <*> pure kwargs <*> shrink body) ++
    [body]
  shrink (SetS name expr) =
    (SetS <$> pure name <*> shrink expr) ++
    (SetS <$> shrink name <*> pure expr)
  shrink (SetBlockS name body filterMay) =
    (SetBlockS <$> pure name <*> pure body <*> shrink filterMay) ++
    (SetBlockS <$> pure name <*> shrink body <*> pure filterMay) ++
    (SetBlockS <$> shrink name <*> pure body <*> pure filterMay)
  shrink (IncludeS name m c) =
    (IncludeS <$> pure name <*> pure m <*> shrink c) ++
    (IncludeS <$> pure name <*> shrink m <*> pure c) ++
    (IncludeS <$> shrink name <*> pure m <*> pure c)
  shrink (ImportS name lname imports m c) =
    (ImportS <$> pure name <*> pure lname <*> pure imports <*> pure m <*> shrink c) ++
    (ImportS <$> pure name <*> pure lname <*> pure imports <*> shrink m <*> pure c) ++
    (ImportS <$> pure name <*> pure lname <*> shrink imports <*> pure m <*> pure c) ++
    (ImportS <$> pure name <*> shrink lname <*> pure imports <*> pure m <*> pure c) ++
    (ImportS <$> shrink name <*> pure lname <*> pure imports <*> pure m <*> pure c)
  shrink (ExtendsS expr) =
    (ExtendsS <$> shrink expr) ++
    [InterpolationS expr]
  shrink (BlockS name (Block b s r)) =
    (BlockS <$> pure name <*> (Block <$> pure b <*> pure s <*> shrink r)) ++
    (BlockS <$> pure name <*> (Block <$> pure b <*> pure s <*> shrink r)) ++
    (BlockS <$> shrink name <*> pure (Block b s r)) ++
    [b]
  shrink (WithS defs body) =
    (WithS <$> pure defs <*> shrink body) ++
    (WithS <$> shrink defs <*> pure body) ++
    [body]

instance Arbitrary IncludeMissingPolicy where
  arbitrary = QC.oneof $ map pure [minBound .. maxBound]

instance Arbitrary IncludeContextPolicy where
  arbitrary = QC.oneof $ map pure [minBound .. maxBound]

arbitraryStatement :: Set Identifier -> QC.Gen Statement
arbitraryStatement defined = do
    fuel <- QC.getSize
    QC.resize (max 0 $ fuel - 1) $
      QC.oneof
        [ ImmediateS <$> arbitrary
        , InterpolationS <$> arbitraryExpr defined
        , CommentS . Text.strip . Text.pack <$> arbitrary
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
                 <*> QC.resize (fuel `div` 4) (QC.listOf (arbitraryExpr defined))
                 <*> QC.resize (fuel `div` 4) (QC.listOf ((,) <$> arbitrary <*> arbitraryExpr defined))
                 <*> QC.resize (fuel `div` 2) (arbitraryStatement $ defined <> Set.singleton "caller")
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
            vars <- QC.listOf1 ((,) <$> arbitrary <*> QC.resize (fuel `div` 4) (arbitraryExpr defined))
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
    -- | @UnaryE op rhs
  | UnaryE !UnaryOperator !Expr
    -- | @BinaryE op lhs rhs
  | BinaryE !BinaryOperator !Expr !Expr
    -- | @IsE scrutinee test args kwargs@
  | IsE !Expr !Expr ![Expr] ![(Identifier, Expr)]
    -- | @CallE callee args kwargs@
  | CallE !Expr ![Expr] ![(Identifier, Expr)]
    -- | @FilterE arg0 filter args kwargs@
  | FilterE !Expr !Expr ![Expr] ![(Identifier, Expr)]
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
  shrink (StringLitE txt) = map (StringLitE . Text.pack) $ shrink $ Text.unpack txt
  shrink (IntLitE i) = IntLitE <$> shrink i
  shrink (FloatLitE i) = FloatLitE <$> shrink i
  shrink (StatementE s) = StatementE <$> shrink s
  shrink (ListE []) = pure NoneE
  shrink (ListE [x]) = (ListE . (:[]) <$> shrink x) ++ [x]
  shrink (ListE xs) = ListE <$> shrink xs
  shrink (DictE xs) = DictE <$> shrink xs
  shrink (UnaryE op e) =
    (UnaryE <$> pure op <*> shrink e) ++
    [e]
  shrink (BinaryE op a b) =
    (BinaryE <$> pure op <*> shrink a <*> pure b) ++
    (BinaryE <$> pure op <*> pure a <*> shrink b) ++
    [a, b]
  shrink (IsE a b args kwargs) =
    (IsE <$> pure a <*> pure b <*> pure args <*> shrink kwargs) ++
    (IsE <$> pure a <*> pure b <*> shrink args <*> pure kwargs) ++
    (IsE <$> shrink a <*> pure b <*> pure args <*> pure kwargs) ++
    (IsE <$> pure a <*> shrink b <*> pure args <*> pure kwargs) ++
    [a, b] ++ args ++ map snd kwargs
  shrink (CallE f args kwargs) =
    (CallE <$> pure f <*> pure args <*> shrink kwargs) ++
    (CallE <$> pure f <*> shrink args <*> pure kwargs) ++
    (CallE <$> shrink f <*> pure args <*> pure kwargs) ++
    [f] ++ args ++ map snd kwargs
  shrink (FilterE a f args kwargs) =
    (FilterE <$> pure a <*> pure f <*> pure args <*> shrink kwargs) ++
    (FilterE <$> pure a <*> pure f <*> shrink args <*> pure kwargs) ++
    (FilterE <$> pure a <*> shrink f <*> pure args <*> pure kwargs) ++
    (FilterE <$> shrink a <*> pure f <*> pure args <*> pure kwargs) ++
    [a, f] ++ args ++ map snd kwargs
  shrink (TernaryE a b c) =
    (TernaryE <$> pure a <*> pure b <*> shrink c) ++
    (TernaryE <$> pure a <*> shrink b <*> pure c) ++
    (TernaryE <$> shrink a <*> pure b <*> pure c) ++
    [a, b, c]
  shrink (VarE i) = VarE <$> shrink i
  shrink _ = []

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
          , (100, ListE <$> fuelledList (arbitraryExpr defined))
          , (100, DictE <$> fuelledList ((,) <$> arbitraryExpr defined <*> arbitraryExpr defined))
          , (90, QC.resize (max 0 $ fuel `div` 2 - 1) $
              BinaryE <$> (arbitrary `QC.suchThat` (/= BinopDot))
                      <*> arbitraryExpr defined
                      <*> arbitraryExpr defined
            )
          , (10, QC.resize (max 0 $ fuel `div` 2 - 1) $
              DotE <$> arbitraryExpr defined
                   <*> (StringLitE . identifierName <$> arbitrary)
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

data UnaryOperator
  = UnopNot
  | UnopNegate
  deriving (Show, Eq, Enum, Ord, Bounded)

pattern NotE :: Expr -> Expr
pattern NotE a = UnaryE UnopNot a

pattern NegateE :: Expr -> Expr
pattern NegateE a = UnaryE UnopNegate a

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
    -- List / dict indexing (@[]@)
  | BinopIndex
    -- Dot member access (@.@)
  | BinopDot
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

pattern DotE :: Expr -> Expr -> Expr
pattern DotE a b = BinaryE BinopDot a b

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
