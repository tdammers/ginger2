{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Ginger.AST
where

import Data.Aeson (ToJSON (..), ToJSONKey (..), FromJSON (..), FromJSONKey (..))
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.List (intersperse)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text, pattern (:<) )
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Tasty.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC

import Language.Ginger.SourcePosition

-- | Identifiers are used to represent variable names and object fields.
newtype Identifier =
  Identifier { identifierName :: Text }
  deriving (Show, Eq, Ord, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

mapIdentifier :: (Text -> Text) -> Identifier -> Identifier
mapIdentifier f (Identifier t) = Identifier (f t)

instance IsString Identifier where
  fromString = Identifier . Text.pack

instance Arbitrary Identifier where
  arbitrary = do
    x <- QC.oneof $ map pure identifierLeadChars
    xs <- QC.listOf (QC.oneof $ map pure identifierChars)
    pure $ Identifier $ Text.pack (x : xs)
  shrink _ = [] -- shrinking identifiers breaks references

isValidIdentifier :: Text -> Bool
isValidIdentifier t =
  case t of
    Text.Empty -> False
    c :< _ -> c `elem` identifierLeadChars

identifierLeadChars :: [Char]
identifierLeadChars =
  [ 'a' .. 'z' ] ++ ['A' .. 'Z'] ++ ['_']

identifierChars :: [Char]
identifierChars =
  identifierLeadChars ++ ['0' .. '9']

-- | Represents an encoded string value, as opposed to a raw (unencoded) string,
-- which we represent as a plain 'Text'.
newtype Encoded =
  Encoded { encoded :: Text }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance Arbitrary Encoded where
  arbitrary = Encoded . Text.replace "{" "{ " . Text.pack <$> QC.listOf arbitrary
  shrink (Encoded e) =
    map (Encoded . Text.pack) . filter (not . null) $ shrink $ Text.unpack e

-- | A template consists of an optional parent template (specified in the
-- source using the @{% extends %}@ construct), and a body statement.
data Template =
  Template
    { templateParent :: !(Maybe Text)
    , templateBody :: !Statement
    }
    deriving (Show, Eq)

-- | A block represents a section of a template that can be overridden in
-- derived templates ("template inheritance").
data Block =
  Block
    { blockBody :: !Statement
    , blockScoped :: !Scoped
    , blockRequired :: !Required
    }
    deriving (Show, Eq, Ord)

data SetTarget
  = SetVar !Identifier
  | SetMutable !Identifier !Identifier
  deriving (Show, Eq, Ord)

-- | A statement in the template language.
data Statement
  = -- | Statement tagged with a source position
    PositionedS !SourcePosition !Statement
  | -- | Bare text written in the template, outside of any curly braces
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
      !SetTarget -- variable name
      !Expr -- value
  | -- | @{% set name %}body{% endset %}@
    SetBlockS
      !SetTarget -- variable name
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
      !(Maybe [(Identifier, Maybe Identifier)]) -- [ (imported name, local name) ]
      !IncludeMissingPolicy !IncludeContextPolicy
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
  deriving (Show, Eq, Ord)

data IncludeMissingPolicy
  = RequireMissing
  | IgnoreMissing
  deriving (Show, Eq, Ord, Enum, Bounded)

data IncludeContextPolicy
  = WithContext
  | WithoutContext
  deriving (Show, Eq, Ord, Enum, Bounded)

escapeComment :: Statement -> Statement
escapeComment (CommentS txt) =
  case Text.splitOn "#}" txt of
    [] -> GroupS []
    [x] -> CommentS x
    xs -> GroupS . intercalate [CommentS "#", CommentS "}"] . map ((:[]) . CommentS) $ xs
escapeComment x = x

instance Arbitrary Statement where
  arbitrary = arbitraryStatement mempty
  shrink (PositionedS pos s) =
    PositionedS pos <$> shrink s ++
    [s]
  shrink (GroupS xs) = map GroupS $ shrink xs
  shrink (ImmediateS txt) = map ImmediateS $ shrink txt
  shrink (InterpolationS e) = map InterpolationS $ shrink e
  shrink (CommentS txt) = map (escapeComment . CommentS . Text.pack) $ shrink $ Text.unpack txt
  shrink (ForS keyMay val iteree condMay recur body elseBranchMay) =
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> pure recur <*> pure body <*> shrink elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> pure recur <*> shrink body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> pure condMay <*> shrink recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> pure iteree <*> shrink condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
    (ForS <$> pure keyMay <*> pure val <*> shrink iteree <*> pure condMay <*> pure recur <*> pure body <*> pure elseBranchMay) ++
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
    [body]
  shrink (FilterS name args kwargs body) =
    (FilterS <$> pure name <*> pure args <*> pure kwargs <*> shrink body) ++
    (FilterS <$> pure name <*> pure args <*> shrink kwargs <*> shrink body) ++
    (FilterS <$> pure name <*> shrink args <*> pure kwargs <*> pure body) ++
    [body]
  shrink (SetS name expr) =
    (SetS <$> pure name <*> shrink expr)
  shrink (SetBlockS name body filterMay) =
    (SetBlockS <$> pure name <*> pure body <*> shrink filterMay) ++
    (SetBlockS <$> pure name <*> shrink body <*> pure filterMay)
  shrink (IncludeS name m c) =
    (IncludeS <$> pure name <*> pure m <*> shrink c) ++
    (IncludeS <$> pure name <*> shrink m <*> pure c)
  shrink (ImportS name lname imports m c) =
    (ImportS <$> pure name <*> pure lname <*> pure imports <*> pure m <*> shrink c) ++
    (ImportS <$> pure name <*> pure lname <*> pure imports <*> shrink m <*> pure c) ++
    (ImportS <$> pure name <*> pure lname <*> shrink imports <*> pure m <*> pure c) ++
    (ImportS <$> pure name <*> shrink lname <*> pure imports <*> pure m <*> pure c)
  shrink (BlockS name (Block b s r)) =
    (BlockS <$> pure name <*> (Block <$> pure b <*> pure s <*> shrink r)) ++
    (BlockS <$> pure name <*> (Block <$> pure b <*> pure s <*> shrink r)) ++
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
        , escapeComment . CommentS . Text.strip . Text.pack <$> arbitrary

        , do
            let fuel' = fuel `div` 6
            keyNameMaybe <- arbitrary
            valName <- arbitrary
            iteree <- QC.resize fuel' $ arbitraryExpr defined
            let defined' = mempty <> Set.singleton valName <> Set.fromList (maybeToList keyNameMaybe) <> Set.singleton "loop"
            filterMay <- QC.oneof [ pure Nothing, QC.resize fuel' $ Just <$> arbitraryExpr defined ]
            body <- QC.resize fuel' $ arbitraryStatement defined'
            elseBody <- QC.resize fuel' $ QC.oneof [ pure Nothing, Just <$> arbitraryStatement defined' ]
            recursive <- arbitrary

            pure $ ForS keyNameMaybe valName iteree filterMay recursive body elseBody

        , IfS <$> QC.resize (fuel `div` 3) (arbitraryExpr defined)
              <*> QC.resize (fuel `div` 3) (arbitraryStatement defined)
              <*> QC.resize (fuel `div` 3) (QC.oneof [ pure Nothing, Just <$> arbitraryStatement defined ])

        , do
            args <- QC.resize (fuel `div` 2) $
                      QC.listOf ((,) <$> arbitrary <*> QC.oneof [ pure Nothing, Just <$> arbitraryExpr mempty ])
            let defined' = Set.fromList (map fst args)
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
        , SetS <$> QC.frequency
                      [ (100, SetVar <$> arbitrary)
                      , (5, SetMutable <$> arbitrary <*> arbitrary)
                      ]
               <*> QC.resize (fuel `div` 2) (arbitraryExpr defined)
        , SetBlockS <$> QC.frequency
                          [ (100, SetVar <$> arbitrary)
                          , (5, SetMutable <$> arbitrary <*> arbitrary)
                          ]
                    <*> QC.resize (fuel `div` 2) (arbitraryStatement defined)
                    <*> QC.resize (fuel `div` 2) (QC.oneof [ pure Nothing, Just <$> arbitraryExpr defined ])

        -- , IncludeS <$> arbitraryExpr defined
        -- , BlockS <$> arbitrary
        --          <*> QC.resize (fuel `div` 2) (arbitraryStatement defined)
        --          <*> arbitrary
        --          <*> arbitrary

        , do
            vars <- QC.listOf1 ((,) <$> arbitrary <*> QC.resize (fuel `div` 4) (arbitraryExpr defined))
            let defined' = mempty <> Set.fromList (map fst vars)
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

-- | An expression. Expressions can occur in interpolations (@{{ ... }}@), and
-- in various places inside statements.
data Expr
  = PositionedE !SourcePosition !Expr
  | NoneE
  | BoolE !Bool
  | StringLitE !Text
  | IntLitE !Integer
  | FloatLitE !Double
  | StatementE !Statement
  | ListE !(Vector Expr)
  | DictE ![(Expr, Expr)]
    -- | @UnaryE op rhs
  | UnaryE !UnaryOperator !Expr
    -- | @BinaryE op lhs rhs
  | BinaryE !BinaryOperator !Expr !Expr
    -- | @SliceE slicee start length
  | SliceE !Expr !(Maybe Expr) !(Maybe Expr)
    -- | @DotE lhs rhs
  | DotE !Expr !Identifier
    -- | @IsE scrutinee test args kwargs@
  | IsE !Expr !Expr ![Expr] ![(Identifier, Expr)]
    -- | @CallE callee args kwargs@
  | CallE !Expr ![Expr] ![(Identifier, Expr)]
    -- | @FilterE arg0 filter args kwargs@
  | FilterE !Expr !Expr ![Expr] ![(Identifier, Expr)]
    -- | @TernaryE cond yes no@
  | TernaryE !Expr !Expr !Expr
  | VarE !Identifier
  deriving (Show, Eq, Ord)

pattern TrueE :: Expr
pattern TrueE = BoolE True

pattern FalseE :: Expr
pattern FalseE = BoolE False

instance Arbitrary Expr where
  arbitrary = arbitraryExpr mempty
  shrink (PositionedE pos e) =
    PositionedE pos <$> shrink e ++
    [e]
  shrink (StringLitE txt) = map (StringLitE . Text.pack) $ shrink $ Text.unpack txt
  shrink (IntLitE i) = IntLitE <$> shrink i
  shrink (FloatLitE i) = FloatLitE <$> shrink i
  shrink (StatementE s) = StatementE <$> shrink s
  shrink (ListE v) =
    case V.uncons v of
      Nothing -> pure NoneE
      Just (x, xs) | V.null xs ->
        (ListE . V.singleton <$> shrink x) ++ [x]
      _ ->
        ListE . V.fromList <$> shrink (V.toList v)
  shrink (DictE xs) = DictE <$> shrink xs
  shrink (UnaryE op e) =
    (UnaryE <$> pure op <*> shrink e) ++
    [e]
  shrink (DotE a b) =
    (DotE <$> pure a <*> shrink b) ++
    (DotE <$> shrink a <*> pure b) ++
    [a]
  shrink (BinaryE op a b) =
    (BinaryE <$> pure op <*> shrink a <*> pure b) ++
    (BinaryE <$> pure op <*> pure a <*> shrink b) ++
    [a, b]
  shrink (SliceE slicee startMay endMay) =
    (SliceE <$> pure slicee <*> pure startMay <*> shrink endMay) ++ 
    (SliceE <$> pure slicee <*> shrink startMay <*> pure endMay) ++ 
    (SliceE <$> shrink slicee <*> pure startMay <*> pure endMay) ++ 
    maybeToList startMay ++
    maybeToList endMay ++
    [slicee]
  shrink (IsE a b args kwargs) =
    (IsE <$> pure a <*> pure b <*> pure args <*> shrink kwargs) ++
    (IsE <$> pure a <*> pure b <*> shrink args <*> pure kwargs) ++
    (IsE <$> pure a <*> shrink b <*> pure args <*> pure kwargs) ++
    (IsE <$> shrink a <*> pure b <*> pure args <*> pure kwargs) ++
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
          , (100, ListE . V.fromList <$> fuelledList (arbitraryExpr defined))
          , (100, DictE <$> fuelledList ((,) <$> arbitraryExpr defined <*> arbitraryExpr defined))
          , (90, QC.resize (max 0 $ fuel `div` 2 - 1) $
              BinaryE <$> arbitrary
                      <*> arbitraryExpr defined
                      <*> arbitraryExpr defined
            )

          , (10, QC.resize (max 0 $ fuel `div` 2 - 1) $
              SliceE <$> arbitraryExpr defined
                     <*> (fmap IntLitE <$> arbitrary)
                     <*> (fmap IntLitE <$> arbitrary)
            )
          , (10, QC.resize (max 0 $ fuel `div` 2 - 1) $
              DotE <$> arbitraryExpr defined
                   <*> arbitrary
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
  if fuel < 1 then
    pure []
  else do
    s <- QC.chooseInt (1, fuel)
    x <- QC.resize s subGen
    xs <- QC.resize (max 0 $ fuel - s - 10) (fuelledList subGen)
    pure $ x : xs

mapS :: Monoid a => (Statement -> a) -> (Expr -> a) -> Statement -> a
mapS fS fE stmt =
  fS stmt <> go stmt
  where
    go (PositionedS _ s) = mapS fS fE s
    go (GroupS xs) = mconcat $ map (mapS fS fE) xs
    go (InterpolationS e) = mapE fE fS e
    go (ForS _ _ i condMay _ body elseMay) =
        mapE fE fS i
          <> maybe mempty (mapE fE fS) condMay
          <> mapS fS fE body
          <> maybe mempty (mapS fS fE) elseMay
    go (IfS cond yes noMay) =
      mapE fE fS cond
        <> mapS fS fE yes
        <> maybe mempty (mapS fS fE) noMay
    go (MacroS _name args body) =
      mconcat [ maybe mempty (mapE fE fS) d | (_, d) <- args]
        <> mapS fS fE body
    go (CallS _name args kwargs body) =
        mconcat (map (mapE fE fS) args)
          <> mconcat [ mapE fE fS v | (_, v) <- kwargs]
          <> mapS fS fE body
    go (FilterS _name args kwargs body) =
        mconcat (map (mapE fE fS) args)
          <> mconcat [ mapE fE fS v | (_, v) <- kwargs]
          <> mapS fS fE body
    go (SetS _name val) =
      mapE fE fS val
    go (SetBlockS _name body filterMay) =
      mapS fS fE body <> maybe mempty (mapE fE fS) filterMay
    go (IncludeS includee _mp _cp) =
      mapE fE fS includee
    go (ImportS importee _lname _imports _mp _cp) =
      mapE fE fS importee
    go (BlockS _name (Block body _s _r)) =
      mapS fS fE body
    go (WithS defs body) =
      mconcat [ mapE fE fS e | (_, e) <- defs ]
        <> mapS fS fE body
    go _ = mempty

mapE :: Monoid a => (Expr -> a) -> (Statement -> a) -> Expr -> a
mapE fE fS expr =
  fE expr <> go expr
  where
    go (StatementE s) = mapS fS fE s
    go (ListE xs) = mconcat $ map (mapE fE fS) (V.toList xs)
    go (DictE items) =
      mconcat [ mapE fE fS k <> mapE fE fS v | (k, v) <- items ]
    go (UnaryE _op e) = mapE fE fS e
    go (BinaryE _op a b) = mapE fE fS a <> mapE fE fS b
    go (SliceE slicee startMay lengthMay) =
        mapE fE fS slicee
          <> maybe mempty (mapE fE fS) startMay
          <> maybe mempty (mapE fE fS) lengthMay
    go (DotE e _i) = mapE fE fS e
    go (IsE scrutinee test args kwargs) =
        mapE fE fS scrutinee
          <> mapE fE fS test
          <> mconcat (map (mapE fE fS) args)
          <> mconcat [ mapE fE fS e | (_, e) <- kwargs ]
    go (CallE callee args kwargs) =
        mapE fE fS callee
          <> mconcat (map (mapE fE fS) args)
          <> mconcat [ mapE fE fS e | (_, e) <- kwargs ]
    go (FilterE arg0 f args kwargs) =
        mapE fE fS arg0
          <> mapE fE fS f
          <> mconcat (map (mapE fE fS) args)
          <> mconcat [ mapE fE fS e | (_, e) <- kwargs ]
    go (TernaryE cond yes no) =
        mapE fE fS cond
          <> mapE fE fS yes
          <> mapE fE fS no
    go _ = mempty

omapS :: (Statement -> Statement) -> (Expr -> Expr) -> Statement -> Statement
omapS fS fE stmt = go (fS stmt)
  where
    go (PositionedS pos s) = PositionedS pos $ omapS fS fE s
    go (GroupS xs) = GroupS (map (omapS fS fE) xs)
    go (InterpolationS e) = InterpolationS (omapE fE fS e)
    go (ForS k v i condMay rec body elseMay) =
      ForS
        k v
        (omapE fE fS i)
        (omapE fE fS <$> condMay)
        rec
        (omapS fS fE body)
        (omapS fS fE <$> elseMay)
    go (IfS cond yes noMay) =
      IfS (omapE fE fS cond) (omapS fS fE yes) (omapS fS fE <$> noMay)
    go (MacroS name args body) =
      MacroS name [(n, omapE fE fS <$> d) | (n, d) <- args] (omapS fS fE body)
    go (CallS name args kwargs body) =
      CallS
        name
        (omapE fE fS <$> args)
        [(n, omapE fE fS v) | (n, v) <- kwargs]
        (omapS fS fE body)
    go (FilterS name args kwargs body) =
      FilterS
        name
        (omapE fE fS <$> args)
        [(n, omapE fE fS v) | (n, v) <- kwargs]
        (omapS fS fE body)
    go (SetS name val) =
      SetS name (omapE fE fS val)
    go (SetBlockS name body filterMay) =
      SetBlockS name (omapS fS fE body) (omapE fE fS <$> filterMay)
    go (IncludeS includee mp cp) =
      IncludeS (omapE fE fS includee) mp cp
    go (ImportS importee lname imports mp cp) =
      ImportS (omapE fE fS importee) lname imports mp cp
    go (BlockS name (Block body s r)) =
      BlockS name (Block (omapS fS fE body) s r)
    go (WithS defs body) =
      WithS [ (n, omapE fE fS e) | (n, e) <- defs ] (omapS fS fE body)
    go s = s

omapE :: (Expr -> Expr) -> (Statement -> Statement) -> Expr -> Expr
omapE fE fS expr = go (fE expr)
  where
    go (StatementE s) = StatementE (omapS fS fE s)
    go (ListE xs) = ListE (fmap (omapE fE fS) xs)
    go (DictE items) =
      DictE [ (omapE fE fS k, omapE fE fS v) | (k, v) <- items ]
    go (UnaryE op e) = UnaryE op (omapE fE fS e)
    go (BinaryE op a b) = BinaryE op (omapE fE fS a) (omapE fE fS b)
    go (SliceE slicee startMay lengthMay) =
      SliceE
        (omapE fE fS slicee)
        (omapE fE fS <$> startMay)
        (omapE fE fS <$> lengthMay)
    go (DotE e i) = DotE (omapE fE fS e) i
    go (IsE scrutinee test args kwargs) =
      IsE
        (omapE fE fS scrutinee)
        (omapE fE fS test)
        (omapE fE fS <$> args)
        [ (n, omapE fE fS e) | (n, e) <- kwargs ]
    go (CallE callee args kwargs) =
      CallE
        (omapE fE fS callee)
        (omapE fE fS <$> args)
        [ (n, omapE fE fS e) | (n, e) <- kwargs ]
    go (FilterE arg0 f args kwargs) =
      FilterE
        (omapE fE fS arg0)
        (omapE fE fS f)
        (omapE fE fS <$> args)
        [ (n, omapE fE fS e) | (n, e) <- kwargs ]
    go (TernaryE cond yes no) =
      TernaryE
        (omapE fE fS cond)
        (omapE fE fS yes)
        (omapE fE fS no)
    go e = e

class Digestive a where
  digest :: a -> LBS.ByteString

instance Digestive Text where
  digest = LBS.fromStrict . encodeUtf8

instance Digestive Integer where
  digest = digest . Text.show

instance Digestive Double where
  digest = digest . Text.show

instance Digestive Statement where
  digest = digestStatement

instance Digestive Encoded where
  digest (Encoded e) = digest e

instance Digestive Identifier where
  digest (Identifier i) = digest i

instance Digestive a => Digestive (Maybe a) where
  digest Nothing = mempty
  digest (Just a) = digest a

instance (Digestive a, Digestive b) => Digestive (Either a b) where
  digest (Left x) = "l:" <> digest x
  digest (Right x) = "r:" <> digest x

instance Digestive Recursivity where
  digest Recursive = "r"
  digest NotRecursive = mempty

instance Digestive IncludeMissingPolicy where
  digest RequireMissing = "r"
  digest IgnoreMissing = mempty

instance Digestive IncludeContextPolicy where
  digest WithContext = "c"
  digest WithoutContext = mempty

instance Digestive Scoped where
  digest Scoped = "s"
  digest NotScoped = mempty

instance Digestive Required where
  digest Required = "r"
  digest Optional = mempty

instance Digestive a => Digestive [a] where
  digest = mconcat . intersperse "," . map digest

instance (Digestive a, Digestive b) => Digestive (a, b) where
  digest (x, y) = digest x <> ":" <> digest y

instance Digestive SetTarget where
  digest (SetVar ident) = digest ident
  digest (SetMutable id1 id2) = "@" <> digest id1 <> ":" <> digest id2

digestStatement :: Statement -> LBS.ByteString
digestStatement (PositionedS _ s) = digest s
digestStatement (ImmediateS e) = "i{" <> digest e <> "}"
digestStatement (InterpolationS expr) = "e{" <> digest expr <> "}"
digestStatement CommentS {} = mempty
digestStatement (ForS keyMay var iteree cond rec body elseMay) =
  "f:" <>
     digest keyMay <> ":" <>
     digest var <> ":" <>
     digest iteree <> ":" <>
     digest cond <> ":" <>
     digest rec <> ":" <>
     digest body <> ":" <>
     digest elseMay
digestStatement (IfS cond yes noMay) =
  "?:" <> digest cond <> ":" <> digest yes <> ":" <> digest noMay
digestStatement (MacroS name args body) =
  "m:" <> digest name <> ":" <> digest args <> ":" <> digest body
digestStatement (CallS callee args kwargs body) =
  "c:" <> digest callee <> ":" <> digest args <> ":" <> digest kwargs <> ":" <> digest body
digestStatement (FilterS name args kwargs body) =
  "|:" <> digest name <> ":" <> digest args <> ":" <> digest kwargs <> ":" <> digest body
digestStatement (SetS target value) =
  "=:" <> digest target <> ":" <> digest value
digestStatement (SetBlockS target body filterMay) =
  "S:" <> digest target <> ":" <> digest body <> ":" <> digest filterMay
digestStatement (IncludeS includee missing context) =
  "I:" <> digest includee <> ":" <> digest missing <> digest context
digestStatement (ImportS filename localname imports missing context) =
  "i:" <> digest filename <> ":" <> digest localname <> ":" <> digest imports <> ":" <> digest missing <> digest context
digestStatement (BlockS name (Block body scoped required)) =
  "b:" <> digest name <> ":" <> digest body <> ":" <> digest scoped <> digest required
digestStatement (WithS defs body) =
  "w:" <> digest defs <> ":" <> digest body
digestStatement (GroupS xs) =
  mconcat . map digest $ xs

instance Digestive UnaryOperator where
  digest UnopNot = "!"
  digest UnopNegate = "-"

instance Digestive BinaryOperator where
  digest BinopPlus = "+"
  digest BinopMinus = "-"
  digest BinopDiv = "/"
  digest BinopIntDiv = "//"
  digest BinopMod = "%"
  digest BinopMul = "*"
  digest BinopPower = "^"
  digest BinopEqual = "="
  digest BinopNotEqual = "!="
  digest BinopGT = ">"
  digest BinopGTE = ">="
  digest BinopLT = "<"
  digest BinopLTE = "<="
  digest BinopAnd = "&"
  digest BinopOr = "|"
  digest BinopIn = "in"
  digest BinopIndex = "[]"
  digest BinopConcat = "~"

instance Digestive Expr where
  digest (PositionedE _ e) = digest e
  digest NoneE = mempty
  digest (BoolE True) = "!!"
  digest (BoolE False) = "!"
  digest (StringLitE t) = digest t
  digest (IntLitE i) = digest i
  digest (FloatLitE i) = digest i
  digest (StatementE s) = "s:" <> digest s
  digest (ListE xs) = "l:" <> (mconcat . V.toList . V.map digest $ xs)
  digest (DictE xs) = "d:" <> digest xs
  digest (UnaryE op e) = digest op <> ":" <> digest e
  digest (BinaryE op a b) = digest op <> ":" <> digest a <> ":" <> digest b
  digest (SliceE slicee leftMay rightMay) = "slice:" <> digest slicee <> ":" <> digest leftMay <> ":" <> digest rightMay
  digest (DotE a b) = digest a <> "." <> digest b
  digest (IsE scrutinee test args kwargs) = "is:" <> digest scrutinee <> ":" <> digest test <> ":" <> digest args <> ":" <> digest kwargs
  digest (CallE callee args kwargs) = "c:" <> digest callee <> ":" <> digest args <> ":" <> digest kwargs
  digest (FilterE arg0 f args kwargs) = "|:" <> digest arg0 <> ":" <> digest f <> ":" <> digest args <> ":" <> digest kwargs
  digest (TernaryE cond yes no) = "?:" <> digest cond <> ":" <> digest yes <> ":" <> digest no
  digest (VarE ident) = digest ident
