module Language.Ginger.TypeSystem
where

import Language.Ginger.AST
import Language.Ginger.Value

import Control.Monad (void, (>=>))
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

type TyVarName = Text

data Ty
  = NoneT
  | BoolT
  | StringT
  | EncodedT
  | BytesT
  | IntT
  | FloatT
  | ListT Ty
    -- ^ A variable-length list; the list element type must be a supertype of
    -- all list elements (worst case, we may need to use 'AnyT').
  | TupleT (Vector Ty)
    -- ^ Also a list, but the number and types of the elements are known
    -- statically.
  | DictT Ty
  | ObjectT (Map Identifier Ty)
  | NativeObjectT
  | ProcedureT ProcedureTy
  | TestT MetaFuncTy
  | FilterT MetaFuncTy

  | AnyT
  | EitherT Ty Ty
  | VarT TyVarName
  deriving (Show, Eq)

(\/) :: Ty -> Ty -> Ty
(\/) = EitherT
infixr \/

data ArgRequired
  = OptionalArg
  | RequiredArg
  deriving (Show, Eq)

data ProcedureTy =
  ProcedureTy
    { procedureArgsT :: [ (Ty, ArgRequired) ]
    , procedureReturnT :: Ty
    }
    deriving (Show, Eq)

data MetaFuncTy =
  MetaFuncTy
    { metaFuncTargetT :: Ty
    , metaFuncArgsT :: [ (Ty, ArgRequired) ]
    , metaFuncReturnT :: Ty
    }
    deriving (Show, Eq)

data FieldsTy k
  = NoFieldsT
    -- ^ Object has no fields
  | KnownFieldsT (Map k Ty)
    -- ^ Object fields are statically known
  | UnknownFieldsT
    -- ^ Object has fields, but they are not statically known
  deriving (Show, Eq)

class PureTyped a where
  typeOf :: a -> Ty

instance PureTyped (Value m) where
  typeOf NoneV = NoneT
  typeOf BoolV {} = BoolT
  typeOf IntV {} = IntT
  typeOf FloatV {} = FloatT
  typeOf StringV {} = StringT
  typeOf EncodedV {} = EncodedT
  typeOf BytesV {} = BytesT
  typeOf ScalarV {} = AnyT
  typeOf (ListV xs) = TupleT (fmap typeOf xs)
  typeOf (DictV m) = ObjectT (fmap typeOf m)
  typeOf NativeV {} = NativeObjectT
  typeOf (ProcedureV _) = ProcedureT (ProcedureTy [] AnyT)
  typeOf (TestV _) = TestT (MetaFuncTy AnyT [] BoolT)
  typeOf (FilterV _) = FilterT (MetaFuncTy AnyT [] AnyT)

isSubtypeOf :: Ty -> Ty -> Bool
isSubtypeOf _ AnyT = True
isSubtypeOf a b | a == b = True
isSubtypeOf (TupleT xs) (TupleT ys) =
  (V.length xs == V.length ys) &&
  (V.and $ V.zipWith isSubtypeOf xs ys)
isSubtypeOf (ListT x) (TupleT ys) =
  V.all (x `isSubtypeOf`) ys
isSubtypeOf (TupleT xs) (ListT y) =
  V.all (`isSubtypeOf` y) xs
isSubtypeOf (ListT x) (ListT y) =
  isSubtypeOf x y
isSubtypeOf a (EitherT l r) = isSubtypeOf a l || isSubtypeOf a r
isSubtypeOf _ _ = False

(<:) :: Ty -> Ty -> Bool
(<:) = isSubtypeOf
infix 4 <:

(>:) :: Ty -> Ty -> Bool
(>:) = flip isSubtypeOf
infix 4 >:

type TyEnv = Map Identifier Ty

type TyChk = ExceptT TypeError (State TyEnv)

data TypeError
  = TypeErrorUnexpected Ty Ty
  | TypeErrorNotInScope Identifier
  deriving (Show)

class Typed a where
  tyCheck :: a -> TyChk Ty

instance Typed (Value m) where
  tyCheck = pure . typeOf

expectTy :: Ty -> Ty -> TyChk Ty
expectTy expected actual
  | actual `isSubtypeOf` expected
  = pure actual
  | otherwise
  = throwError $ TypeErrorUnexpected actual expected

instance Typed Expr where
  tyCheck (PositionedE _ e) = tyCheck e
  tyCheck NoneE = pure NoneT
  tyCheck BoolE {} = pure BoolT
  tyCheck StringLitE {} = pure StringT
  tyCheck IntLitE {} = pure IntT
  tyCheck FloatLitE {} = pure FloatT
  tyCheck (StatementE s) = tyCheck s
  tyCheck (ListE xs) = do
    elemTys <- V.mapM tyCheck xs
    case V.uncons elemTys of
      Nothing -> pure $ ListT AnyT
      Just (x, xs) | V.all (== x) xs -> pure $ ListT x
      _ -> pure $ TupleT elemTys
  tyCheck (DictE {}) = pure $ DictT AnyT
  tyCheck (UnaryE UnopNot a) =
    tyCheck a >>= expectTy BoolT
  tyCheck (UnaryE UnopNegate a) =
    tyCheck a >>= expectTy (EitherT IntT FloatT)

  tyCheck (BinaryE BinopPlus a b) = do
    aT <- tyCheck a >>= expectTy (EitherT IntT FloatT)
    bT <- tyCheck b >>= expectTy (EitherT IntT FloatT)
    if aT `isSubtypeOf` IntT && bT `isSubtypeOf` IntT then
      pure IntT
    else
      pure FloatT
  tyCheck (BinaryE BinopMinus a b) = do
    aT <- tyCheck a >>= expectTy (EitherT IntT FloatT)
    bT <- tyCheck b >>= expectTy (EitherT IntT FloatT)
    if aT `isSubtypeOf` IntT && bT `isSubtypeOf` IntT then
      pure IntT
    else
      pure FloatT
  tyCheck (BinaryE BinopDiv a b) = do
    aT <- tyCheck a >>= expectTy (EitherT IntT FloatT)
    bT <- tyCheck b >>= expectTy (EitherT IntT FloatT)
    if aT `isSubtypeOf` IntT && bT `isSubtypeOf` IntT then
      pure IntT
    else
      pure FloatT
  tyCheck (BinaryE BinopIntDiv a b) = do
    aT <- tyCheck a >>= expectTy IntT
    bT <- tyCheck b >>= expectTy IntT
    pure IntT
  tyCheck (BinaryE BinopMod a b) = do
    aT <- tyCheck a >>= expectTy IntT
    bT <- tyCheck b >>= expectTy IntT
    pure IntT
  tyCheck (BinaryE BinopMul a b) = do
    aT <- tyCheck a >>= expectTy (EitherT IntT FloatT)
    bT <- tyCheck b >>= expectTy (EitherT IntT FloatT)
    if aT `isSubtypeOf` IntT && bT `isSubtypeOf` IntT then
      pure IntT
    else
      pure FloatT
  tyCheck (BinaryE BinopPower a b) = do
    aT <- tyCheck a >>= expectTy (EitherT IntT FloatT)
    bT <- tyCheck b >>= expectTy (EitherT IntT FloatT)
    if aT `isSubtypeOf` IntT && bT `isSubtypeOf` IntT then
      pure IntT
    else
      pure FloatT
  tyCheck (BinaryE BinopEqual a b) = pure BoolT
  tyCheck (BinaryE BinopNotEqual a b) = pure BoolT
  tyCheck (BinaryE BinopGT a b) = do
    void $ tyCheck a >>= expectTy (EitherT IntT FloatT)
    void $ tyCheck b >>= expectTy (EitherT IntT FloatT)
    pure BoolT
  tyCheck (BinaryE BinopGTE a b) = do
    void $ tyCheck a >>= expectTy (EitherT IntT FloatT)
    void $ tyCheck b >>= expectTy (EitherT IntT FloatT)
    pure BoolT
  tyCheck (BinaryE BinopLT a b) = do
    void $ tyCheck a >>= expectTy (EitherT IntT FloatT)
    void $ tyCheck b >>= expectTy (EitherT IntT FloatT)
    pure BoolT
  tyCheck (BinaryE BinopLTE a b) = do
    void $ tyCheck a >>= expectTy (EitherT IntT FloatT)
    void $ tyCheck b >>= expectTy (EitherT IntT FloatT)
    pure BoolT
  tyCheck (BinaryE BinopAnd a b) = do
    void $ tyCheck a >>= expectTy BoolT
    void $ tyCheck b >>= expectTy BoolT
    pure BoolT
  tyCheck (BinaryE BinopOr a b) = do
    void $ tyCheck a >>= expectTy BoolT
    void $ tyCheck b >>= expectTy BoolT
    pure BoolT
  tyCheck (BinaryE BinopIn a b) = do
    bTy <- tyCheck b >>= expectTy (ListT AnyT \/ DictT AnyT)
    if DictT AnyT <: bTy then do
      tyCheck a >>= expectTy (NoneT \/ BoolT \/ IntT \/ FloatT \/ StringT \/ EncodedT \/ BytesT)
      pure BoolT
    else do
      tyCheck a >>= expectTy IntT
      pure BoolT
  tyCheck (BinaryE BinopIndex a b) = do
    bTy <- tyCheck b >>= expectTy (ListT AnyT \/ DictT AnyT)
    if DictT AnyT <: bTy then do
      tyCheck a >>= expectTy (NoneT \/ BoolT \/ IntT \/ FloatT \/ StringT \/ EncodedT \/ BytesT)
      case bTy of
        DictT a -> pure a
        _ -> pure AnyT
    else do
      tyCheck a >>= expectTy IntT
      case bTy of
        ListT a -> pure a
        _ -> pure AnyT
  tyCheck (BinaryE BinopConcat a b) = do
    aTy <- tyCheck a >>= expectTy (NoneT \/ BoolT \/ IntT \/ FloatT \/ StringT \/ EncodedT \/ BytesT)
    bTy <- tyCheck a >>= expectTy (NoneT \/ BoolT \/ IntT \/ FloatT \/ StringT \/ EncodedT \/ BytesT)
    if aTy >: StringT && bTy >: StringT then
      pure StringT
    else if aTy >: BytesT && bTy >: BytesT then
      pure BytesT
    else if aTy >: EncodedT || bTy >: EncodedT then
      pure EncodedT
    else if aTy <: NoneT then
      pure bTy
    else if bTy <: NoneT then
      pure aTy
    else
      pure StringT
  tyCheck (SliceE slicee startMay endMay) = do
    mapM_ (tyCheck >=> expectTy IntT) startMay
    mapM_ (tyCheck >=> expectTy IntT) endMay
    sliceeT <- tyCheck slicee >>= expectTy (ListT AnyT \/ StringT \/ BytesT \/ EncodedT)
    pure sliceeT
  tyCheck (DotE e _) = do
    tyCheck e >>= expectTy (DictT AnyT)
    pure AnyT
  tyCheck (IsE scrutinee testE args kwargs) = do
    testT <- tyCheck testE >>=
                expectTy
                  ( ProcedureT (ProcedureTy [] BoolT)
                  \/ TestT (MetaFuncTy AnyT [] BoolT)
                  )
    pure BoolT
  tyCheck (CallE callee args kwargs) = do
    calleeT <- tyCheck callee >>= expectTy (ProcedureT $ ProcedureTy [] AnyT)
    pure AnyT -- TODO
  tyCheck (FilterE filtree filterE args kwargs) = do
    filterT <- tyCheck filterE >>=
                expectTy
                  ( ProcedureT (ProcedureTy [] AnyT)
                  \/ FilterT (MetaFuncTy AnyT [] AnyT)
                  )
    pure AnyT -- TODO
  tyCheck (TernaryE cond a b) = do
    void $ tyCheck cond >>= expectTy BoolT
    aT <- tyCheck a
    bT <- tyCheck b
    pure $ aT \/ bT
  tyCheck (VarE name) = do
    tyMay <- gets $ Map.lookup name
    maybe
      (throwError $ TypeErrorNotInScope name)
      pure
      tyMay

scoped :: MonadState s m
       => m a
       -> m a
scoped action = do
  s <- get
  retval <- action
  modify $ const s
  return retval

setVarTy :: Identifier
         -> Ty
         -> TyChk ()
setVarTy name ty =
  modify $ Map.insert name ty

instance Typed Statement where
  tyCheck (PositionedS _ s) = tyCheck s
  tyCheck CommentS {} = pure NoneT
  tyCheck ImmediateS {} = pure EncodedT
  tyCheck (InterpolationS e) = tyCheck e
  tyCheck (ForS keyMay elem iteree recursivity body elseMay) = do
    itereeT <- tyCheck iteree >>= expectTy (ListTy AnyT \/ DictT AnyT)
    scoped $ do
      mapM (\k -> setVarTy k (VarT "#loopKey"))
      setVarTy elem (VarT "#loopVal")
      setVarTy "loop"
        ( ObjectT $ Map.fromList
          [ ("index", IntT)
          , ("index0", IntT)
          , ("revindex", IntT)
          , ("revindex0", IntT)
          , ("first", BoolT)
          , ("last", BoolT)
          , ("length", BoolT)
          , ("cycle", ProcedureT (ProcedureTy [] AnyT)
          , ("depth", IntT)
          , ("depth0", IntT)
          , ("previtem", AnyT)
          , ("nextitem", AnyT)
          , ("changed", ProcedureT (ProcedureTy [(AnyT, RequiredArg)] BoolTy))
          , ("__call__",
                if is recursivity then
                  ProcedureT (ProcedureTy [] AnyT)
                else
                  NoneT
            )
          ]
        )
      bodyT <- tyCheck body
      maybe (pure bodyT) (\e -> (bodyT \/) <$> tyCheck e) elseMay
  tyCheck (IfS cond yes noMay) = do
    void $ tyCheck cond >>= expectTy BoolT
    yesT <- tyCheck yes
    maybe (pure yesT) (\no -> (yesT \/) <$> tyCheck no) noMay
  tyCheck (MacroS name args body) = do
    bodyT <- tyCheck body
    setVarTy name (ProcedureT (ProcedureTy [] bodyT))
    pure NoneT

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
  deriving (Show, Eq)

