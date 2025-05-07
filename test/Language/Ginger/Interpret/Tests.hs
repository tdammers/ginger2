{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ginger.Interpret.Tests
where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word16, Word32, Word64)
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Monoid (Any (..))
import Data.Either (isRight)

import Language.Ginger.AST
import Language.Ginger.Interpret
import Language.Ginger.RuntimeError
import Language.Ginger.Value

tests :: TestTree
tests = testGroup "Language.Ginger.Interpret"
  [ testGroup "misc"
    [ testProperty "setVar lookupVar" prop_setVarLookupVar
    , testProperty "scoped vars disappear outside" prop_scopedVarsDisappear
    , testProperty "no bottoms in expression eval" (prop_noBottoms @Expr)
    , testProperty "no bottoms in statement eval" (prop_noBottoms @Statement)
    ]
  , testGroup "stringify"
    [ testProperty "string stringifies to self" prop_stringifyString
    , testProperty "None stringifies to empty" prop_stringifyNone
    , testProperty "Integer stringifies to show instance" (prop_stringifyShow @Integer Proxy)
    , testProperty "Int stringifies to show instance" (prop_stringifyShow @Int Proxy)
    , testProperty "Int8 stringifies to show instance" (prop_stringifyShow @Int8 Proxy)
    , testProperty "Int16 stringifies to show instance" (prop_stringifyShow @Int16 Proxy)
    , testProperty "Int32 stringifies to show instance" (prop_stringifyShow @Int32 Proxy)
    , testProperty "Int64 stringifies to show instance" (prop_stringifyShow @Int64 Proxy)
    , testProperty "Word stringifies to show instance" (prop_stringifyShow @Word Proxy)
    , testProperty "Word8 stringifies to show instance" (prop_stringifyShow @Word8 Proxy)
    , testProperty "Word16 stringifies to show instance" (prop_stringifyShow @Word16 Proxy)
    , testProperty "Word32 stringifies to show instance" (prop_stringifyShow @Word32 Proxy)
    , testProperty "Word64 stringifies to show instance" (prop_stringifyShow @Word64 Proxy)
    , testProperty "Double stringifies to show instance" (prop_stringifyShow @Double Proxy)
    -- Not testing this for Float, because converting a Float to a Double
    -- doesn't always give the same results as @show@ing the Float directly.
    -- , testProperty "Float stringifies to show instance" (prop_stringifyShow @Float Proxy)
    ]
  , testGroup "Expr"
    [ testGroup "literals"
      [ testProperty "None literal" (prop_literal (\() -> NoneE))
      , testProperty "Bool literal" (prop_literal BoolE)
      , testProperty "Integer literal" (prop_literal IntLitE)
      , testProperty "Double literal" (prop_literal FloatLitE)
      , testProperty "String literal" (prop_literalWith Text.pack StringLitE)
      , testProperty "List literal" (prop_literal (ListE . map IntLitE))
      , testProperty "Dict literal" (prop_literal (DictE . map (\(k, v) -> (IntLitE k, IntLitE v)) . Map.toList))
      ]
    , testGroup "BinaryE"
      [ testProperty "Integer addition" (prop_binop @Integer BinopPlus (+))
      , testProperty "Integer subtraction" (prop_binop @Integer BinopMinus (-))
      , testProperty "Integer multiplication" (prop_binop @Integer BinopMul (*))
      , testProperty "Integer division" (prop_binopCond @Integer Just justNonzero BinopIntDiv div)
      , testProperty "Integer modulo" (prop_binopCond @Integer Just justNonzero BinopMod mod)
      , testProperty "Integer power" (prop_binopCond @Integer @Integer justPositive justPositive BinopPower (^))

      , testProperty "Double addition" (prop_binop @Double BinopPlus (+))
      , testProperty "Double subtraction" (prop_binop @Double BinopMinus (-))
      , testProperty "Double multiplication" (prop_binop @Double BinopMul (*))
      , testProperty "Double division" (prop_binopCond @Double Just justNonzero BinopDiv (/))
      , testProperty "Double power" (prop_binopCond @Double justPositive Just BinopPower (**))

      , testProperty "Integer equal" (prop_binop @Integer BinopEqual (==))
      , testProperty "Integer not equal" (prop_binop @Integer BinopNotEqual (/=))
      , testProperty "Integer greater-than" (prop_binop @Integer BinopGT (>))
      , testProperty "Integer greater-than-equal" (prop_binop @Integer BinopGTE (>=))
      , testProperty "Integer less-than" (prop_binop @Integer BinopLT (<))
      , testProperty "Integer less-than-equal" (prop_binop @Integer BinopLTE (<=))

      , testProperty "Double equal" (prop_binop @Double BinopEqual (==))
      , testProperty "Double not equal" (prop_binop @Double BinopNotEqual (/=))
      , testProperty "Double greater-than" (prop_binop @Double BinopGT (>))
      , testProperty "Double greater-than-equal" (prop_binop @Double BinopGTE (>=))
      , testProperty "Double less-than" (prop_binop @Double BinopLT (<))
      , testProperty "Double less-than-equal" (prop_binop @Double BinopLTE (<=))

      , testProperty "Boolean and" (prop_binop @Bool BinopAnd (&&))
      , testProperty "Boolean or" (prop_binop @Bool BinopOr (||))

      , testProperty "String equal" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopEqual (==))
      , testProperty "String not equal" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopNotEqual (/=))
      , testProperty "String greater-than" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopGT (>))
      , testProperty "String greater-than-equal" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopGTE (>=))
      , testProperty "String less-than" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopLT (<))
      , testProperty "String less-than-equal" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopLTE (<=))

      , testProperty "List membership (Word8)" (prop_binop @Word8 @[Word8] BinopIn elem)
      , testProperty "Dict membership (Word8)" (prop_binop @Word8 @(Map Word8 Word8) BinopIn (Map.member))
      , testProperty "List index (Word8)" (prop_binop @[Word8] @Word8 BinopIndex (flip $ safeAt . fromIntegral))
      , testProperty "Dict index (Word8)" (prop_binop @(Map Word8 Word8) @Word8 BinopIndex (flip Map.lookup))
      , testProperty "List index (Word8/Integer)" (prop_binop @[Integer] @Word8 BinopIndex (flip $ safeAt . fromIntegral))
      , testProperty "Dict index (Word8/Integer)" (prop_binop @(Map Word8 Integer) @Word8 BinopIndex (flip Map.lookup))

      , testProperty "Bytes concatenation" (prop_binopCond @ByteString (Just . BS.pack) (Just . BS.pack) BinopConcat (<>))
      , testProperty "String concatenation" (prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopConcat (<>))
      ]
      , testGroup "CallE"
        [ testProperty "Native nullary" prop_nativeNullary
        , testProperty "Native identity" prop_nativeIdentity
        , testProperty "User nullary" prop_userNullary
        ]
      , testProperty "TernaryE" prop_ternary
      , testGroup "VarE"
        [ testProperty "existing variable" prop_var
        , testProperty "nonexisting variable" prop_varNeg
        ]
    ]
  , testGroup "Statement"
    [ testProperty "Immediate statement outputs itself" prop_immediateStatementOutput
    , testProperty "Interpolation statement outputs its argument" prop_interpolationStatementOutput
    , testProperty "Comment statement outputs None" prop_commentStatementOutput
    , testProperty "IfS outputs the correct branch" prop_ifStatementOutput
    , testGroup "ForS"
      [ testProperty "simple loop" prop_forStatementSimple
      , testProperty "simple loop with key" prop_forStatementWithKey
      , testProperty "with else branch" prop_forStatementEmpty
      , testProperty "with filter" prop_forStatementFilter
      , testProperty "loop object" prop_forStatementLoopVars
      ]
    , testGroup "CallS"
      [ testProperty "no args" prop_callNoArgs
      ]
    ]
  ]

safeAt :: Int -> [a] -> Maybe a
safeAt n = listToMaybe . drop n

justNonzero :: (Eq a, Num a) => a -> Maybe a
justNonzero 0 = Nothing
justNonzero n = Just n

justPositive :: (Eq a, Ord a, Num a) => a -> Maybe a
justPositive n | n > 0 = Just n
justPositive _ = Nothing  

runGingerIdentity :: GingerT Identity a -> a
runGingerIdentity action =
  either (error . show) id $ runGingerIdentityEither action

runGingerIdentityEither :: GingerT Identity a -> Either RuntimeError a
runGingerIdentityEither action =
  runIdentity (runGingerT action defContext emptyEnv)

prop_noBottoms :: (Eval Identity a, Arbitrary a) => a -> Bool
prop_noBottoms e =
  runGingerIdentityEither (eval e) `seq` True

isProcedure :: Value m -> Bool
isProcedure ProcedureV {} = True
isProcedure _ = False

prop_setVarLookupVar :: Identifier -> Value Identity -> Property
prop_setVarLookupVar k v =
  let (w, equal) = runGingerIdentity program

      program :: GingerT Identity (Value Identity, Bool)
      program = do
        setVar k v
        v' <- lookupVar k
        (,) <$> pure v' <*> valuesEqual v v'
  in
    -- exclude procedures, because we cannot compare those
    (not . getAny) (traverseValue (Any . isProcedure) v) ==>
    counterexample (show w)
    (equal === True)

prop_stringifyString :: String -> Property
prop_stringifyString str =
  let expected = Text.pack str
      actual = runGingerIdentity (stringify (StringV expected))
  in
    expected === actual

prop_stringifyNone :: Property
prop_stringifyNone =
  runGingerIdentity (stringify NoneV) === ""

prop_stringifyShow :: (ToValue a Identity, Show a) => Proxy a -> a -> Property
prop_stringifyShow _ i =
  let expected = Text.show i
      actual = runGingerIdentity (stringify $ toValue i)
  in
    expected === actual

prop_scopedVarsDisappear :: (Identifier, Value Identity)
                         -> (Identifier, Value Identity)
                         -> Property
prop_scopedVarsDisappear (name1, val1) (name2, val2) =
  name1 /= name2 ==>
  property . runGingerIdentity $ do
    setVar name1 val1
    exists1a <- isJust <$> lookupVarMaybe name1
    exists2 <- scoped $ do
      setVar name2 val2
      isJust <$> lookupVarMaybe name2
    exists1c <- isJust <$> lookupVarMaybe name1
    notExists2 <- isNothing <$> lookupVarMaybe name2
    pure $ and [ exists1a, exists1c, exists2, notExists2 ]

prop_binop :: (ToValue a Identity, ToValue b Identity, ToValue c Identity)
           => BinaryOperator
           -> (a -> b -> c)
           -> a
           -> b
           -> Property
prop_binop = prop_binopCond Just Just

prop_binopCond :: (ToValue a' Identity, ToValue b' Identity, ToValue c Identity)
               => (a -> Maybe a')
               -> (b -> Maybe b')
               -> BinaryOperator
               -> (a' -> b' -> c)
               -> a
               -> b
               -> Property
prop_binopCond fX fY binop f x' y' =
  let x = fX x'
      y = fY y'
      resultG = runGingerIdentity $ do
                  setVar "a" (toValue x)
                  setVar "b" (toValue y)
                  eval (BinaryE binop (VarE "a") (VarE "b"))
      resultH = toValue $ f <$> x <*> y
  in
    isJust x ==>
    isJust y ==>
    resultG === resultH

prop_literal :: ToValue a Identity => (a -> Expr) -> a -> Property
prop_literal = prop_literalWith id

prop_literalWith :: ToValue b Identity => (a -> b) -> (b -> Expr) -> a -> Property
prop_literalWith f mkExpr val =
  let expr = mkExpr (f val)
      result = runGingerIdentity (eval expr)
  in
    result === toValue (f val)

prop_ternary :: Bool -> Integer -> Integer -> Property
prop_ternary cond yes no =
  let expr = TernaryE (BoolE cond) (IntLitE yes) (IntLitE no)
      resultG = runGingerIdentity (eval expr)
      resultH = if cond then yes else no
  in
    resultG === toValue resultH

prop_var :: Identifier -> Integer -> Property
prop_var name val =
  let expr = VarE name
      resultG = runGingerIdentity (setVar name (toValue val) >> eval expr)
  in
    resultG === toValue val

prop_varNeg :: Identifier -> Integer -> Identifier -> Property
prop_varNeg name1 val1 name2 =
  let expr = VarE name2
      resultG = runGingerIdentityEither (setVar name1 (toValue val1) >> eval expr)
  in
    name1 /= name2 ==>
    resultG === Left (NotInScopeError (Just $ identifierName name2))

prop_nativeNullary :: Identifier -> Integer -> Property
prop_nativeNullary varName constVal =
  let fVal = ProcedureV . NativeProcedure $ const . pure @Identity . Right . toValue $ constVal
      expr = CallE (VarE varName) [] []
      result = runGingerIdentity (setVar varName fVal >> eval expr)
  in
    result === toValue constVal

prop_nativeIdentity :: Identifier -> Identifier -> Integer -> Property
prop_nativeIdentity varName argVarName arg =
  let fVal = toValue (id :: Value Identity -> Value Identity)
      argVal = toValue arg
      expr = CallE (VarE varName) [VarE argVarName] []
      result = runGingerIdentity $ do
                setVar varName fVal
                setVar argVarName argVal
                eval expr
  in
    result === argVal

prop_userNullary :: Identifier -> Expr -> Property
prop_userNullary varName bodyExpr =
  let fVal = ProcedureV $ GingerProcedure [] bodyExpr
      resultCall = runGingerIdentityEither $ do
                    setVar varName fVal
                    eval $ CallE (VarE varName) [] []
      resultDirect = runGingerIdentityEither $ do
                        eval bodyExpr
  in
    resultCall === resultDirect

prop_immediateStatementOutput :: Encoded -> Property
prop_immediateStatementOutput str =
  let stmt = ImmediateS str
      result = runGingerIdentity (eval stmt)
  in
    result === EncodedV str

prop_commentStatementOutput :: String -> Property
prop_commentStatementOutput str =
  let stmt = CommentS (Text.pack str)
      result = runGingerIdentity (eval stmt)
  in
    result === NoneV

prop_interpolationStatementOutput :: Expr -> Property
prop_interpolationStatementOutput expr =
  let resultS = runGingerIdentityEither (eval $ InterpolationS expr)
      resultE = runGingerIdentityEither (eval expr)
  in
    isRight resultE ==>
    resultS === resultE

prop_ifStatementOutput :: Bool -> Statement -> Statement -> Property
prop_ifStatementOutput cond yes no =
  let resultYes = runGingerIdentityEither (eval yes)
      resultNo = runGingerIdentityEither (eval no)
      resultE = if cond then resultYes else resultNo
      resultIf = runGingerIdentityEither (eval $ IfS (BoolE cond) yes (Just no))
  in
    isRight resultE ==>
    resultIf === resultE

prop_forStatementSimple :: Identifier -> Identifier -> [String] -> Property
prop_forStatementSimple itereeName varName strItems =
  let items = ListV $ map (StringV . Text.pack) strItems
      expected = StringV . Text.pack . mconcat $ strItems
      resultFor = runGingerIdentity $ do
                    setVar itereeName items
                    eval $ ForS
                            Nothing -- loop key var
                            varName-- loop value var
                            (VarE itereeName) -- iteree
                            Nothing -- loop condition
                            NotRecursive
                            (InterpolationS (VarE varName)) -- loop body
                            Nothing -- empty body
  in
    not (null strItems) ==>
    resultFor === expected

prop_forStatementWithKey :: Identifier -> Identifier -> Identifier -> [String] -> Property
prop_forStatementWithKey itereeName varName keyName strItems =
  let items = ListV $ map (StringV . Text.pack) strItems
      expected = StringV . Text.pack . mconcat $ [ show k ++ v | (k, v) <- zip [(0 :: Int) ..] strItems ]
      resultFor = runGingerIdentity $ do
                    setVar itereeName items
                    eval $ ForS
                            (Just keyName) -- loop key var
                            varName-- loop value var
                            (VarE itereeName) -- iteree
                            Nothing -- loop condition
                            NotRecursive
                            (GroupS
                              [ InterpolationS (VarE keyName)
                              , InterpolationS (VarE varName)
                              ]
                            ) -- loop body
                            Nothing -- empty body
  in
    not (null strItems) ==>
    resultFor === expected

prop_forStatementEmpty :: Statement -> Property
prop_forStatementEmpty body =
  let resultDirect = runGingerIdentityEither (eval body)
      resultFor = runGingerIdentityEither
                    (eval $ ForS
                      Nothing
                      "item"
                      (ListE [])
                      Nothing
                      NotRecursive
                      (InterpolationS (VarE "item"))
                      (Just body)
                    )
  in
    isRight resultDirect ==>
    resultFor === resultDirect

prop_forStatementFilter :: [Integer] -> Property
prop_forStatementFilter intItems =
  let items = ListV $ map IntV intItems
      expected = StringV . Text.pack . mconcat $ [ show i | i <- intItems, i > 0 ]
      resultFor = runGingerIdentity $ do
                    setVar "items" items
                    eval $ ForS
                            Nothing
                            "item"
                            (VarE "items") -- iteree
                            (Just $ BinaryE BinopGT (VarE "item") (IntLitE 0))
                            NotRecursive
                            (InterpolationS (VarE "item"))
                            Nothing
  in
    length (filter (> 0) intItems) > 1 ==>
    resultFor === expected

prop_forStatementLoopVars :: [Integer] -> Property
prop_forStatementLoopVars intItems = 
  let items = ListV $ map IntV intItems
      expected = StringV . Text.pack . mconcat $
          [ show (succ i) ++ show (i :: Int) ++ show v
          | (i, v) <- zip [0..] intItems
          ]
      resultFor = runGingerIdentity $ do
                    setVar "items" items
                    eval $ ForS
                            Nothing
                            "item"
                            (VarE "items") -- iteree
                            Nothing
                            NotRecursive
                            (GroupS
                              [ InterpolationS (BinaryE BinopIndex (VarE "loop") (StringLitE "index"))
                              , InterpolationS (BinaryE BinopIndex (VarE "loop") (StringLitE "index0"))
                              , InterpolationS (VarE "item")
                              ]
                            )
                            Nothing
  in
    not (null intItems) ==>
    resultFor === expected

prop_callNoArgs :: Expr -> Property
prop_callNoArgs body =
  let resultDirect = runGingerIdentityEither (eval body)
      resultCall = runGingerIdentityEither $ do
                      setVar "f" $ ProcedureV (GingerProcedure [] body)
                      eval $ CallS "f" [] []
  in
    isRight resultDirect ==>
    resultCall === resultDirect
