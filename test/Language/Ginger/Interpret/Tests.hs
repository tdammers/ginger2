{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ginger.Interpret.Tests
where

import Language.Ginger.Value
import Language.Ginger.AST
import Language.Ginger.Interpret
import Language.Ginger.RuntimeError
import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Monad.Identity
import qualified Data.Text as Text
import Data.Proxy (Proxy (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Maybe (isJust, isNothing)

tests :: TestTree
tests = testGroup "Language.Ginger.Interpret"
  [ testGroup "misc"
    [ testProperty "setVar lookupVar" prop_setVarLookupVar
    , testProperty "scoped vars disappear outside" prop_scopedVarsDisappear
    , testProperty "no bottoms in eval" prop_noBottoms
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
  ]

runGingerIdentity :: GingerT Identity a -> a
runGingerIdentity action =
  either (error . show) id $ runGingerIdentityEither action

runGingerIdentityEither :: GingerT Identity a -> Either RuntimeError a
runGingerIdentityEither action =
  runIdentity (runGingerT action defContext emptyEnv)

prop_noBottoms :: Expr -> Bool
prop_noBottoms e =
  runGingerIdentityEither (eval e) `seq` True

prop_setVarLookupVar :: Identifier -> Value Identity -> Property
prop_setVarLookupVar k v =
  let (w, equal) = runGingerIdentity program

      program :: GingerT Identity (Value Identity, Bool)
      program = do
        setVar k v
        v' <- lookupVar k
        (,) <$> pure v' <*> valuesEqual v v'
  in
    counterexample (show w) $
    equal === True

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
