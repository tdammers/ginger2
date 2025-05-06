{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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

tests :: TestTree
tests = testGroup "Language.Ginger.Interpret"
  [ testGroup "misc"
    [ testProperty "setVar lookupVar" prop_setVarLookupVar
    , testProperty "no bottoms in eval" prop_noBottoms
    ]
  , testGroup "stringify"
    [ testProperty "string stringifies to self" prop_stringifyString
    , testProperty "None stringifies to empty" prop_stringifyNone
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
