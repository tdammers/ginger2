module Language.Ginger.Value.Tests
where

import Language.Ginger.Value
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Text as Text
import Data.String (IsString (..))

tests :: TestTree
tests = testGroup "Language.Ginger.Value"
  [ testProperty "Int toScalar" prop_IntToScalar
  , testProperty "Scalar IsString" prop_ScalarIsString
  ]

prop_IntToScalar :: Int -> Property
prop_IntToScalar i =
  IntScalar (fromIntegral i) === toScalar i


prop_ScalarIsString :: String -> Property
prop_ScalarIsString str =
  let s1 = fromString str
      s2 = StringScalar . Text.pack $ str
  in s1 === s2
