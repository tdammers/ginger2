module Main
where

import Test.Tasty

import qualified Language.Ginger.Value.Tests as Value
import qualified Language.Ginger.AST.Tests as AST
import qualified Language.Ginger.Interpret.Tests as Interpret
import qualified Language.Ginger.Parse.Tests as Parse

tests :: TestTree
tests =
  testGroup "ginger"
    [ Value.tests
    , AST.tests
    , Interpret.tests
    , Parse.tests
    ]

main :: IO ()
main = defaultMain tests
