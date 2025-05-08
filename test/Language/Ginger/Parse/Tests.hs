{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ginger.Parse.Tests
where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse, errorBundlePretty, eof)

import Language.Ginger.AST
import Language.Ginger.Parse

tests :: TestTree
tests = testGroup "Language.Ginger.Parse"
  [ testGroup "Expr"
    [ testGroup "Simple"
      [ testCase "StringLitE simple" $
          test_parser exprP "\"Hello\"" (StringLitE "Hello")
      , testCase "NoneE" $
          test_parser exprP "none" NoneE
      , testCase "BoolE True" $
          test_parser exprP "true" (BoolE True)
      , testCase "BoolE False" $
          test_parser exprP "false" (BoolE False)
      , testCase "NoneE (uppercase)" $
          test_parser exprP "None" NoneE
      , testCase "BoolE True (uppercase)" $
          test_parser exprP "True" (BoolE True)
      , testCase "BoolE False (uppercase)" $
          test_parser exprP "False" (BoolE False)
      , testCase "VarE" $
          test_parser exprP "foo" (VarE "foo")
      , testCase "Parenthesized" $
          test_parser exprP "(foo)" (VarE "foo")
      ]
    , testGroup "Binops" $
        map
          (\(binop, sym) ->
              testCase (show binop) $
                test_parser
                  exprP
                  ("foo " <> sym <> " bar")
                  (BinaryE binop (VarE "foo") (VarE "bar"))
          )
          [ (BinopPlus, "+")
          , (BinopMinus, "-")
          , (BinopDiv, "/")
          , (BinopIntDiv, "//")
          , (BinopMod, "%")
          , (BinopMul, "*")
          , (BinopPower, "**")
          , (BinopEqual, "==")
          , (BinopNotEqual, "!=")
          , (BinopGT, ">")
          , (BinopGTE, ">=")
          , (BinopLT, "<")
          , (BinopLTE, "<=")
          , (BinopAnd, "&&")
          , (BinopOr, "||")
          , (BinopIn, "in")
          , (BinopConcat, "~")
          ]
    , testGroup "Call and index"
      [ testCase "call nullary" $
          test_parser exprP "foo()" (CallE (VarE "foo") [] [])
      , testCase "call 1 positional arg" $
          test_parser exprP "foo(a)" (CallE (VarE "foo") [VarE "a"] [])
      , testCase "call 2 positional args" $
          test_parser exprP "foo(a, b)" (CallE (VarE "foo") [VarE "a", VarE "b"] [])
      , testCase "call 1 named arg" $
          test_parser exprP "foo(a=b)" (CallE (VarE "foo") [] [("a", VarE "b")])
      , testCase "call 2 named args" $
          test_parser exprP "foo(a=b, c = d)" (CallE (VarE "foo") [] [("a", VarE "b"), ("c", VarE "d")])
      , testCase "call mixed args" $
          test_parser exprP "foo(a, b = c)" (CallE (VarE "foo") [VarE "a"] [("b", VarE "c")])
      , testCase "index" $
          test_parser exprP "foo[bar]" (BinaryE BinopIndex (VarE "foo") (VarE "bar"))
      , testCase "dot-member" $
          test_parser exprP "foo.bar" (BinaryE BinopIndex (VarE "foo") (StringLitE "bar"))
      ]
    ]
    , testGroup "Precedence"
      [ testCase "+ vs *" $
          test_parser exprP
            "a * b + c * d"
            (BinaryE BinopPlus
              (BinaryE BinopMul (VarE "a") (VarE "b"))
              (BinaryE BinopMul (VarE "c") (VarE "d"))
            )
      , testCase "* vs **" $
          test_parser exprP
            "a ** b * c ** d"
            (BinaryE BinopMul
              (BinaryE BinopPower (VarE "a") (VarE "b"))
              (BinaryE BinopPower (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ~" $
          test_parser exprP
            "a + b ~ c + d"
            (BinaryE BinopConcat
              (BinaryE BinopPlus (VarE "a") (VarE "b"))
              (BinaryE BinopPlus (VarE "c") (VarE "d"))
            )
      , testCase "== vs &&" $
          test_parser exprP
            "a == b && c == d"
            (BinaryE BinopAnd
              (BinaryE BinopEqual (VarE "a") (VarE "b"))
              (BinaryE BinopEqual (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ." $
          test_parser exprP
            "a + b[c]"
            (BinaryE BinopPlus
              (VarE "a")
              (BinaryE BinopIndex
                (VarE "b")
                (VarE "c")
              )
            )
      ]
  ]

test_parser :: (Eq a, Show a) => P a -> Text -> a -> Assertion
test_parser p input expected = do
  assertEqual "" (Right expected) actual
  where
    actual = mapLeft errorBundlePretty $ parse (p <* eof) "" input

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
