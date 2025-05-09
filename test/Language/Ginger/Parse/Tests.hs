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
      , testCase "IntLitE positive" $
          test_parser exprP "2134" (IntLitE 2134)
      , testCase "IntLitE negative" $
          test_parser exprP "-2134" (IntLitE (-2134))
      , testCase "FloatLitE positive decimal" $
          test_parser exprP "21.34" (FloatLitE 21.34)
      , testCase "FloatLitE negative decimal" $
          test_parser exprP "-21.34" (FloatLitE (-21.34))
      , testCase "FloatLitE positive exponential" $
          test_parser exprP "21.34e10" (FloatLitE 21.34e10)
      , testCase "FloatLitE negative exponential" $
          test_parser exprP "-21.34e10" (FloatLitE (-21.34e10))
      , testCase "FloatLitE positive exponential, negative exponent" $
          test_parser exprP "21.34e-10" (FloatLitE 21.34e-10)
      , testCase "FloatLitE positive decimal, leading dot" $
          test_parser exprP ".34" (FloatLitE 0.34)
      , testCase "FloatLitE positive decimal, trailing dot" $
          test_parser exprP "12." (FloatLitE 12)
      , testCase "FloatLitE negative decimal, leading dot" $
          test_parser exprP "-.34" (FloatLitE (-0.34))
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
    , testGroup "Unops" $
        map
          (\(unop, sym) ->
              testCase (show unop) $
                test_parser
                  exprP
                  (sym <> " foo")
                  (UnaryE unop (VarE "foo"))
          )
          [ (UnopNot, "not")
          , (UnopNegate, "-")
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
          , (BinopAnd, "and")
          , (BinopOr, "or")
          , (BinopIn, "in")
          , (BinopConcat, "~")
          ]
    , testGroup "List"
      [ testCase "empty list" $
          test_parser exprP "[]" (ListE [])
      , testCase "single-item list" $
          test_parser exprP "[ 1 ] " (ListE [IntLitE 1])
      , testCase "multi-item list" $
          test_parser exprP "[ 1, \"aaa\" ] " (ListE [IntLitE 1, StringLitE "aaa"])
      , testCase "nested list" $
          test_parser exprP "[ [ none ], true, false ] "
            (ListE
              [ ListE [ NoneE ]
              , BoolE True
              , BoolE False
              ])
      ]
    , testGroup "Dict"
      [ testCase "empty dict" $
          test_parser exprP "{}" (DictE [])
      , testCase "single-item dict" $
          test_parser exprP "{ 1: none } " (DictE [(IntLitE 1, NoneE)])
      , testCase "multi-item dict" $
          test_parser exprP "{ foo: 1, bar: \"aaa\" } " (DictE [(VarE "foo", IntLitE 1), (VarE "bar", StringLitE "aaa")])
      , testCase "nested dict" $
          test_parser exprP "{ \"a\": {}, \"b\": { \"c\": 123 }}"
            (DictE
              [ (StringLitE "a", DictE [])
              , (StringLitE "b", DictE [(StringLitE "c", IntLitE 123)])
              ])
      ]
    , testGroup "ternary"
      [ testCase "simple" $
          test_parser exprP "foo if bar else baz"
            (TernaryE (VarE "bar") (VarE "foo") (VarE "baz"))
      , testCase "nested" $
          test_parser exprP "foo if bar else baz if quux else none"
            (TernaryE (VarE "bar") (VarE "foo")
              (TernaryE (VarE "quux") (VarE "baz") NoneE))
      ]
    , testGroup "is"
      [ testCase "simple" $
          test_parser exprP "foo is bar"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "empty arg list" $
          test_parser exprP "foo is bar()"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "with arg" $
          test_parser exprP "foo is bar(baz)"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "is not" $
          test_parser exprP "foo is not bar"
            (NotE $ IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "single arg without parentheses" $
          test_parser exprP "foo is bar baz"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
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
          test_parser exprP "foo[bar]" (IndexE (VarE "foo") (VarE "bar"))
      , testCase "dot-member" $
          test_parser exprP "foo.bar" (IndexE (VarE "foo") (StringLitE "bar"))
      , testCase "filter (no args)" $
          test_parser exprP "foo|bar" (FilterE (VarE "foo") (VarE "bar") [] [])
      , testCase "filter (positional arg)" $
          test_parser exprP "foo|bar(baz)" (FilterE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "filter (kw arg)" $
          test_parser exprP "foo|bar(baz=quux)" (FilterE (VarE "foo") (VarE "bar") [] [("baz", VarE "quux")])
      ]
    ]
    , testGroup "Precedence"
      [ testCase "+ vs *" $
          test_parser exprP
            "a * b + c * d"
            (PlusE
              (MulE (VarE "a") (VarE "b"))
              (MulE (VarE "c") (VarE "d"))
            )
      , testCase "* vs **" $
          test_parser exprP
            "a ** b * c ** d"
            (MulE
              (PowerE (VarE "a") (VarE "b"))
              (PowerE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ~" $
          test_parser exprP
            "a + b ~ c + d"
            (ConcatE
              (PlusE (VarE "a") (VarE "b"))
              (PlusE (VarE "c") (VarE "d"))
            )
      , testCase "== vs and" $
          test_parser exprP
            "a == b and c == d"
            (AndE
              (EqualE (VarE "a") (VarE "b"))
              (EqualE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ." $
          test_parser exprP
            "a + b[c]"
            (PlusE
              (VarE "a")
              (IndexE
                (VarE "b")
                (VarE "c")
              )
            )
      , testCase "is vs ==" $
          test_parser exprP
            "a is b == c"
            (EqualE
              (IsE (VarE "a") (VarE "b") [] [])
              (VarE "c")
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
