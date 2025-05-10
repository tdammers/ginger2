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
      [ testCase "StringLitE simple (double-quoted)" $
          test_parser expr "\"Hello\"" (StringLitE "Hello")
      , testCase "StringLitE simple (single-quoted)" $
          test_parser expr "'Hello'" (StringLitE "Hello")
      , testCase "StringLitE with escapes (double-quoted)" $
          test_parser expr "\"Hello\\\n\\\"world\\\"\"" (StringLitE "Hello\n\"world\"")
      , testCase "StringLitE with escapes (single-quoted)" $
          test_parser expr "'Hello\\\n\\'world\\''" (StringLitE "Hello\n'world'")
      , testCase "IntLitE positive" $
          test_parser expr "2134" (IntLitE 2134)
      , testCase "IntLitE negative" $
          test_parser expr "-2134" (IntLitE (-2134))
      , testCase "FloatLitE positive decimal" $
          test_parser expr "21.34" (FloatLitE 21.34)
      , testCase "FloatLitE negative decimal" $
          test_parser expr "-21.34" (FloatLitE (-21.34))
      , testCase "FloatLitE positive exponential" $
          test_parser expr "21.34e10" (FloatLitE 21.34e10)
      , testCase "FloatLitE negative exponential" $
          test_parser expr "-21.34e10" (FloatLitE (-21.34e10))
      , testCase "FloatLitE positive exponential, negative exponent" $
          test_parser expr "21.34e-10" (FloatLitE 21.34e-10)
      , testCase "FloatLitE positive decimal, leading dot" $
          test_parser expr ".34" (FloatLitE 0.34)
      , testCase "FloatLitE positive decimal, trailing dot" $
          test_parser expr "12." (FloatLitE 12)
      , testCase "FloatLitE negative decimal, leading dot" $
          test_parser expr "-.34" (FloatLitE (-0.34))
      , testCase "NoneE" $
          test_parser expr "none" NoneE
      , testCase "BoolE True" $
          test_parser expr "true" (BoolE True)
      , testCase "BoolE False" $
          test_parser expr "false" (BoolE False)
      , testCase "NoneE (uppercase)" $
          test_parser expr "None" NoneE
      , testCase "BoolE True (uppercase)" $
          test_parser expr "True" (BoolE True)
      , testCase "BoolE False (uppercase)" $
          test_parser expr "False" (BoolE False)
      , testCase "VarE" $
          test_parser expr "foo" (VarE "foo")
      , testCase "Parenthesized" $
          test_parser expr "(foo)" (VarE "foo")
      ]
    , testGroup "Unops" $
        map
          (\(unop, sym) ->
              testCase (show unop) $
                test_parser
                  expr
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
                  expr
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
          test_parser expr "[]" (ListE [])
      , testCase "single-item list" $
          test_parser expr "[ 1 ] " (ListE [IntLitE 1])
      , testCase "multi-item list" $
          test_parser expr "[ 1, \"aaa\" ] " (ListE [IntLitE 1, StringLitE "aaa"])
      , testCase "nested list" $
          test_parser expr "[ [ none ], true, false ] "
            (ListE
              [ ListE [ NoneE ]
              , BoolE True
              , BoolE False
              ])
      ]
    , testGroup "Dict"
      [ testCase "empty dict" $
          test_parser expr "{}" (DictE [])
      , testCase "single-item dict" $
          test_parser expr "{ 1: none } " (DictE [(IntLitE 1, NoneE)])
      , testCase "multi-item dict" $
          test_parser expr "{ foo: 1, bar: \"aaa\" } " (DictE [(VarE "foo", IntLitE 1), (VarE "bar", StringLitE "aaa")])
      , testCase "nested dict" $
          test_parser expr "{ \"a\": {}, \"b\": { \"c\": 123 }}"
            (DictE
              [ (StringLitE "a", DictE [])
              , (StringLitE "b", DictE [(StringLitE "c", IntLitE 123)])
              ])
      ]
    , testGroup "ternary"
      [ testCase "simple" $
          test_parser expr "foo if bar else baz"
            (TernaryE (VarE "bar") (VarE "foo") (VarE "baz"))
      , testCase "nested" $
          test_parser expr "foo if bar else baz if quux else none"
            (TernaryE (VarE "bar") (VarE "foo")
              (TernaryE (VarE "quux") (VarE "baz") NoneE))
      ]
    , testGroup "is"
      [ testCase "simple" $
          test_parser expr "foo is bar"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "empty arg list" $
          test_parser expr "foo is bar()"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "with arg" $
          test_parser expr "foo is bar(baz)"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "is not" $
          test_parser expr "foo is not bar"
            (NotE $ IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "single arg without parentheses" $
          test_parser expr "foo is bar baz"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      ]
    , testGroup "Call and index"
      [ testCase "call nullary" $
          test_parser expr "foo()" (CallE (VarE "foo") [] [])
      , testCase "call 1 positional arg" $
          test_parser expr "foo(a)" (CallE (VarE "foo") [VarE "a"] [])
      , testCase "call 2 positional args" $
          test_parser expr "foo(a, b)" (CallE (VarE "foo") [VarE "a", VarE "b"] [])
      , testCase "call 1 named arg" $
          test_parser expr "foo(a=b)" (CallE (VarE "foo") [] [("a", VarE "b")])
      , testCase "call 2 named args" $
          test_parser expr "foo(a=b, c = d)" (CallE (VarE "foo") [] [("a", VarE "b"), ("c", VarE "d")])
      , testCase "call mixed args" $
          test_parser expr "foo(a, b = c)" (CallE (VarE "foo") [VarE "a"] [("b", VarE "c")])
      , testCase "index" $
          test_parser expr "foo[bar]" (IndexE (VarE "foo") (VarE "bar"))
      , testCase "dot-member" $
          test_parser expr "foo.bar" (IndexE (VarE "foo") (StringLitE "bar"))
      , testCase "filter (no args)" $
          test_parser expr "foo|bar" (FilterE (VarE "foo") (VarE "bar") [] [])
      , testCase "filter (positional arg)" $
          test_parser expr "foo|bar(baz)" (FilterE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "filter (kw arg)" $
          test_parser expr "foo|bar(baz=quux)" (FilterE (VarE "foo") (VarE "bar") [] [("baz", VarE "quux")])
      ]
    ]
    , testGroup "Precedence"
      [ testCase "+ vs *" $
          test_parser expr
            "a * b + c * d"
            (PlusE
              (MulE (VarE "a") (VarE "b"))
              (MulE (VarE "c") (VarE "d"))
            )
      , testCase "* vs **" $
          test_parser expr
            "a ** b * c ** d"
            (MulE
              (PowerE (VarE "a") (VarE "b"))
              (PowerE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ~" $
          test_parser expr
            "a + b ~ c + d"
            (ConcatE
              (PlusE (VarE "a") (VarE "b"))
              (PlusE (VarE "c") (VarE "d"))
            )
      , testCase "== vs and" $
          test_parser expr
            "a == b and c == d"
            (AndE
              (EqualE (VarE "a") (VarE "b"))
              (EqualE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ." $
          test_parser expr
            "a + b[c]"
            (PlusE
              (VarE "a")
              (IndexE
                (VarE "b")
                (VarE "c")
              )
            )
      , testCase "is vs ==" $
          test_parser expr
            "a is b == c"
            (EqualE
              (IsE (VarE "a") (VarE "b") [] [])
              (VarE "c")
            )
      ]
  , testGroup "Statement"
    [ testCase "plain immediate" $
        test_parser statement
          "Hello, world!"
          (ImmediateS $ Encoded "Hello, world!")
    , testCase "immediate with curly braces" $
        test_parser statement
          "Hello, {world}!"
          (ImmediateS $ Encoded "Hello, {world}!")
    , testCase "interpolation" $
        test_parser statement
          "{{ world }}"
          (InterpolationS (VarE "world"))
    , testCase "dict interpolation" $
        test_parser statement
          "{{ {\"bar\": { \"foo\": world }} }}"
          (InterpolationS (DictE [(StringLitE "bar", (DictE [(StringLitE "foo", VarE "world")]))]))
    , testCase "comment" $
        test_parser statement
          "{# world #}"
          (CommentS "world")
    , testCase "if" $
        test_parser statement
          "{% if foo %}blah{% endif %}"
          (IfS (VarE "foo") (ImmediateS $ Encoded "blah") Nothing)
    , testCase "if-else" $
        test_parser statement
          "{% if foo %}blah{% else %}pizza{% endif %}"
          (IfS (VarE "foo") (ImmediateS $ Encoded "blah") (Just (ImmediateS $ Encoded "pizza")))
    , testCase "set" $
        test_parser statement
          "{% set foo = bar %}"
          (SetS "foo" (VarE "bar"))
    , testCase "set block" $
        test_parser statement
          "{% set foo %}foo{% endset %}"
          (SetBlockS "foo" (ImmediateS $ Encoded "foo") Nothing)
    , testCase "set block with filter" $
        test_parser statement
          "{% set foo | bar %}foo{% endset %}"
          (SetBlockS "foo" (ImmediateS $ Encoded "foo") (Just (VarE "bar")))
    , testCase "for" $
        test_parser statement
          "{% for user in users %}{{ user.name }}{% endfor %}"
          (ForS
            Nothing (Identifier "user") (VarE "users")
            Nothing
            NotRecursive
            (InterpolationS (IndexE (VarE "user") (StringLitE "name")))
            Nothing
          )
    , testCase "for-else" $
        test_parser statement
          "{% for user in users %}{{ user.name }}{% else %}Nope{% endfor %}"
          (ForS
            Nothing (Identifier "user") (VarE "users")
            Nothing
            NotRecursive
            (InterpolationS (IndexE (VarE "user") (StringLitE "name")))
            (Just (ImmediateS (Encoded "Nope")))
          )
    , testCase "for-if" $
        test_parser statement
          "{% for user in users if user.name is defined %}{{ user.name }}{% endfor %}"
          (ForS
            Nothing (Identifier "user") (VarE "users")
            (Just (IsE (IndexE (VarE "user") (StringLitE "name")) (VarE "defined") [] []))
            NotRecursive
            (InterpolationS (IndexE (VarE "user") (StringLitE "name")))
            Nothing
          )
    , testCase "for-if-else" $
        test_parser statement
          "{% for user in foo if not bar else baz %}{{ user.name }}{% endfor %}"
          (ForS
            Nothing (Identifier "user")
            (TernaryE (NotE (VarE "bar")) (VarE "foo") (VarE "baz"))
            Nothing
            NotRecursive
            (InterpolationS (IndexE (VarE "user") (StringLitE "name")))
            Nothing
          )
    , testCase "for-if-recursive" $
        test_parser statement
          "{% for user in foo if bar recursive %}{{ user.name }}{% endfor %}"
          (ForS
            Nothing (Identifier "user")
            (VarE "foo")
            (Just (VarE "bar"))
            Recursive
            (InterpolationS (IndexE (VarE "user") (StringLitE "name")))
            Nothing
          )
    , testCase "include" $
        test_parser statement
          "{% include \"foo\" %}"
          (IncludeS (StringLitE "foo") RequireMissing WithContext)
    , testCase "include without context" $
        test_parser statement
          "{% include \"foo\" without context %}"
          (IncludeS (StringLitE "foo") RequireMissing WithoutContext)
    , testCase "include ignore missing" $
        test_parser statement
          "{% include \"foo\" ignore missing %}"
          (IncludeS (StringLitE "foo") IgnoreMissing WithContext)
    , testCase "include ignore missing without context" $
        test_parser statement
          "{% include \"foo\" ignore missing without context %}"
          (IncludeS (StringLitE "foo") IgnoreMissing WithoutContext)
    , testCase "extends" $
        test_parser statement
          "{% extends \"foo\" %}"
          (ExtendsS (StringLitE "foo"))
    , testCase "block" $
        test_parser statement
          "{% block foo %}Hello{% endblock %}"
          (BlockS "foo" (ImmediateS . Encoded $ "Hello") NotScoped Optional)
    , testCase "block (repeat block name)" $
        test_parser statement
          "{% block foo %}Hello{% endblock foo %}"
          (BlockS "foo" (ImmediateS . Encoded $ "Hello") NotScoped Optional)
    , testCase "block (repeat wrong block name)" $
        test_parserEx statement
          "{% block foo %}Hello{% endblock bar %}"
          (Left . unlines $
                  [ "1:33:"
                  , "  |"
                  , "1 | {% block foo %}Hello{% endblock bar %}"
                  , "  |                                 ^^"
                  , "unexpected \"ba\""
                  , "expecting \"%}\", \"foo\", or white space"
                  ])
    ]
  ]

test_parser :: (Eq a, Show a) => P a -> Text -> a -> Assertion
test_parser p input expected =
  test_parserEx p input (Right expected)

test_parserEx :: (Eq a, Show a) => P a -> Text -> (Either String a) -> Assertion
test_parserEx p input expected = do
  assertEqual "" expected actual
  where
    actual = mapLeft errorBundlePretty $ parse (p <* eof) "" input

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
