{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Ginger.Parse.Tests
where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec (eof)
import Test.QuickCheck.Instances ()

import Language.Ginger.AST
import Language.Ginger.Parse
import Language.Ginger.Render
import Language.Ginger.TestUtils

tests :: TestTree
tests = testGroup "Language.Ginger.Parse"
  [ testGroup "Expr"
    [ testProperty "roundTrip" (prop_parserRoundTrip exprUP id)
    , testGroup "Simple"
      [ testGroup "StringLitE"
        [ testCase "simple (double-quoted)" $
            test_parser exprUP "\"Hello\"" (StringLitE "Hello")
        , testCase "simple (single-quoted)" $
            test_parser exprUP "'Hello'" (StringLitE "Hello")
        , testCase "with escapes (double-quoted)" $
            test_parser exprUP "\"Hello\\n\\\"world\\\"\"" (StringLitE "Hello\n\"world\"")
        , testCase "with escapes (single-quoted)" $
            test_parser exprUP "'Hello\\n\\'world\\''" (StringLitE "Hello\n'world'")
        ]
      , testGroup "IntLitE"
        [ testCase "IntLitE positive" $
            test_parser exprUP "2134" (IntLitE 2134)
        , testCase "IntLitE negative" $
            test_parser exprUP "-2134" (IntLitE (-2134))
        ]
      , testGroup "FloatLitE"
        [ testCase "FloatLitE positive decimal" $
            test_parser exprUP "21.34" (FloatLitE 21.34)
        , testCase "FloatLitE negative decimal" $
            test_parser exprUP "-21.34" (FloatLitE (-21.34))
        , testCase "FloatLitE positive exponential" $
            test_parser exprUP "21.34e10" (FloatLitE 21.34e10)
        , testCase "FloatLitE negative exponential" $
            test_parser exprUP "-21.34e10" (FloatLitE (-21.34e10))
        , testCase "FloatLitE positive exponential, negative exponent" $
            test_parser exprUP "21.34e-10" (FloatLitE 21.34e-10)
        , testCase "FloatLitE positive decimal, leading dot" $
            test_parser exprUP ".34" (FloatLitE 0.34)
        , testCase "FloatLitE positive decimal, trailing dot" $
            test_parser exprUP "12." (FloatLitE 12)
        , testCase "FloatLitE negative decimal, leading dot" $
            test_parser exprUP "-.34" (FloatLitE (-0.34))
        ]
      , testGroup "NoneE"
        [ testCase "none" $
            test_parser exprUP "none" NoneE
        , testCase "None (uppercase)" $
            test_parser exprUP "None" NoneE
        ]
      , testGroup "BoolE"
        [ testCase "BoolE True" $
            test_parser exprUP "true" (BoolE True)
        , testCase "BoolE False" $
            test_parser exprUP "false" (BoolE False)
        , testCase "BoolE True (uppercase)" $
            test_parser exprUP "True" (BoolE True)
        , testCase "BoolE False (uppercase)" $
            test_parser exprUP "False" (BoolE False)
        ]
      , testGroup "VarE"
        [ testCase "simple" $
            test_parser exprUP "foo" (VarE "foo")
        , testCase "parenthesized" $
            test_parser exprUP "(foo)" (VarE "foo")
        ]
      ]
    , testGroup "Unops" $
        map
          (\(unop, sym) ->
              testCase (show unop) $
                test_parser
                  exprUP
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
                  exprUP
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
          test_parser exprUP "[]" (ListE mempty)
      , testCase "single-item list" $
          test_parser exprUP "[ 1 ] " (ListE [IntLitE 1])
      , testCase "multi-item list" $
          test_parser exprUP "[ 1, \"aaa\" ] " (ListE [IntLitE 1, StringLitE "aaa"])
      , testCase "nested list" $
          test_parser exprUP "[ [ none ], true, false ] "
            (ListE
              [ ListE [ NoneE ]
              , BoolE True
              , BoolE False
              ])
      ]
    , testGroup "Dict"
      [ testCase "empty dict" $
          test_parser exprUP "{}" (DictE [])
      , testCase "single-item dict" $
          test_parser exprUP "{ 1: none } " (DictE [(IntLitE 1, NoneE)])
      , testCase "multi-item dict" $
          test_parser exprUP "{ foo: 1, bar: \"aaa\" } " (DictE [(VarE "foo", IntLitE 1), (VarE "bar", StringLitE "aaa")])
      , testCase "nested dict" $
          test_parser exprUP "{ \"a\": {}, \"b\": { \"c\": 123 }}"
            (DictE
              [ (StringLitE "a", DictE [])
              , (StringLitE "b", DictE [(StringLitE "c", IntLitE 123)])
              ])
      ]
    , testGroup "ternary"
      [ testCase "simple" $
          test_parser exprUP "foo if bar else baz"
            (TernaryE (VarE "bar") (VarE "foo") (VarE "baz"))
      , testCase "nested" $
          test_parser exprUP "foo if bar else baz if quux else none"
            (TernaryE (VarE "bar") (VarE "foo")
              (TernaryE (VarE "quux") (VarE "baz") NoneE))
      ]
    , testGroup "is"
      [ testCase "simple" $
          test_parser exprUP "foo is bar"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "empty arg list" $
          test_parser exprUP "foo is bar()"
            (IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "with arg" $
          test_parser exprUP "foo is bar(baz)"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "is not" $
          test_parser exprUP "foo is not bar"
            (NotE $ IsE (VarE "foo") (VarE "bar") [] [])
      , testCase "single arg without parentheses" $
          test_parser exprUP "foo is bar baz"
            (IsE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      ]
    , testGroup "Call and index"
      [ testCase "call nullary" $
          test_parser exprUP "foo()" (CallE (VarE "foo") [] [])
      , testCase "call 1 positional arg" $
          test_parser exprUP "foo(a)" (CallE (VarE "foo") [VarE "a"] [])
      , testCase "call 2 positional args" $
          test_parser exprUP "foo(a, b)" (CallE (VarE "foo") [VarE "a", VarE "b"] [])
      , testCase "call 1 named arg" $
          test_parser exprUP "foo(a=b)" (CallE (VarE "foo") [] [("a", VarE "b")])
      , testCase "call 2 named args" $
          test_parser exprUP "foo(a=b, c = d)" (CallE (VarE "foo") [] [("a", VarE "b"), ("c", VarE "d")])
      , testCase "call mixed args" $
          test_parser exprUP "foo(a, b = c)" (CallE (VarE "foo") [VarE "a"] [("b", VarE "c")])
      , testCase "index" $
          test_parser exprUP "foo[bar]" (IndexE (VarE "foo") (VarE "bar"))
      , testCase "slice begin" $
          test_parser exprUP "foo[bar:]" (SliceE (VarE "foo") (Just $ VarE "bar") Nothing)
      , testCase "slice end" $
          test_parser exprUP "foo[:bar]" (SliceE (VarE "foo") Nothing (Just $ VarE "bar"))
      , testCase "slice both" $
          test_parser exprUP "foo[bar:baz]" (SliceE (VarE "foo") (Just $ VarE "bar") (Just $ VarE "baz"))
      , testCase "dot-member" $
          test_parser exprUP "foo.bar" (DotE (VarE "foo") "bar")
      , testCase "filter (no args)" $
          test_parser exprUP "foo|bar" (FilterE (VarE "foo") (VarE "bar") [] [])
      , testCase "filter (positional arg)" $
          test_parser exprUP "foo|bar(baz)" (FilterE (VarE "foo") (VarE "bar") [VarE "baz"] [])
      , testCase "filter (kw arg)" $
          test_parser exprUP "foo|bar(baz=quux)" (FilterE (VarE "foo") (VarE "bar") [] [("baz", VarE "quux")])
      ]
    ]
    , testGroup "Precedence"
      [ testCase "+ vs *" $
          test_parser exprUP
            "a * b + c * d"
            (PlusE
              (MulE (VarE "a") (VarE "b"))
              (MulE (VarE "c") (VarE "d"))
            )
      , testCase "* vs **" $
          test_parser exprUP
            "a ** b * c ** d"
            (MulE
              (PowerE (VarE "a") (VarE "b"))
              (PowerE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ~" $
          test_parser exprUP
            "a + b ~ c + d"
            (ConcatE
              (PlusE (VarE "a") (VarE "b"))
              (PlusE (VarE "c") (VarE "d"))
            )
      , testCase "== vs and" $
          test_parser exprUP
            "a == b and c == d"
            (AndE
              (EqualE (VarE "a") (VarE "b"))
              (EqualE (VarE "c") (VarE "d"))
            )
      , testCase "+ vs ." $
          test_parser exprUP
            "a + b[c]"
            (PlusE
              (VarE "a")
              (IndexE
                (VarE "b")
                (VarE "c")
              )
            )
      , testCase "is vs ==" $
          test_parser exprUP
            "a is b == c"
            (EqualE
              (IsE (VarE "a") (VarE "b") [] [])
              (VarE "c")
            )
      , testCase "is vs and" $
          test_parser exprUP
            "a is b and c is d"
            (AndE
              (IsE (VarE "a") (VarE "b") [] [])
              (IsE (VarE "c") (VarE "d") [] [])
            )
      , testCase "multiple filters" $
          test_parser exprUP
            "a|b|c"
            (FilterE
              (FilterE (VarE "a") (VarE "b") [] [])
              (VarE "c")
              [] []
            )
      ]
  , testGroup "Statement"
    [ testProperty "roundTrip" (prop_parserRoundTrip statementUP simplifyS)
    , testGroup "ImmediateS"
      [ testCase "plain immediate" $
          test_parser statementUP
            "Hello, world!"
            (ImmediateS $ Encoded "Hello, world!")
      , testCase "immediate with curly braces" $
          test_parser statementUP
            "Hello, {world}!"
            (ImmediateS $ Encoded "Hello, {world}!")
      ]
    , testGroup "InterpolationS"
      [ testCase "interpolation" $
          test_parser statementUP
            "{{ world }}"
            (InterpolationS (VarE "world"))
      , testCase "dict interpolation" $
          test_parser statementUP
            "{{ {\"bar\": { \"foo\": world }} }}"
            (InterpolationS (DictE [(StringLitE "bar", (DictE [(StringLitE "foo", VarE "world")]))]))
      ]
    , testCase "CommentS" $
        test_parser statementUP
          "{# world #}"
          (CommentS "world")
    , testGroup "IfS"
      [ testCase "if" $
          test_parser statementUP
            "{% if foo %}blah{% endif %}"
            (IfS (VarE "foo") (ImmediateS $ Encoded "blah") Nothing)
      , testCase "if-else" $
          test_parser statementUP
            "{% if foo %}blah{% else %}pizza{% endif %}"
            (IfS (VarE "foo") (ImmediateS $ Encoded "blah") (Just (ImmediateS $ Encoded "pizza")))
      , testCase "if-elif" $
          test_parser statementUP
            "{% if foo %}blah{% elif bar %}olives{% endif %}"
            (IfS (VarE "foo")
              (ImmediateS $ Encoded "blah")
              (Just $
                (IfS (VarE "bar")
                  (ImmediateS $ Encoded "olives")
                  Nothing)))
      , testCase "if-elif-else" $
          test_parser statementUP
            "{% if foo %}blah{% elif bar %}olives{% else %}pizza{% endif %}"
            (IfS (VarE "foo")
              (ImmediateS $ Encoded "blah")
              (Just $
                (IfS (VarE "bar")
                  (ImmediateS $ Encoded "olives")
                  (Just (ImmediateS $ Encoded "pizza")))))
      ]
    , testGroup "SetS, SetBlockS"
      [ testCase "set" $
          test_parser statementUP
            "{% set foo = bar %}"
            (SetS (SetVar "foo") (VarE "bar"))
      , testCase "set block" $
          test_parser statementUP
            "{% set foo %}foo{% endset %}"
            (SetBlockS (SetVar "foo") (ImmediateS $ Encoded "foo") Nothing)
      , testCase "set block with filter" $
          test_parser statementUP
            "{% set foo | bar %}foo{% endset %}"
            (SetBlockS (SetVar "foo") (ImmediateS $ Encoded "foo") (Just (VarE "bar")))
      ]
    , testGroup "ForS"
      [ testCase "for" $
          test_parser statementUP
            "{% for user in users %}{{ user.name }}{% endfor %}"
            (ForS
              Nothing (Identifier "user") (VarE "users")
              Nothing
              NotRecursive
              (InterpolationS (DotE (VarE "user") "name"))
              Nothing
            )
      , testCase "for-else" $
          test_parser statementUP
            "{% for user in users %}{{ user.name }}{% else %}Nope{% endfor %}"
            (ForS
              Nothing (Identifier "user") (VarE "users")
              Nothing
              NotRecursive
              (InterpolationS (DotE (VarE "user") "name"))
              (Just (ImmediateS (Encoded "Nope")))
            )
      , testCase "for-if" $
          test_parser statementUP
            "{% for user in users if user.name is defined %}{{ user.name }}{% endfor %}"
            (ForS
              Nothing (Identifier "user") (VarE "users")
              (Just (IsE (DotE (VarE "user") "name") (VarE "defined") [] []))
              NotRecursive
              (InterpolationS (DotE (VarE "user") "name"))
              Nothing
            )
      , testCase "for-if-else" $
          test_parser statementUP
            "{% for user in foo if not bar else baz %}{{ user.name }}{% endfor %}"
            (ForS
              Nothing (Identifier "user")
              (TernaryE (NotE (VarE "bar")) (VarE "foo") (VarE "baz"))
              Nothing
              NotRecursive
              (InterpolationS (DotE (VarE "user") "name"))
              Nothing
            )
      , testCase "for-if-recursive" $
          test_parser statementUP
            "{% for user in foo if bar recursive %}{{ user.name }}{% endfor %}"
            (ForS
              Nothing (Identifier "user")
              (VarE "foo")
              (Just (VarE "bar"))
              Recursive
              (InterpolationS (DotE (VarE "user") "name"))
              Nothing
            )
      ]
    , testGroup "IncludeS"
      [ testCase "include" $
          test_parser statementUP
            "{% include \"foo\" %}"
            (IncludeS (StringLitE "foo") RequireMissing WithContext)
      , testCase "include without context" $
          test_parser statementUP
            "{% include \"foo\" without context %}"
            (IncludeS (StringLitE "foo") RequireMissing WithoutContext)
      , testCase "include ignore missing" $
          test_parser statementUP
            "{% include \"foo\" ignore missing %}"
            (IncludeS (StringLitE "foo") IgnoreMissing WithContext)
      , testCase "include ignore missing without context" $
          test_parser statementUP
            "{% include \"foo\" ignore missing without context %}"
            (IncludeS (StringLitE "foo") IgnoreMissing WithoutContext)
      ]
    , testGroup "ImportS"
      [ testCase "plain import" $
          test_parser statementUP
            "{% import 'foo.html' %}"
            (ImportS (StringLitE "foo.html") Nothing Nothing RequireMissing WithoutContext)
      , testCase "qualified import" $
          test_parser statementUP
            "{% import 'foo.html' as foo %}"
            (ImportS (StringLitE "foo.html") (Just "foo") Nothing RequireMissing WithoutContext)
      , testCase "from import" $
          test_parser statementUP
            "{% from 'foo.html' import bar %}"
            (ImportS (StringLitE "foo.html") Nothing (Just [("bar", Nothing)]) RequireMissing WithoutContext)
      , testCase "from import qualified" $
          test_parser statementUP
            "{% from 'foo.html' as foo import bar %}"
            (ImportS (StringLitE "foo.html") (Just "foo") (Just [("bar", Nothing)]) RequireMissing WithoutContext)
      , testCase "from import as" $
          test_parser statementUP
            "{% from 'foo.html' import bar as baz %}"
            (ImportS (StringLitE "foo.html") Nothing (Just [("bar", Just "baz")]) RequireMissing WithoutContext)
      , testCase "from import qualified as" $
          test_parser statementUP
            "{% from 'foo.html' as foo import bar as baz %}"
            (ImportS (StringLitE "foo.html") (Just "foo") (Just [("bar", Just "baz")]) RequireMissing WithoutContext)
      , testCase "from import qualified as ignore missing" $
          test_parser statementUP
            "{% from 'foo.html' as foo import bar as baz ignore missing %}"
            (ImportS (StringLitE "foo.html") (Just "foo") (Just [("bar", Just "baz")]) IgnoreMissing WithoutContext)
      , testCase "from import qualified as with context" $
          test_parser statementUP
            "{% from 'foo.html' as foo import bar as baz with context %}"
            (ImportS (StringLitE "foo.html") (Just "foo") (Just [("bar", Just "baz")]) RequireMissing WithContext)
      , testCase "from import qualified as ignore missing with context" $
          test_parser statementUP
            "{% from 'foo.html' as foo import bar as baz ignore missing with context %}"
            (ImportS (StringLitE "foo.html") (Just "foo") (Just [("bar", Just "baz")]) IgnoreMissing WithContext)
      ]
    , testGroup "BlockS"
      [ testCase "block" $
          test_parser statementUP
            "{% block foo %}Hello{% endblock %}"
            (BlockS "foo" $ Block (ImmediateS . Encoded $ "Hello") NotScoped Optional)
      , testCase "block (repeat block name)" $
          test_parser statementUP
            "{% block foo %}Hello{% endblock foo %}"
            (BlockS "foo" $ Block (ImmediateS . Encoded $ "Hello") NotScoped Optional)
      , testCase "block (repeat wrong block name)" $
          test_parserEx statementUP
            "{% block foo %}Hello{% endblock bar %}"
            (Left . unlines $
                    [ "<input>:1:33:"
                    , "  |"
                    , "1 | {% block foo %}Hello{% endblock bar %}"
                    , "  |                                 ^^"
                    , "unexpected \"ba\""
                    , "expecting \"%}\", \"foo\", '+', '-', or white space"
                    ])
      ]
    , testGroup "WithS"
      [ testCase "plain" $
          test_parser statementUP
            "{% with %}hello{% endwith %}"
            (WithS [] (ImmediateS . Encoded $ "hello"))
      , testCase "one variable" $
          test_parser statementUP
            "{% with foo = bar %}hello{% endwith %}"
            (WithS [("foo", VarE "bar")] (ImmediateS . Encoded $ "hello"))
      , testCase "two variables" $
          test_parser statementUP
            "{% with foo = bar, baz = 123 %}hello{% endwith %}"
            (WithS [("foo", VarE "bar"), ("baz", IntLitE 123)] (ImmediateS . Encoded $ "hello"))
      ]
    ]
  , testGroup "Trimming"
    [ testCase "trim newline" $
        test_parser statementUP
          "{% set foo = bar %}\nHello"
          (GroupS [SetS (SetVar "foo") (VarE "bar"), ImmediateS . Encoded $ "Hello"])
    , testCase "trim only newline" $
        test_parser statementUP
          "{% set foo = bar %}\n Hello"
          (GroupS [SetS (SetVar "foo") (VarE "bar"), ImmediateS . Encoded $ " Hello"])
    , testCase "keep newline" $
        test_parser statementUP
          "{% set foo = bar +%}\nHello"
          (GroupS
            [ SetS (SetVar "foo") (VarE "bar")
            , ImmediateS . Encoded $ "\nHello"
            ])
    , testCase "force-trim newline" $
        test_parser statementUP
          "{% set foo = bar -%}\nHello"
          (GroupS [SetS (SetVar "foo") (VarE "bar"), ImmediateS . Encoded $ "Hello"])
    , testCase "keep leading" $
        test_parser statementUP
          "    {% set foo = bar %}"
          (GroupS [ImmediateS . Encoded $ "    ", SetS (SetVar "foo") (VarE "bar")])
    , testCase "force-keep leading" $
        test_parser statementUP
          "    {%+ set foo = bar %}"
          (GroupS [ImmediateS . Encoded $ "    ", SetS (SetVar "foo") (VarE "bar")])
    , testCase "trim leading" $
        test_parser statementUP
          "    {%- set foo = bar %}"
          (SetS (SetVar "foo") (VarE "bar"))
    , testCase "trim both" $
        test_parser statementUP
          "    {%- set foo = bar -%}\n"
          (SetS (SetVar "foo") (VarE "bar"))
    , testCase "trim leading after non-space" $
        test_parser statementUP
          "Hello,\n    {%- set foo = bar %}"
          (GroupS
            [ ImmediateS . Encoded $ "Hello,\n"
            , SetS (SetVar "foo") (VarE "bar")
            ])
    , testCase "trim trailing before non-space" $
        test_parser statementUP
          "{% set foo = bar -%} \nHello"
          (GroupS [SetS (SetVar "foo") (VarE "bar"), ImmediateS . Encoded $ "Hello"])
    , testCase "adjacent right and left trim" $
        test_parser statementUP
          "{% set foo = bar -%} \t \r\n\t {%- set baz = quux %}"
          (GroupS [SetS (SetVar "foo") (VarE "bar"), SetS (SetVar "baz") (VarE "quux")])
    ]
    , testGroup "Regressions"
      [ testCase "-%} after test" $
        test_parser statementUP
          "{%- for a in b if a -%}\na{%- endfor -%}"
          ( ForS
              Nothing
              "a"
              (VarE "b")
              (Just (VarE "a"))
              NotRecursive
              (ImmediateS (Encoded "a"))
              Nothing
          )
      ]
  ]

statementUP :: P Statement
statementUP = simplifyS . unPositionedS <$> statement

exprUP :: P Expr
exprUP = unPositionedE <$> expr

test_parser :: (Eq a, Show a) => P a -> Text -> a -> Assertion
test_parser p input expected =
  test_parserEx p input (Right expected)

test_parserEx :: (Eq a, Show a) => P a -> Text -> (Either String a) -> Assertion
test_parserEx p input expected = do
  assertBool
    ("expected:\n" ++ either id show expected ++ "\n" ++
     "actual:\n" ++ either id show actual)
    (expected == actual)
  where
    actual = parseGinger (p <* eof) "<input>" input

prop_parserRoundTrip :: (Eq a, Show a, Arbitrary a, RenderSyntax a) => P a -> (a -> a) -> a -> Property
prop_parserRoundTrip p postProcess v =
  let src = renderSyntaxText $ v
      expected = Right . postProcess $ v
      actual = either (Left . ImmediateString) (Right . postProcess) $
                  parseGinger (p <* eof) "<input>" src
  in
    counterexample (Text.unpack src) $
    expected === actual

newtype ImmediateString = ImmediateString String
  deriving (Eq)

instance Show ImmediateString where
  show (ImmediateString str) = str
