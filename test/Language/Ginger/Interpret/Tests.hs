{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Ginger.Interpret.Tests
where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Identity
import Control.Monad.State (gets)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace, isAlpha, isAlphaNum, chr)
import Data.Either (isRight)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (sortOn, intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Any (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8, Word16, Word32, Word64)
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

import Language.Ginger.AST
import Language.Ginger.Interpret
import Language.Ginger.Interpret.DefEnv (htmlEncode)
import Language.Ginger.Render
import Language.Ginger.RuntimeError
import Language.Ginger.TestUtils
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
    , testGroup "UnaryE"
      [ testProperty "Integer negation" (prop_unop @Integer UnopNegate negate)
      , testProperty "Double negation" (prop_unop @Double UnopNegate negate)
      , testProperty "Boolean not" (prop_unop @Bool UnopNot not)
      ]
    , testGroup "SliceE"
      [ testProperty "List at start" prop_sliceListStart
      , testProperty "List at end" prop_sliceListEnd
      , testProperty "List both sides" prop_sliceListBoth
      , testProperty "String at start" prop_sliceStringStart
      , testProperty "String at end" prop_sliceStringEnd
      , testProperty "String both sides" prop_sliceStringBoth
      , testProperty "Bytes at start" prop_sliceBytesStart
      , testProperty "Bytes at end" prop_sliceBytesEnd
      , testProperty "Bytes both sides" prop_sliceBytesBoth
      ]
    , testGroup "BinaryE"
      [ testProperty "Integer addition" (prop_binop @Integer BinopPlus (+))
      , testProperty "Integer subtraction" (prop_binop @Integer BinopMinus (-))
      , testProperty "Integer multiplication" (prop_binop @Integer BinopMul (*))
      , testProperty "Integer division" (prop_binopCond @Integer Just justNonzero BinopIntDiv div)
      , testProperty "Integer division by zero" prop_intDivByZero
      , testProperty "Integer modulo" (prop_binopCond @Integer Just justNonzero BinopMod mod)
      , testProperty "Integer power" (prop_binopCond @Integer @Integer justPositive justPositive BinopPower (^))

      , testProperty "Double addition" (prop_binop @Double BinopPlus (+))
      , testProperty "Double subtraction" (prop_binop @Double BinopMinus (-))
      , testProperty "Double multiplication" (prop_binop @Double BinopMul (*))
      , testProperty "Double division" (prop_binopCond @Double Just justNonzero BinopDiv (/))
      , testProperty "Double division by zero" prop_divByZero
      , testProperty "Double division to NaN" prop_divToNaN
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

      , testProperty "List membership Word8" (prop_binop @Word8 @[Word8] BinopIn elem)
      , testProperty "Dict membership Word8" (prop_binop @Word8 @(Map Word8 Word8) BinopIn (Map.member))
      , testProperty "List index Word8" (prop_binop @[Word8] @Word8 BinopIndex (flip $ safeAt . fromIntegral))
      , testProperty "Dict index Word8" (prop_binop @(Map Word8 Word8) @Word8 BinopIndex (flip Map.lookup))
      , testProperty "List index (Word8/Integer)" (prop_binop @[Integer] @Word8 BinopIndex (flip $ safeAt . fromIntegral))
      , testProperty "Dict index (Word8/Integer)" (prop_binop @(Map Word8 Integer) @Word8 BinopIndex (flip Map.lookup))

      , testProperty "Bytes concatenation" $
          prop_binopCond @ByteString (Just . BS.pack) (Just . BS.pack) BinopConcat (<>)
      , testProperty "String concatenation" $
          prop_binopCond @Text (Just . Text.pack) (Just . Text.pack) BinopConcat (<>)
      , testProperty "Int concatenation" $
          prop_binop @Int @Int @Text BinopConcat (\a b -> Text.show a <> Text.show b)
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
    , testGroup "FilterE"
        [ testProperty "even" $
            prop_eval (\i -> FilterE (IntLitE i) (VarE "even") [] []) (BoolV . even)
        , testProperty "odd" $
            prop_eval (\i -> FilterE (IntLitE i) (VarE "odd") [] []) (BoolV . odd)
        , testProperty "upper string" $
            prop_eval (\(ArbitraryText t) -> FilterE (StringLitE t) (VarE "upper") [] [])
                      (\(ArbitraryText t) -> StringV . Text.toUpper $ t)
        , testProperty "upper int" $
            prop_eval (\i -> FilterE (IntLitE i) (VarE "upper") [] [])
                      (StringV . Text.show)
        , testProperty "int from string" $
            prop_eval (\i -> FilterE (StringLitE $ Text.show i) (VarE "int") [] [])
                      IntV
        , testProperty "int from int" $
            prop_eval (\i -> FilterE (IntLitE i) (VarE "int") [] [])
                      IntV
        , testProperty "int from float" $
            prop_eval (\i -> FilterE (FloatLitE $ fromIntegral i + 0.25) (VarE "int") [] [])
                      IntV
        , testProperty "float from string" $
            prop_eval (\i -> FilterE (StringLitE $ Text.show i) (VarE "float") [] [])
                      FloatV
        , testProperty "float from float" $
            prop_eval (\i -> FilterE (IntLitE i) (VarE "float") [] [])
                      (FloatV . fromIntegral)
        , testProperty "float from int" $
            prop_eval (\i -> FilterE (FloatLitE i) (VarE "float") [] [])
                      FloatV
        , testProperty "list from list" $
            prop_eval (\xs -> FilterE (ListE $ map IntLitE xs) (VarE "list") [] [])
                      (ListV . map IntV)
        , testProperty "list from string" $
            prop_eval (\(ArbitraryText txt) ->
                        FilterE (StringLitE txt) (VarE "list") [] []
                      )
                      (\(ArbitraryText txt) ->
                        ListV . map (StringV . Text.singleton) . Text.unpack $ txt
                      )
        , testGroup "filesizeformat"
            [ testProperty "bytes" $
                prop_eval
                  (\(i :: Word8) ->
                    FilterE (IntLitE $ fromIntegral i) (VarE "filesizeformat") [] []
                  )
                  (\i -> StringV $ Text.show i <> "B")
            , testProperty "whole kilobytes" $
                prop_eval
                  (\(i :: Word8) ->
                    FilterE (IntLitE $ fromIntegral i * 1000 + 1000) (VarE "filesizeformat") [] []
                  )
                  (\i -> StringV $ Text.show (fromIntegral i + 1 :: Int) <> ".0kB")
            , testProperty "fractional kilobytes" $
                prop_eval
                  (\(i :: Word8, j :: Word8) ->
                    FilterE (IntLitE $ fromIntegral i * 1000 + 1000 + fromIntegral j) (VarE "filesizeformat") [] []
                  )
                  (\(i, j) ->
                    StringV $
                      Text.show (fromIntegral i + 1 :: Int) <> "." <>
                      Text.show (j `div` 100) <>
                      "kB"
                  )
            , testProperty "fractional megabytes" $
                prop_eval
                  (\(i :: Word8, j :: Word8) ->
                    FilterE (IntLitE $ fromIntegral i * 1000000 + 1000000 + fromIntegral j * 1000) (VarE "filesizeformat") [] []
                  )
                  (\(i, j) ->
                    StringV $
                      Text.show (fromIntegral i + 1 :: Int) <> "." <>
                      Text.show (j `div` 100) <>
                      "MB"
                  )
            , testProperty "fractional gigabytes" $
                prop_eval
                  (\(i :: Word8, j :: Word8) ->
                    FilterE (IntLitE $ fromIntegral i * 1000000000 + 1000000000 + fromIntegral j * 1000000) (VarE "filesizeformat") [] []
                  )
                  (\(i, j) ->
                    StringV $
                      Text.show (fromIntegral i + 1 :: Int) <> "." <>
                      Text.show (j `div` 100) <>
                      "GB"
                  )
            , testProperty "fractional kibibytes" $
                prop_eval
                  (\(i :: Word8, j :: Word8) ->
                    FilterE
                      (IntLitE $ fromIntegral i * 1024 + 1024 + fromIntegral j)
                      (VarE "filesizeformat")
                      [TrueE]
                      []
                  )
                  (\(i, j) ->
                    StringV $
                      Text.show (fromIntegral i + 1 :: Int) <> "." <>
                      Text.show (fromIntegral j * 1000 `div` 102400 :: Int) <>
                      "kiB"
                  )
            ]
        , testProperty "length (string)" $
            prop_eval
              (\(ArbitraryText t) ->
                  FilterE (StringLitE t) (VarE "length") [] []
              )
              (\(ArbitraryText t) -> toValue (Text.length t))
        , testProperty "length (list)" $
            prop_eval
              (\(PositiveInt i) ->
                  FilterE (ListE $ replicate i NoneE) (VarE "length") [] []
              )
              (\(PositiveInt i) -> toValue i)
        , testProperty "length (dict)" $
            prop_eval
              (\(PositiveInt (i :: Integer)) ->
                  FilterE
                    (DictE
                      [(StringLitE $ "item" <> Text.show n, NoneE) | n <- [1..i]])
                    (VarE "length")
                    [] []
              )
              (\(PositiveInt i) -> toValue i)

        , testGroup "escape"
            [ testProperty "string" $
                prop_eval
                  (\(ArbitraryText t) ->
                    FilterE (StringLitE t) (VarE "escape") [] []
                  )
                  (\(ArbitraryText t) ->
                      EncodedV (htmlEncode t)
                  )
            , testProperty "int" $
                prop_eval
                  (\i ->
                    FilterE (IntLitE i) (VarE "escape") [] []
                  )
                  (\i ->
                      EncodedV (Encoded $ Text.show i)
                  )
            ]

        , testGroup "first"
            [ testProperty "list" $
                prop_eval
                  (\(x, xs) ->
                    FilterE (ListE (IntLitE x : map IntLitE xs)) (VarE "first") [] []
                  )
                  (\(x, _) ->
                      IntV x
                  )
            , testProperty "empty list" $
                prop_eval
                  (\() ->
                    FilterE (ListE []) (VarE "first") [] []
                  )
                  (\() ->
                      NoneV
                  )
            , testProperty "string" $
                prop_eval
                  (\(x, ArbitraryText xs) ->
                    FilterE (StringLitE (Text.singleton x <> xs)) (VarE "first") [] []
                  )
                  (\(x, _) ->
                      StringV (Text.singleton x)
                  )
            , testProperty "empty string" $
                prop_eval
                  (\() ->
                    FilterE (StringLitE "") (VarE "first") [] []
                  )
                  (\() ->
                      StringV ""
                  )
            ]

        , testGroup "last"
            [ testProperty "list" $
                prop_eval
                  (\(x, xs) ->
                    FilterE (ListE (map IntLitE (xs ++ [x]))) (VarE "last") [] []
                  )
                  (\(x, _) ->
                      IntV x
                  )
            , testProperty "empty list" $
                prop_eval
                  (\() ->
                    FilterE (ListE []) (VarE "last") [] []
                  )
                  (\() ->
                      NoneV
                  )
            , testProperty "string" $
                prop_eval
                  (\(x, ArbitraryText xs) ->
                    FilterE (StringLitE (xs <> Text.singleton x)) (VarE "last") [] []
                  )
                  (\(x, _) ->
                      StringV (Text.singleton x)
                  )
            , testProperty "empty string" $
                prop_eval
                  (\() ->
                    FilterE (StringLitE "") (VarE "last") [] []
                  )
                  (\() ->
                      StringV ""
                  )
            ]

        , testProperty "dictsort" $
            prop_eval
              (\items ->
                FilterE
                  (DictE $
                    [ (StringLitE k, StringLitE v)
                    | (NonEmptyText k, ArbitraryText v)
                    <- items
                    ]
                  )
                  (VarE "dictsort")
                  []
                  [(Identifier "case_sensitive", TrueE)]
              )
              (\items ->
                toValue $
                  sortOn fst
                    [ (k, v)
                    | (NonEmptyText k, ArbitraryText v)
                    <- Map.toAscList . Map.fromList $ items
                    ]
              )

        , testGroup "map"
            [ testProperty "map(string)" $
                prop_eval
                  (\xs ->
                    FilterE
                      (ListE (map IntLitE xs))
                      (VarE "map")
                      [VarE "string"]
                      []
                  )
                  (\xs ->
                      ListV (map (StringV . Text.show) xs)
                  )
            , testProperty "map('string')" $
                prop_eval
                  (\xs ->
                    FilterE
                      (ListE (map IntLitE xs))
                      (VarE "map")
                      [StringLitE "string"]
                      []
                  )
                  (\xs ->
                      ListV (map (StringV . Text.show) xs)
                  )
            , testProperty "map(center, width)" $
                prop_eval
                  (\(xs, PositiveInt n) ->
                    FilterE
                      (ListE [ StringLitE x | ArbitraryText x <- xs ])
                      (VarE "map")
                      [VarE "center"]
                      [("width", IntLitE n)]
                  )
                  (\(xs, PositiveInt n) ->
                      let w = fromInteger n
                      in
                        ListV
                          [ if Text.length t >= w then
                              StringV t
                            else
                              let p = w - Text.length t
                                  pL = p `div` 2
                                  pR = p - pL
                              in StringV $ Text.replicate pL " " <> t <> Text.replicate pR " "
                          | ArbitraryText t <- xs
                          ]
                  )
            , testProperty "map('center', width)" $
                prop_eval
                  (\(xs, PositiveInt n) ->
                    FilterE
                      (ListE [ StringLitE x | ArbitraryText x <- xs ])
                      (VarE "map")
                      [StringLitE "center"]
                      [("width", IntLitE n)]
                  )
                  (\(xs, PositiveInt n) ->
                      let w = fromInteger n
                      in
                        ListV
                          [ if Text.length t >= w then
                              StringV t
                            else
                              let p = w - Text.length t
                                  pL = p `div` 2
                                  pR = p - pL
                              in StringV $ Text.replicate pL " " <> t <> Text.replicate pR " "
                          | ArbitraryText t <- xs
                          ]
                  )
            , testProperty "map(attribute=)" $
                prop_eval
                  (\(xs, name) ->
                    FilterE
                      (ListE [ DictE [(StringLitE $ identifierName name, IntLitE x)] | x <- xs ])
                      (VarE "map")
                      []
                      [("attribute", StringLitE $ identifierName name)]
                  )
                  (\(xs, _) ->
                      ListV (map IntV xs)
                  )
            , testProperty "map(attribute=x, default=y)" $
                prop_eval
                  (\(xs, name, otherName, ArbitraryText dummyVal, ArbitraryText defval) ->
                    FilterE
                      (ListE $ intersperse
                        (DictE [(StringLitE $ identifierName otherName, StringLitE dummyVal)])
                        [ DictE [(StringLitE $ identifierName name, IntLitE x)]
                        | x <- xs
                        ])
                      (VarE "map")
                      []
                      [ ("attribute", StringLitE $ identifierName name)
                      , ("default", StringLitE defval)
                      ]
                  )
                  (\(xs, _, _, _, ArbitraryText defval) ->
                      ListV $ intersperse (StringV defval) (map IntV xs)
                  )
            ]

        , testGroup "join"
            [ testProperty "simple" $
                prop_eval
                  (\(ArbitraryText t, ArbitraryText u) ->
                      FilterE (ListE [StringLitE t, StringLitE u]) (VarE "join") [] []
                  )
                  (\(ArbitraryText t, ArbitraryText u) ->
                    StringV (t <> u)
                  )
            , testProperty "ints" $
                prop_eval
                  (\(i, j) ->
                      FilterE (ListE [IntLitE i, IntLitE j]) (VarE "join") [] []
                  )
                  (\(i, j) ->
                    StringV (Text.show i <> Text.show j)
                  )
            , testProperty "with sep" $
                prop_eval
                  (\(ArbitraryText t, ArbitraryText u, ArbitraryText v, ArbitraryText s) ->
                      FilterE
                        (ListE [StringLitE t, StringLitE u, StringLitE v])
                        (VarE "join")
                        [StringLitE s]
                        []
                  )
                  (\(ArbitraryText t, ArbitraryText u, ArbitraryText v, ArbitraryText s) ->
                    StringV (t <> s <> u <> s <> v)
                  )
            ]
        , testGroup "default"
            [ testProperty "undefined" $
                prop_eval (\i -> FilterE (VarE "something_undefined") (VarE "default") [(IntLitE i)] []) IntV
            , testProperty "none" $
                prop_eval (\i -> FilterE NoneE (VarE "default") [(IntLitE i)] []) IntV
            , testProperty "boolean false" $
                prop_eval (\i -> FilterE FalseE (VarE "default") [(IntLitE i)] []) (const FalseV)
            , testProperty "boolean false, boolean mode" $
                prop_eval (\i -> FilterE FalseE (VarE "default") [(IntLitE i), TrueE] []) IntV
            ]
        , testGroup "center string"
            [ testProperty "width as vararg" $
                prop_eval (\((ArbitraryText t), w) ->
                              FilterE (StringLitE t) (VarE "center") [IntLitE $ fromIntegral w] []
                          )
                          (\((ArbitraryText t), w) ->
                              if Text.length t >= w then
                                StringV t
                              else
                                let p = w - Text.length t
                                    pL = p `div` 2
                                    pR = p - pL
                                in StringV $ Text.replicate pL " " <> t <> Text.replicate pR " "
                          )
            , testProperty "width as kwarg" $
                prop_eval (\((ArbitraryText t), w) ->
                              FilterE (StringLitE t) (VarE "center") [] [("width", IntLitE $ fromIntegral w)]
                          )
                          (\((ArbitraryText t), w) ->
                              if Text.length t >= w then
                                StringV t
                              else
                                let p = w - Text.length t
                                    pL = p `div` 2
                                    pR = p - pL
                                in StringV $ Text.replicate pL " " <> t <> Text.replicate pR " "
                          )
            ]
        , testGroup "round"
            [ testProperty "precision 0" $
                prop_eval
                  (\(PositiveInt i) ->
                      FilterE
                        (FloatLitE $ fromIntegral i / 10)
                        (VarE "round")
                        []
                        [("precision", IntLitE 0), ("method", StringLitE "floor")]
                  )
                  (\(PositiveInt i) ->
                      FloatV . fromIntegral $ (i `div` 10 :: Integer)
                  )
            , testProperty "precision 2, floor" $
                prop_eval
                  (\(PositiveInt i) ->
                      FilterE
                        (FloatLitE $ fromIntegral i / 1000)
                        (VarE "round")
                        []
                        [("precision", IntLitE 2), ("method", StringLitE "floor")]
                  )
                  (\(PositiveInt i) ->
                      FloatV $ fromIntegral (i `div` 10 :: Integer) / 100
                  )
            ]

        , testProperty "items" $
            prop_eval
              (\pairs ->
                  FilterE
                    (DictE [ (StringLitE k, StringLitE v) | (ArbitraryText k, ArbitraryText v) <- pairs ])
                    (VarE "items")
                    [] []
              )
              (\pairs -> ListV [ ListV [StringV k, StringV v] | (ArbitraryText k, ArbitraryText v) <- Map.toAscList . Map.fromList $ pairs ])

        , testProperty "batch" $
            prop_eval
              (\(PositiveInt i, PositiveInt j) -> FilterE (ListE (replicate (i * j) NoneE)) (VarE "batch") [IntLitE $ fromIntegral j] [])
              (\(PositiveInt i, PositiveInt j) -> ListV (replicate i (ListV (replicate j NoneV))))
        , testProperty "batch with fill" $
            prop_eval
              (\(PositiveInt i, PositiveInt j, PositiveInt x, f) ->
                  FilterE
                    (ListE (replicate (i * j + x `mod` j) NoneE))
                    (VarE "batch")
                    [IntLitE $ fromIntegral j, IntLitE f]
                    []
              )
              (\(PositiveInt i, PositiveInt j, PositiveInt x, f) ->
                  ListV (
                    replicate i (ListV (replicate j NoneV)) ++
                    if x `mod` j == 0 then
                      []
                    else
                      [ListV (replicate (x `mod` j) NoneV ++ replicate (j - x `mod` j) (IntV f))]
                  )
              )

        ]
    , testGroup "DotE"
      [ testGroup "StringV"
        [ testProperty "string length" $
            prop_attr "length"
              (\(ArbitraryText t) -> StringLitE t)
              (\(ArbitraryText t) -> Text.length t)
        , testProperty "upper string" $
            prop_string_method "upper" (const []) Text.toUpper
        , testProperty "lower string" $
            prop_string_method "lower" (const []) Text.toLower
        , testProperty "capitalize string" $
            prop_string_method "capitalize" (const []) Text.toTitle
        , testProperty "title string" $
            prop_string_method "title" (const []) Text.toTitle
        , testProperty "casefold string" $
            prop_string_method "casefold" (const []) Text.toCaseFold
        , testProperty "string isalpha" $
            prop_method "isalpha"
              (\(ArbitraryText t) -> StringLitE (Text.filter isAlpha t))
              (const [])
              (\(ArbitraryText t) -> if Text.null (Text.filter isAlpha t) then TrueV else TrueV)
        , testProperty "string isalnum" $
            prop_method "isalnum"
              (\(ArbitraryText t) -> StringLitE (Text.filter isAlphaNum t))
              (const [])
              (\(ArbitraryText t) -> if Text.null (Text.filter isAlphaNum t) then TrueV else TrueV)
        , testProperty "string isascii" $
            prop_method "isascii"
              (\(ArbitraryByteString chars) ->
                  StringLitE . Text.pack . map (chr . fromIntegral . (.&. 0x7F)) $ BS.unpack chars
              )
              (const [])
              (const TrueV)
        , testProperty "string isascii" $
            prop_method "isascii"
              (\(ArbitraryByteString chars) ->
                  StringLitE . Text.pack . (++ [chr 128]) . map (chr . fromIntegral . (.&. 0x7F)) $ BS.unpack chars
              )
              (const [])
              (const FalseV)
        , testProperty "count string" $
            prop_method "count"
              (\(NonEmptyText t, NonEmptyText f, PositiveInt p) ->
                  StringLitE (Text.intercalate f . replicate (fromIntegral p) $ t)
              )
              (\(NonEmptyText t, NonEmptyText _, PositiveInt _) ->
                [ StringLitE t ]
              )
              (\(NonEmptyText t, NonEmptyText f, PositiveInt p) ->
                -- The second part accounts for cases where the needle (t) is a
                -- substring of the filler (f).
                IntV $
                  p + (fromIntegral (Text.count t f) * (p - 1))
              )
        , testProperty "center string" $
            prop_method "center"
              (\(ArbitraryText t, _) -> StringLitE t)
              (\(ArbitraryText t, PositiveInt p) -> [IntLitE (p + p + fromIntegral (Text.length t))])
              (\(ArbitraryText t, PositiveInt p) ->
                  let padding = Text.replicate (fromInteger p) " "
                  in StringV $ padding <> t <> padding
              )
        , testProperty "center string with fillchar" $
            prop_method "center"
              (\(ArbitraryText t, _, _) -> StringLitE t)
              (\(ArbitraryText t, PositiveInt p, c) ->
                  [ IntLitE (p + p + fromIntegral (Text.length t))
                  , StringLitE (Text.singleton c)
                  ])
              (\(ArbitraryText t, PositiveInt p, c) ->
                  let padding = Text.replicate (fromInteger p) (Text.singleton c)
                  in StringV $ padding <> t <> padding
              )
        , testProperty "encode string (UTF-8)" $
            prop_method "encode"
              (\(ArbitraryText t) -> StringLitE t)
              (const [])
              (\(ArbitraryText t) -> BytesV . Text.encodeUtf8 $ t)
        , testProperty "encode string (UTF-16LE)" $
            prop_method "encode"
              (\(ArbitraryText t) -> StringLitE t)
              (const [StringLitE "utf-16-LE"])
              (\(ArbitraryText t) -> BytesV . Text.encodeUtf16LE $ t)
        , testProperty "encode string (UTF-16BE)" $
            prop_method "encode"
              (\(ArbitraryText t) -> StringLitE t)
              (const [StringLitE "utf-16-BE"])
              (\(ArbitraryText t) -> BytesV . Text.encodeUtf16BE $ t)
        , testProperty "encode string (UTF-32LE)" $
            prop_method "encode"
              (\(ArbitraryText t) -> StringLitE t)
              (const [StringLitE "utf-32-LE"])
              (\(ArbitraryText t) -> BytesV . Text.encodeUtf32LE $ t)
        , testProperty "encode string (UTF-32BE)" $
            prop_method "encode"
              (\(ArbitraryText t) -> StringLitE t)
              (const [StringLitE "utf-32-BE"])
              (\(ArbitraryText t) -> BytesV . Text.encodeUtf32BE $ t)
        , testProperty "encode string (ASCII)" $
            prop_method "encode"
              (\(ArbitraryByteString b) -> StringLitE (Text.decodeUtf8 $ BS.filter (< 128) b))
              (const [StringLitE "ASCII"])
              (\(ArbitraryByteString b) -> BytesV (BS.filter (< 128) b))
        , testProperty "string startswith" $
            prop_method "startswith"
              (\(NonEmptyText needle, NonEmptyText p) -> StringLitE (needle <> p))
              (\(NonEmptyText needle, _) -> [StringLitE needle])
              (\_ -> TrueV)
        , testProperty "string startswith ranged" $
            prop_method "startswith"
              (\(NonEmptyText needle, NonEmptyText p, NonEmptyText b, NonEmptyText a) ->
                StringLitE (b <> needle <> p <> a))
              (\(NonEmptyText needle, NonEmptyText p, NonEmptyText b, _) ->
                [ StringLitE needle
                , IntLitE . fromIntegral $ Text.length b
                , IntLitE . fromIntegral $ Text.length b + Text.length needle + Text.length p
                ]
              )
              (\_ -> TrueV)
        , testProperty "string endswith" $
            prop_method "endswith"
              (\(NonEmptyText needle, NonEmptyText p) -> StringLitE (p <> needle))
              (\(NonEmptyText needle, _) -> [StringLitE needle])
              (\_ -> TrueV)
        , testProperty "string split (no sep)" $
            prop_method "split"
              (\(NonEmptyText _sep, NonEmptyText item) ->
                StringLitE item
              )
              (\(NonEmptyText sep, NonEmptyText _item) ->
                [StringLitE sep]
              )
              (\(NonEmptyText sep, NonEmptyText item) ->
                if sep `Text.isInfixOf` item then
                  ListV (map StringV $ Text.splitOn sep item)
                else
                  ListV [StringV item]
              )
        , testProperty "string split" $
            prop_method "split"
              (\(NonEmptyText sep, NonEmptyText item, PositiveInt i) ->
                StringLitE (Text.intercalate sep $ replicate (i + 1) item)
              )
              (\(NonEmptyText sep, NonEmptyText _item, PositiveInt _i) ->
                [StringLitE sep]
              )
              (\(NonEmptyText sep, NonEmptyText item, PositiveInt i) ->
                if sep `Text.isInfixOf` item then
                  ListV $ concat $ replicate (i + 1) (map StringV $ Text.splitOn sep item)
                else
                  ListV $ replicate (i + 1) (StringV item)
              )
        , testProperty "string splitlines" $
            prop_method "splitlines"
              (\(NonEmptyText item, PositiveInt i) ->
                StringLitE (Text.unlines $ replicate i (Text.filter (not . isControl) item))
              )
              (const [])
              (\(NonEmptyText item, PositiveInt i) ->
                ListV $ replicate i (StringV (Text.filter (not . isControl) item))
              )
        , testProperty "string join" $
            prop_method "join"
              (\(NonEmptyText sep, NonEmptyText _item, PositiveInt _i) ->
                StringLitE sep
              )
              (\(NonEmptyText _sep, NonEmptyText item, PositiveInt i) ->
                [ListE $ replicate i (StringLitE item)]
              )
              (\(NonEmptyText sep, NonEmptyText item, PositiveInt i) ->
                StringV $ Text.intercalate sep $ replicate i item
              )

        , testProperty "string replace" $
            prop_method "replace"
              (\() ->
                StringLitE "Hello, world!"
              )
              (const [StringLitE "world", StringLitE "Tobias"])
              (\() ->
                StringV "Hello, Tobias!"
              )
        , testProperty "string replace limited 1" $
            prop_method "replace"
              (\() ->
                StringLitE "Hello, world! How are you!"
              )
              (const [StringLitE "!", StringLitE "?", IntLitE 1])
              (\() ->
                StringV "Hello, world? How are you!"
              )
        , testProperty "string replace limited 2" $
            prop_method "replace"
              (\() ->
                StringLitE "Hello, world! How are you!"
              )
              (const [StringLitE "!", StringLitE "?", IntLitE 3])
              (\() ->
                StringV "Hello, world? How are you?"
              )

        , testProperty "string strip" $
            prop_method "strip"
              (\(NonEmptyText item, PositiveInt i, PositiveInt j) ->
                StringLitE (Text.replicate i " " <> item <> Text.replicate j " ")
              )
              (const [])
              (\(NonEmptyText item, _, _) ->
                StringV (Text.strip item)
              )
        , testProperty "string lstrip" $
            prop_method "lstrip"
              (\(NonEmptyText item, PositiveInt i, PositiveInt j) ->
                StringLitE (Text.replicate i " " <> item <> Text.replicate j " ")
              )
              (const [])
              (\(NonEmptyText item, _, PositiveInt j) ->
                if Text.all isSpace item then
                  StringV ""
                else
                  StringV (Text.stripStart item <> Text.replicate j " ")
              )
        , testProperty "string rstrip" $
            prop_method "rstrip"
              (\(NonEmptyText item, PositiveInt i, PositiveInt j) ->
                StringLitE (Text.replicate i " " <> item <> Text.replicate j " ")
              )
              (const [])
              (\(NonEmptyText item, PositiveInt i, _) ->
                if Text.all isSpace item then
                  StringV ""
                else
                  StringV (Text.replicate i " " <> Text.stripEnd item)
              )
        ]
      ]
    , testGroup "IsE"
      [ testGroup "defined"
        [ testProperty "is defined true" prop_isDefinedTrue
        , testProperty "is defined false" prop_isDefinedFalse
        , testProperty "is defined dict" prop_isDefinedTrueDict
        ]
      , testGroup "boolean"
        [ testProperty "boolean is boolean" $ prop_is @Bool "boolean" True
        , testProperty "integer is not boolean" $ prop_is @Integer "boolean" False
        , testProperty "float is not boolean" $ prop_is @Double "boolean" False
        , testProperty "none is not boolean" $ prop_is @() "boolean" False
        , testProperty "text is not boolean" $ prop_is @ArbitraryText "boolean" False
        , testProperty "list is not boolean" $ prop_is @[Bool] "boolean" False
        , testProperty "dict is not boolean" $ prop_is @(Map Bool Bool) "boolean" False
        ]
      , testGroup "integer"
        [ testProperty "boolean is not int" $ prop_is @Bool "integer" False
        , testProperty "integer is int" $ prop_is @Integer "integer" True
        , testProperty "float is not int" $ prop_is @Double "integer" False
        , testProperty "none is not int" $ prop_is @() "integer" False
        , testProperty "text is not int" $ prop_is @ArbitraryText "integer" False
        , testProperty "list is not int" $ prop_is @[Bool] "integer" False
        , testProperty "dict is not int" $ prop_is @(Map Bool Bool) "integer" False
        ]
      , testGroup "float"
        [ testProperty "boolean is not float" $ prop_is @Bool "float" False
        , testProperty "integer is not float" $ prop_is @Integer "float" False
        , testProperty "float is float" $ prop_is @Double "float" True
        , testProperty "none is not float" $ prop_is @() "float" False
        , testProperty "text is not float" $ prop_is @ArbitraryText "float" False
        , testProperty "list is not float" $ prop_is @[Bool] "float" False
        , testProperty "dict is not float" $ prop_is @(Map Bool Bool) "float" False
        ]
      , testGroup "none"
        [ testProperty "boolean is not none" $ prop_is @Bool "none" False
        , testProperty "integer is not none" $ prop_is @Integer "none" False
        , testProperty "float is none" $ prop_is @Double "none" False
        , testProperty "none is none" $ prop_is @() "none" True
        , testProperty "text is not none" $ prop_is @ArbitraryText "none" False
        , testProperty "list is not none" $ prop_is @[Bool] "none" False
        , testProperty "dict is not none" $ prop_is @(Map Bool Bool) "none" False
        ]
      , testGroup "true"
        [ testProperty "integer is not true" $ prop_is @Integer "true" False
        , testProperty "float is true" $ prop_is @Double "true" False
        , testProperty "none is not true" $ prop_is @() "true" False
        , testProperty "text is not true" $ prop_is @ArbitraryText "true" False
        , testProperty "list is not true" $ prop_is @[Bool] "true" False
        , testProperty "dict is not true" $ prop_is @(Map Bool Bool) "true" False
        ]
      , testGroup "false"
        [ testProperty "integer is not false" $ prop_is @Integer "false" False
        , testProperty "float is false" $ prop_is @Double "false" False
        , testProperty "none is not false" $ prop_is @() "false" False
        , testProperty "text is not false" $ prop_is @ArbitraryText "false" False
        , testProperty "list is not false" $ prop_is @[Bool] "false" False
        , testProperty "dict is not false" $ prop_is @(Map Bool Bool) "false" False
        ]
      , testGroup "filter"
        [ testProperty "even is filter" $ prop_is "filter" True (StringV "even" :: Value Identity)
        , testProperty "default is filter" $ prop_is "filter" True (StringV "default" :: Value Identity)
        , testProperty "number is not filter" $ prop_is "filter" False (StringV "number" :: Value Identity)
        , testProperty "true is not filter" $ prop_is "filter" False (StringV "true" :: Value Identity)
        , testProperty "none is not filter" $ prop_is "filter" False (StringV "none" :: Value Identity)
        ]
      , testGroup "test"
        [ testProperty "even is test" $ prop_is "test" True (StringV "even" :: Value Identity)
        , testProperty "default is not test" $ prop_is "test" False (StringV "default" :: Value Identity)
        , testProperty "number is test" $ prop_is "test" True (StringV "number" :: Value Identity)
        , testProperty "true is test" $ prop_is "test" True (StringV "true" :: Value Identity)
        , testProperty "none is test" $ prop_is "test" True (StringV "none" :: Value Identity)
        ]
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
      , testProperty "identity" prop_callIdentity
      , testProperty "echo" prop_callEcho
      , testProperty "ginger macro" prop_callMacro
      ]
    , testGroup "FilterS"
      [
      ]
    , testGroup "IncludeS"
      [ testProperty "plain" prop_include
      , testProperty "into" prop_includeInto
      , testProperty "macro" prop_includeMacro
      , testProperty "macro without context" prop_includeMacroWithoutContext
      , testProperty "set" prop_includeSet
      , testProperty "without context" prop_includeWithoutContext
      , testProperty "with context" prop_includeWithContext
      ]
    , testGroup "ImportS"
      [ testProperty "value" prop_importValue
      , testProperty "macro" prop_importMacro
      , testProperty "alias" prop_importValueAlias
      , testProperty "with context" prop_importWithContext
      , testProperty "without context" prop_importWithoutContext
      , testProperty "explicit" prop_importExplicit
      ]
    , testGroup "extends"
      [ testProperty "simple" prop_extendSimple
      , testProperty "super" prop_extendSuper
      , testProperty "with context" prop_extendWithContext
      , testProperty "without context" prop_extendWithoutContext
      ]
    ]
  ]

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

--------------------------------------------------------------------------------
-- Expression properties
--------------------------------------------------------------------------------

prop_sliceListStart :: [Int]
                        -> Int
                        -> Property
prop_sliceListStart items start =
  let expected = if start < 0 then
                    toValue $ drop (length items + start) items
                 else
                    toValue $ drop start items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                eval (SliceE (VarE "items") (Just $ VarE "start") Nothing)
  in
    actual === expected

prop_sliceListEnd :: [Int]
                  -> Int
                  -> Property
prop_sliceListEnd items end =
  let expected = if end < 0 then
                    toValue $ take (length items + end) items
                 else
                    toValue $ take end items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") Nothing (Just $ VarE "end"))
  in
    actual === expected

prop_sliceListBoth :: [Int]
                  -> Int
                  -> Int
                  -> Property
prop_sliceListBoth items start end =
  let start' = if start < 0 then length items + start else start
      end' = if end < 0 then length items - start' + end else end
      expected = toValue . take end' . drop start' $ items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") (Just $ VarE "start") (Just $ VarE "end"))
  in
    actual === expected

prop_sliceStringStart :: ArbitraryText
                        -> Int
                        -> Property
prop_sliceStringStart (ArbitraryText items) start =
  let expected = if start < 0 then
                    toValue $ Text.drop (Text.length items + start) items
                 else
                    toValue $ Text.drop start items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                eval (SliceE (VarE "items") (Just $ VarE "start") Nothing)
  in
    actual === expected

prop_sliceStringEnd :: ArbitraryText
                  -> Int
                  -> Property
prop_sliceStringEnd (ArbitraryText items) end =
  let expected = if end < 0 then
                    toValue $ Text.take (Text.length items + end) items
                 else
                    toValue $ Text.take end items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") Nothing (Just $ VarE "end"))
  in
    actual === expected

prop_sliceStringBoth :: ArbitraryText
                  -> Int
                  -> Int
                  -> Property
prop_sliceStringBoth (ArbitraryText items) start end =
  let start' = if start < 0 then Text.length items + start else start
      end' = if end < 0 then Text.length items - start' + end else end
      expected = toValue . Text.take end' . Text.drop start' $ items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") (Just $ VarE "start") (Just $ VarE "end"))
  in
    actual === expected

prop_sliceBytesStart :: ArbitraryByteString
                        -> Int
                        -> Property
prop_sliceBytesStart (ArbitraryByteString items) start =
  let expected = if start < 0 then
                    toValue $ BS.drop (BS.length items + start) items
                 else
                    toValue $ BS.drop start items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                eval (SliceE (VarE "items") (Just $ VarE "start") Nothing)
  in
    actual === expected

prop_sliceBytesEnd :: ArbitraryByteString
                  -> Int
                  -> Property
prop_sliceBytesEnd (ArbitraryByteString items) end =
  let expected = if end < 0 then
                    toValue $ BS.take (BS.length items + end) items
                 else
                    toValue $ BS.take end items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") Nothing (Just $ VarE "end"))
  in
    actual === expected

prop_sliceBytesBoth :: ArbitraryByteString
                  -> Int
                  -> Int
                  -> Property
prop_sliceBytesBoth (ArbitraryByteString items) start end =
  let start' = if start < 0 then BS.length items + start else start
      end' = if end < 0 then BS.length items - start' + end else end
      expected = toValue . BS.take end' . BS.drop start' $ items
      actual = runGingerIdentity $ do
                setVar "items" (toValue items)
                setVar "start" (toValue start)
                setVar "end" (toValue end)
                eval (SliceE (VarE "items") (Just $ VarE "start") (Just $ VarE "end"))
  in
    actual === expected

prop_unop :: (ToValue a Identity, ToValue b Identity)
           => UnaryOperator
           -> (a -> b)
           -> a
           -> Property
prop_unop = prop_unopCond Just

prop_unopCond :: (ToValue a' Identity, ToValue b Identity)
               => (a -> Maybe a')
               -> UnaryOperator
               -> (a' -> b)
               -> a
               -> Property
prop_unopCond fX unop f x' =
  let x = fX x'
      resultG = runGingerIdentity $ do
                  setVar "a" (toValue x)
                  eval (UnaryE unop (VarE "a"))
      resultH = toValue $ f <$> x
  in
    isJust x ==>
    resultG === resultH

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

prop_intDivByZero :: Integer -> Property
prop_intDivByZero i =
  let result = runGingerIdentityEither $ do
                  setVar "i" (toValue i)
                  eval (BinaryE BinopIntDiv (IntLitE i) (IntLitE 0))
  in
    i /= 0 ==>
    result === leftPRE (NumericError (Just "//") (Just "division by zero"))

prop_divByZero :: Double -> Property
prop_divByZero d =
  let result = runGingerIdentityEither $ do
                  setVar "d" (toValue d)
                  eval (BinaryE BinopDiv (FloatLitE d) (FloatLitE 0))
  in
    d /= 0 ==>
    result === leftPRE (NumericError (Just "/") (Just "division by zero"))

prop_divToNaN :: Property
prop_divToNaN =
  let result = runGingerIdentityEither $ do
                  eval (BinaryE BinopDiv (FloatLitE 0) (FloatLitE 0))
  in
    result === leftPRE (NumericError (Just "/") (Just "not a number"))

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
    resultG === leftPRE (NotInScopeError (Just $ identifierName name2))

prop_nativeNullary :: Identifier -> Integer -> Property
prop_nativeNullary varName constVal =
  let fVal = ProcedureV . NativeProcedure $
              const . const . pure @Identity . Right . toValue $ constVal
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
    counterexample (show fVal) $
    counterexample (show argVal) $
    varName /= argVarName ==>
    result === argVal

prop_userNullary :: Identifier -> Expr -> Property
prop_userNullary varName bodyExpr =
  let fVal = ProcedureV $ GingerProcedure mempty [] bodyExpr
      resultCall = runGingerIdentityEither $ do
                    setVar varName fVal
                    eval $ CallE (VarE varName) [] []
      resultDirect = runGingerIdentityEither $ do
                        eval bodyExpr
  in
    resultCall === resultDirect

prop_isDefinedTrue :: Identifier -> Integer -> Property
prop_isDefinedTrue name val =
  let result = runGingerIdentity $ do
                setVar name (toValue val)
                eval $ IsE (VarE name) (VarE "defined") [] []
  in
    result === TrueV

prop_isDefinedFalse :: Identifier -> Property
prop_isDefinedFalse name =
  let result = runGingerIdentity $ do
                eval $ IsE (VarE name) (VarE "defined") [] []
  in
    not (name `Map.member` envVars (defEnv @Identity)) ==>
    result === FalseV

prop_isDefinedTrueDict :: Identifier -> Identifier -> Integer -> Property
prop_isDefinedTrueDict name selector val =
  let result = runGingerIdentity $ do
                setVar name (dictV [toScalar (identifierName selector) .= val])
                eval $ IsE (IndexE (VarE name) (StringLitE $ identifierName selector)) (VarE "defined") [] []
  in
    result === TrueV

prop_is :: ToValue a Identity => Identifier -> Bool -> a -> Property
prop_is testName expected val =
  let testE = case testName of
                "none" -> NoneE
                "true" -> TrueE
                "false" -> FalseE
                t -> VarE t
      result = runGingerIdentity $ do
                  setVar "v" (toValue val)
                  eval $ IsE (VarE "v") testE [] []
  in
    result === BoolV expected

prop_eval :: (Arbitrary a, Show a, Show e, Eval Identity e) => (a -> e) -> (a -> Value Identity) -> a -> Property
prop_eval mkEvaluable mkExpected x =
  let e = mkEvaluable x
      expected = mkExpected x
      result = runGingerIdentity $ eval e
  in
    counterexample (show e) $
    result === expected

prop_attr :: (Arbitrary a, Show a, ToValue b Identity)
            => Identifier
            -> (a -> Expr)
            -> (a -> b)
            -> a
            -> Property
prop_attr methodName mkSelf mkExpected =
  prop_eval (\t' -> DotE (mkSelf t') methodName)
            (toValue . mkExpected)

prop_method :: (Arbitrary a, Show a)
            => Identifier
            -> (a -> Expr)
            -> (a -> [Expr])
            -> (a -> Value Identity)
            -> a
            -> Property
prop_method methodName mkSelf mkArgs mkExpected t =
  counterexample (show $ mkArgs t) $
  prop_eval (\t' -> CallE (DotE (mkSelf t) methodName) (mkArgs t') [])
            mkExpected
            t

prop_string_method :: Identifier
                   -> (Text -> [Expr])
                   -> (Text -> Text)
                   -> ArbitraryText
                   -> Property
prop_string_method methodName mkArgs mkExpected =
  prop_method methodName
    (\(ArbitraryText t) -> StringLitE t)
    (\(ArbitraryText t) -> mkArgs t)
    (\(ArbitraryText t) -> StringV . mkExpected $ t)

--------------------------------------------------------------------------------
-- Statement properties
--------------------------------------------------------------------------------

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
    either (const True) (not . getAny . traverseValue (Any . isProcedure)) resultE ==>
    resultS === resultE

prop_ifStatementOutput :: Bool -> Statement -> Statement -> Property
prop_ifStatementOutput cond yes no =
  let resultYes = runGingerIdentityEither (eval yes)
      resultNo = runGingerIdentityEither (eval no)
      resultE = if cond then resultYes else resultNo
      resultIf = runGingerIdentityEither (eval $ IfS (BoolE cond) yes (Just no))
      hasProcedure = case resultE of
                        Left _ -> False
                        Right v -> getAny (traverseValue (Any . isProcedure) v)
  in
    -- exclude procedures, because we cannot compare those
    not hasProcedure ==>
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
      expected = StringV . Text.pack . mconcat $
                  [ show k ++ v | (k, v) <- zip [(0 :: Int) ..] strItems ]
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
    varName /= keyName ==>
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
  let resultDirect = runGingerIdentityEither $ do
                      eval body
      resultCall = runGingerIdentityEither $ do
                      setVar "f" $ ProcedureV (GingerProcedure mempty [] body)
                      eval $ CallS "f" [] [] (InterpolationS NoneE)
      cat = case resultDirect of
              Right {} -> "OK"
              Left err -> unwords . take 1 . words $ show err
  in
    label cat $
    either (const True) (not . getAny . traverseValue (Any . isProcedure)) resultDirect ==>
    resultCall === resultDirect

prop_callIdentity :: Expr -> Property
prop_callIdentity body =
  -- Some trickery is needed to make sure that if anything inside @body@
  -- references a variable @f@, it points to the same thing in both cases.
  let body' = StatementE $ GroupS
                              [ SetS "f" NoneE
                              , InterpolationS body
                              ]
      resultDirect = runGingerIdentityEither $
                      eval body'
      resultCall = runGingerIdentityEither $ do
                      setVar "f" $ toValue (id :: Value Identity -> Value Identity)
                      eval $ CallS "f" [body'] [] (InterpolationS NoneE)
      cat = case resultDirect of
              Right {} -> "OK"
              Left err -> unwords . take 1 . words $ show err
  in
    label cat $
    either (const True) (not . getAny . traverseValue (Any . isProcedure)) resultDirect ==>
    resultCall === resultDirect

prop_callEcho :: Expr -> Property
prop_callEcho body =
  -- Some trickery is needed to make sure that if anything inside @body@
  -- references a variable @f@, it points to the same thing in both cases.
  let body' = GroupS
                 [ SetS "f" NoneE
                 , InterpolationS body
                 ]
      resultDirect = runGingerIdentityEither $
                      eval body'
      resultCall = runGingerIdentityEither $ do
                      env <- gets evalEnv
                      setVar "f" $
                        ProcedureV $
                          GingerProcedure env [] $ CallE (VarE "caller") [] []
                      eval $ CallS "f" [] [] body'
      cat = case resultDirect of
              Right {} -> "OK"
              Left err -> unwords . take 1 . words $ show err
  in
    label cat $
    either (const True) (not . getAny . traverseValue (Any . isProcedure)) resultDirect ==>
    resultCall === resultDirect

prop_callMacro :: Statement -> Property
prop_callMacro body =
  -- Some trickery is needed to make sure that if anything inside @body@
  -- references a variable @f@, it points to the same thing in both cases.
  let body' = GroupS
                 [ SetS "f" NoneE
                 , body
                 ]
      resultDirect = runGingerIdentityEither $
                      eval $ GroupS
                        [ ImmediateS $ Encoded "Hello, "
                        , body'
                        ]
      resultCall = runGingerIdentityEither $
                      eval $
                        GroupS
                          [ MacroS "f" [] $ GroupS
                            [ ImmediateS $ Encoded "Hello, "
                            , InterpolationS (CallE (VarE "caller") [] [])
                            ]
                          , CallS "f" [] [] body'
                          ]
      cat = case resultDirect of
              Right {} -> "OK"
              Left err -> unwords . take 1 . words $ show err
  in
    label cat $
    either (const True) (not . getAny . traverseValue (Any . isProcedure)) resultDirect ==>
    resultCall === resultDirect

prop_include :: ArbitraryText -> Statement -> Property
prop_include (ArbitraryText name) body =
  let bodySrc = renderSyntaxText body
      resultDirect = runGingerIdentityEither $
                      eval body
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (IncludeS (StringLitE name) RequireMissing WithContext)
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === resultDirect

prop_includeInto :: ArbitraryText -> Statement -> Statement -> Property
prop_includeInto (ArbitraryText name) body parent =
  let bodySrc = renderSyntaxText body
      resultDirect = runGingerIdentityEither $
                      eval (GroupS [ body, parent ])
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ IncludeS (StringLitE name) RequireMissing WithContext
                            , parent
                            ])
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === resultDirect

prop_includeMacro :: ArbitraryText -> Identifier -> Statement -> Property
prop_includeMacro (ArbitraryText name) macroName body =
  let bodySrc = renderSyntaxText $
                  MacroS macroName [] body
      resultDirect = runGingerIdentityEither $
                      eval body
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ IncludeS (StringLitE name) RequireMissing WithContext
                            , CallS macroName [] [] (InterpolationS NoneE)
                            ]
                        )
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === resultDirect

prop_includeMacroWithoutContext :: ArbitraryText -> Identifier -> Statement -> Property
prop_includeMacroWithoutContext (ArbitraryText name) macroName body =
  let bodySrc = renderSyntaxText $
                  MacroS macroName [] body
      resultDirect = runGingerIdentityEither $
                      eval body
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ IncludeS (StringLitE name) RequireMissing WithoutContext
                            , CallS macroName [] [] (InterpolationS NoneE)
                            ]
                        )
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    name /= identifierName macroName ==>
    resultInclude === resultDirect

prop_includeSet :: ArbitraryText -> Identifier -> ArbitraryText -> Property
prop_includeSet (ArbitraryText name) varName (ArbitraryText varValue) =
  let bodySrc = renderSyntaxText $
                  SetS varName (StringLitE varValue)
      resultDirect = runGingerIdentityEither $
                      eval (StringLitE varValue)
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ IncludeS (StringLitE name) RequireMissing WithContext
                            , InterpolationS (VarE varName)
                            ]
                        )
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === resultDirect

prop_includeWithContext :: ArbitraryText -> Identifier -> ArbitraryText -> Property
prop_includeWithContext (ArbitraryText name) varName (ArbitraryText varValue) =
  let bodySrc = renderSyntaxText $
                  InterpolationS (VarE varName)
      resultDirect = runGingerIdentityEither $
                      eval (StringLitE varValue)
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ SetS varName (StringLitE varValue)
                            , IncludeS (StringLitE name) RequireMissing WithContext
                            ]
                        )
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === resultDirect

prop_includeWithoutContext :: ArbitraryText -> Identifier -> ArbitraryText -> Property
prop_includeWithoutContext (ArbitraryText name) varName (ArbitraryText varValue) =
  let bodySrc = renderSyntaxText $
                  InterpolationS (VarE varName)
      loader = mockLoader [(name, bodySrc)]
      resultInclude = runGingerIdentityEitherWithLoader loader $
                        eval (
                          GroupS
                            [ SetS varName (StringLitE varValue)
                            , IncludeS (StringLitE name) RequireMissing WithoutContext
                            ]
                        )
      expected = Left . PrettyRuntimeError $ NotInScopeError (Just $ identifierName varName)
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultInclude === expected

prop_importValue :: ArbitraryText -> Identifier -> Expr -> Property
prop_importValue (ArbitraryText name) varName valE =
  let bodySrc = renderSyntaxText (SetS varName valE)
      resultDirect = runGingerIdentityEither $ eval valE
      loader = mockLoader [(name, bodySrc)]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $
                        GroupS
                          [ ImportS (StringLitE name) Nothing Nothing RequireMissing WithoutContext
                          , InterpolationS (VarE varName)
                          ]
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultImport === resultDirect

prop_importValueAlias :: ArbitraryText -> Identifier -> Identifier -> Expr -> Property
prop_importValueAlias (ArbitraryText name) alias varName valE =
  let bodySrc = renderSyntaxText (SetS varName valE)
      resultDirect = runGingerIdentityEither $ eval valE
      loader = mockLoader [(name, bodySrc)]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $
                        GroupS
                          [ ImportS (StringLitE name) (Just alias) Nothing RequireMissing WithoutContext
                          , InterpolationS $
                              DotE
                                (VarE alias)
                                varName
                          ]
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultImport === resultDirect

prop_importMacro :: ArbitraryText -> Identifier -> Statement -> Property
prop_importMacro (ArbitraryText name) varName bodyS =
  let bodySrc = renderSyntaxText (MacroS varName [] bodyS)
      resultDirect = runGingerIdentityEither $ eval bodyS
      loader = mockLoader [(name, bodySrc)]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $
                        GroupS
                          [ ImportS (StringLitE name) Nothing Nothing RequireMissing WithoutContext
                          , CallS varName [] [] (InterpolationS NoneE)
                          ]
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    resultImport === resultDirect

prop_importWithoutContext :: ArbitraryText -> Identifier -> Identifier -> Expr -> Property
prop_importWithoutContext (ArbitraryText name) macroName varName bodyE =
  let bodySrc = renderSyntaxText $
                    (MacroS macroName []
                      (InterpolationS
                        (VarE varName)))
      resultDirect = runGingerIdentityEither $ do
                      void $ eval bodyE
                      throwError $ NotInScopeError (Just $ identifierName varName)
      loader = mockLoader [(name, bodySrc)]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $
                        GroupS
                          [ SetS varName bodyE
                          , ImportS (StringLitE name) Nothing Nothing RequireMissing WithoutContext
                          , CallS macroName [] [] (InterpolationS NoneE)
                          ]
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    macroName /= varName ==>
    resultImport === resultDirect

prop_importWithContext :: ArbitraryText -> Identifier -> Identifier -> Expr -> Property
prop_importWithContext (ArbitraryText name) macroName varName bodyE =
  let bodySrc = renderSyntaxText $
                    (MacroS macroName []
                      (InterpolationS
                        (VarE varName)))
      resultDirect = runGingerIdentityEither $ eval bodyE
      loader = mockLoader [(name, bodySrc)]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $
                        GroupS
                          [ SetS varName bodyE
                          , ImportS (StringLitE name) Nothing Nothing RequireMissing WithContext
                          , CallS macroName [] [] (InterpolationS NoneE)
                          ]
  in
    counterexample ("SOURCE:\n" ++ Text.unpack bodySrc) $
    macroName /= varName ==>
    resultImport === resultDirect

prop_importExplicit :: NonEmptyText
                    -> Identifier -> Expr
                    -> Identifier -> Expr
                    -> Expr
                    -> Property
prop_importExplicit (NonEmptyText name)
                    varName1 body1E
                    varName2 body2E
                    body3E =
  let bodySrc = renderSyntaxText $
                    GroupS
                      [ SetS varName1 body1E
                      , SetS varName2 body2E
                      ]
      directS = GroupS
                  [ InterpolationS body2E
                  , InterpolationS body3E
                  ]
      resultDirect = runGingerIdentityEither $ do
                        void $ eval $ InterpolationS body3E
                        void $ eval $ InterpolationS body1E
                        eval $ directS
      directSrc = renderSyntaxText $ directS

      loader = mockLoader [(name, bodySrc)]
      mainS = GroupS
                [ SetS varName1 body3E
                , ImportS (StringLitE name) Nothing (Just [(varName2, Nothing)]) RequireMissing WithoutContext
                , InterpolationS (VarE varName2)
                , InterpolationS (VarE varName1)
                ]
      resultImport = runGingerIdentityEitherWithLoader loader . eval $ mainS
      importerSrc = renderSyntaxText $ mainS
  in
    counterexample ("DIRECT SOURCE:\n" ++ Text.unpack directSrc) $
    counterexample ("IMPORTED SOURCE:\n" ++ Text.unpack bodySrc) $
    counterexample ("MAIN SOURCE:\n" ++ Text.unpack importerSrc) $
    varName1 /= varName2 ==>
    resultImport === resultDirect

prop_extendSimple :: NonEmptyText
                  -> Identifier
                  -> Statement
                  -> Statement
                  -> Property
prop_extendSimple (NonEmptyText parentName) blockName body body' =
  let parentSrc = renderSyntaxText $
                    GroupS
                      [ ImmediateS (Encoded "foo\n")
                      , BlockS blockName (Block body NotScoped Optional)
                      , ImmediateS (Encoded "bar\n")
                      ]
      directS = GroupS
                  [ ImmediateS (Encoded "foo\n")
                  , body'
                  , ImmediateS (Encoded "bar\n")
                  ]
      directSrc = renderSyntaxText $
                    directS
      mainT = Template
                (Just parentName)
                (BlockS blockName $ Block body' NotScoped Optional)
      mainSrc = renderSyntaxText $
                  templateBody mainT
      loader = mockLoader [(parentName, parentSrc)]
      resultDirect = runGingerIdentityEither $ do
        evalS directS
      resultExtends = runGingerIdentityEitherWithLoader loader $ do
        evalT mainT
      cat = case resultDirect of
              Left err -> unwords . take 1 . words $ show err
              Right _ -> "OK"
  in
    label cat $
    counterexample ("PARENT SOURCE:\n" ++ Text.unpack parentSrc) $
    counterexample ("CHILD SOURCE:\n" ++ Text.unpack mainSrc) $
    counterexample ("DIRECT SOURCE:\n" ++ Text.unpack directSrc) $
    resultExtends === resultDirect

prop_extendSuper :: NonEmptyText
                  -> Identifier
                  -> Statement
                  -> Statement
                  -> Property
prop_extendSuper (NonEmptyText parentName) blockName body body' =
  let parentSrc = renderSyntaxText $
                    GroupS
                      [ ImmediateS (Encoded "foo\n")
                      , BlockS blockName (Block body NotScoped Optional)
                      , ImmediateS (Encoded "bar\n")
                      ]
      directS = GroupS
                  [ ImmediateS (Encoded "foo\n")
                  , body'
                  , body
                  , ImmediateS (Encoded "bar\n")
                  ]
      directSrc = renderSyntaxText $
                    directS
      mainT = Template
                (Just parentName)
                (BlockS blockName $
                    Block
                      (GroupS
                        [ body'
                        , CallS "super" [] [] (GroupS [])
                        ]
                      )
                      NotScoped
                      Optional)
      mainSrc = renderSyntaxText $
                  templateBody mainT
      loader = mockLoader [(parentName, parentSrc)]
      resultDirect = runGingerIdentityEither $ do
        evalS directS
      resultExtends = runGingerIdentityEitherWithLoader loader $ do
        evalT mainT
      cat = case resultDirect of
              Left err -> unwords . take 1 . words $ show err
              Right _ -> "OK"
  in
    label cat $
    counterexample ("PARENT SOURCE:\n" ++ Text.unpack parentSrc) $
    counterexample ("CHILD SOURCE:\n" ++ Text.unpack mainSrc) $
    counterexample ("DIRECT SOURCE:\n" ++ Text.unpack directSrc) $
    resultExtends === resultDirect

prop_extendWithContext :: NonEmptyText
                       -> Identifier
                       -> Identifier
                       -> Expr
                       -> Property
prop_extendWithContext (NonEmptyText parentName) blockName varName varExpr =
  let
      parentS = GroupS
        [ SetS varName varExpr
        , BlockS blockName (Block (GroupS []) Scoped Optional)
        ]
      parentSrc = renderSyntaxText $ parentS

      childT = Template
                  (Just parentName)
                  (BlockS blockName
                    (Block (InterpolationS (VarE varName)) Scoped Optional)
                  )
      childSrc = renderSyntaxText $ childT

      directS = GroupS
                  [ SetS varName varExpr
                  , BlockS blockName
                      (Block (InterpolationS (VarE varName)) Scoped Optional)
                  ]
      directSrc = renderSyntaxText $ directS

      loader = mockLoader [(parentName, parentSrc)]

      resultDirect = runGingerIdentityEither $ do
        evalS directS
      resultExtends = runGingerIdentityEitherWithLoader loader $ do
        evalT childT
      cat = case resultDirect of
              Left err -> unwords . take 1 . words $ show err
              Right _ -> "OK"
  in
    label cat $
    counterexample ("PARENT SOURCE:\n" ++ Text.unpack parentSrc) $
    counterexample ("CHILD SOURCE:\n" ++ Text.unpack childSrc) $
    counterexample ("DIRECT SOURCE:\n" ++ Text.unpack directSrc) $
    resultExtends === resultDirect

prop_extendWithoutContext :: NonEmptyText
                          -> Identifier
                          -> Identifier
                          -> Expr
                          -> Identifier
                          -> Property
prop_extendWithoutContext (NonEmptyText parentName) blockName varName varExpr dummyVarName =
  let
      parentS = GroupS
        [ SetS varName varExpr
        , BlockS blockName (Block (GroupS []) NotScoped Optional)
        ]
      parentSrc = renderSyntaxText $ parentS

      childT = Template
                  (Just parentName)
                  (BlockS blockName
                    (Block (InterpolationS (VarE varName)) NotScoped Optional)
                  )
      childSrc = renderSyntaxText $ childT

      directS = GroupS
                  [ SetS dummyVarName varExpr
                  , BlockS blockName
                      (Block (InterpolationS (VarE varName)) NotScoped Optional)
                  ]
      directSrc = renderSyntaxText $ directS

      loader = mockLoader [(parentName, parentSrc)]

      resultDirect = runGingerIdentityEither $ do
        evalS directS
      resultExtends = runGingerIdentityEitherWithLoader loader $ do
        evalT childT
      cat = case resultDirect of
              Left (PrettyRuntimeError (NotInScopeError (Just n))) | Identifier n == varName -> "OK (NotInScope, expected)"
              Left err -> unwords . take 1 . words $ show err
              Right _ -> "Unexpected success"
  in
    label cat $
    counterexample ("PARENT SOURCE:\n" ++ Text.unpack parentSrc) $
    counterexample ("CHILD SOURCE:\n" ++ Text.unpack childSrc) $
    counterexample ("DIRECT SOURCE:\n" ++ Text.unpack directSrc) $
    varName /= dummyVarName ==>
    resultExtends === resultDirect
