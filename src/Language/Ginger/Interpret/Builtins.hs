{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Language.Ginger.Interpret.Builtins
where

import Language.Ginger.AST
import Language.Ginger.Interpret.Type
import Language.Ginger.Render (renderSyntaxText)
import Language.Ginger.RuntimeError
import Language.Ginger.Value
import Language.Ginger.StringFormatting (FormatArg (..))

import Control.Applicative ( (<|>) )
import Control.Monad.Except
import Control.Monad.Trans (lift, MonadTrans (..))
import qualified Data.Aeson as JSON
import qualified Data.Array as Array
import Data.Bits (popCount, shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isUpper, isLower, isAlphaNum, isPrint, isSpace, isAlpha, isDigit, ord)
import Data.Foldable (asum)
#if MIN_VERSION_base(4,20,0)
import Data.List (sortBy, minimumBy, maximumBy)
#else
import Data.List (sortBy, minimumBy, maximumBy, foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
        ( TimeZone (..)
        , ZonedTime (..)
        , TimeOfDay (..)
        , LocalTime (..)
        , parseTimeM
        , defaultTimeLocale
        , utc
        , fromGregorian
        , utcToZonedTime
        , zonedTimeToUTC
        , formatTime
        )
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random (uniformR)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as RE

--------------------------------------------------------------------------------
-- Builtins
--------------------------------------------------------------------------------

type BuiltinAttribs a m = Map Identifier (a -> m (Either RuntimeError (Value m)))

builtinGlobals :: forall m. Monad m
                 => (Expr -> GingerT m (Value m))
                 -> Map Identifier (Value m)
builtinGlobals evalE = Map.fromList $
  [ ("abs", numericBuiltin
               "builtin:abs"
               (Just ProcedureDoc
                  { procedureDocName = "abs"
                  , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "number")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "number")
                  , procedureDocDescription = "Absolute of a number."
                  }
                )
               abs abs)
  , ("attr", fnToValue "builtin:attr"
               (Just ProcedureDoc
                { procedureDocName = "attr"
                , procedureDocArgs =
                  [ ArgumentDoc
                      "value"
                      (Just $ TypeDocSingle "dict")
                      Nothing
                      ""
                  , ArgumentDoc
                      "attrName"
                      (Just $ TypeDocSingle "string")
                      Nothing
                      ""
                  ]
                , procedureDocReturnType = (Just $ TypeDocAny)
                , procedureDocDescription = Text.unlines
                    [ "Get a named attribute from a `dict` or dict-like object."
                    , "Unlike `[]` or dot member access, this will only look " <>
                      "at attributes, not items."
                    ]
                }
              )
               $ \x y -> case y :: Value m of
                    StringV yStr ->
                      fmap (fromMaybe NoneV) <$> getAttrRaw @m x (Identifier yStr)
                    _ ->
                      pure . Right $ NoneV)
  , ("batch", ProcedureV fnBatch)
  , ("capitalize", textBuiltin
                      "builtin:capitalize"
                      (Just ProcedureDoc
                        { procedureDocName = "capitalize"
                        , procedureDocArgs =
                          [ ArgumentDoc
                              "value"
                              (Just $ TypeDocSingle "string")
                              Nothing
                              ""
                          ]
                        , procedureDocReturnType = (Just $ TypeDocSingle "string")
                        , procedureDocDescription = "Convert `value` to title case."
                        }
                      )
                      Text.toTitle)
  , ("center", ProcedureV fnCenter)
  , ("count", ProcedureV fnLength)
  , ("dictsort", ProcedureV fnDictsort)
  , ("e", ProcedureV fnEscape)
  , ("escape", ProcedureV fnEscape)
  , ("even", intBuiltin
                "builtin:even"
                (Just ProcedureDoc
                  { procedureDocName = "even"
                  , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "int")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                  , procedureDocDescription = "Check if `value` is an even number"
                  }
                )
                even)
  , ("filesizeformat", ProcedureV fnFilesizeFormat)
  , ("first", ProcedureV fnFirst)
  , ("float", ProcedureV fnToFloat)
  -- , ("forceescape", undefined)
  , ("format", ProcedureV fnFormat)
  , ("groupby", ProcedureV fnGroupBy)
  -- , ("indent", undefined)
  , ("int", ProcedureV fnToInt)
  , ("items", ProcedureV fnItems)
  , ("join", ProcedureV fnJoin)
  , ("json", ProcedureV fnToJSON)
  , ("last", ProcedureV fnLast)
  , ("length", ProcedureV fnLength)
  , ("list", ProcedureV fnToList)
  , ("lower", textBuiltin
                "builtin:lower"
                (Just ProcedureDoc
                  { procedureDocName = "lower"
                  , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "string")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "string")
                  , procedureDocDescription = "Convert `value` to lowercase."
                  }
                )
                Text.toLower)
  , ("map", FilterV $ fnMap evalE)
  , ("max", ProcedureV fnMax)
  , ("min", ProcedureV fnMin)
  , ("namespace", ProcedureV NamespaceProcedure)
  , ("odd", intBuiltin
              "builtin:odd"
              (Just ProcedureDoc
                { procedureDocName = "odd"
                , procedureDocArgs =
                  [ ArgumentDoc
                      "value"
                      (Just $ TypeDocSingle "int")
                      Nothing
                      ""
                  ]
                , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                , procedureDocDescription = "Checks if `value` is an odd number."
                }
              )
              odd)
  -- , ("pprint", undefined)
  , ("random", ProcedureV fnRandom)
  -- , ("rejectattr", undefined)
  , ("reject", FilterV $ fnReject evalE)
  , ("replace", ProcedureV fnStrReplace)
  , ("reverse", ProcedureV fnReverse)
  , ("round", ProcedureV fnRound)
  , ("safe", textBuiltin
              "builtin:safe"
              (Just ProcedureDoc
                { procedureDocName = "safe"
                , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "string")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "encoded")
                  , procedureDocDescription = "Mark `value` as pre-encoded HTML."
                  }
                )
              (EncodedV @m . Encoded)

    )
  -- , ("selectattr", undefined)
  , ("select", FilterV $ fnSelect evalE)
  -- , ("slice", undefined)
  , ("sort", ProcedureV fnSort)
  , ("split", ProcedureV fnStrSplit)
  , ("string", ProcedureV fnToString)
  -- , ("striptags", undefined)
  , ("sum", ProcedureV fnSum)
  , ("title", textBuiltin
                "builtin:title"
                (Just ProcedureDoc
                  { procedureDocName = "title"
                  , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "string")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "string")
                  , procedureDocDescription = "Convert `value` to title case."
                  }
                )
                Text.toTitle)
  , ("tojson", ProcedureV fnToJSON)
  -- , ("trim", undefined)
  -- , ("truncate", undefined)
  -- , ("unique", undefined)
  , ("upper", textBuiltin
                "builtin:upper"
                (Just ProcedureDoc
                  { procedureDocName = "upper"
                  , procedureDocArgs =
                    [ ArgumentDoc
                        "value"
                        (Just $ TypeDocSingle "string")
                        Nothing
                        ""
                    ]
                  , procedureDocReturnType = (Just $ TypeDocSingle "string")
                  , procedureDocDescription = "Convert `value` to uppercase."
                  }
                )
                Text.toUpper)
  -- , ("urlencode", undefined)
  -- , ("urlize", undefined)
  , ("wordcount", textBuiltin
                    "builtin:wordcount"
                    (Just ProcedureDoc
                      { procedureDocName = "wordcount"
                      , procedureDocArgs =
                        [ ArgumentDoc
                            "value"
                            (Just $ TypeDocSingle "string")
                            Nothing
                            ""
                        ]
                      , procedureDocReturnType = (Just $ TypeDocSingle "int")
                      , procedureDocDescription = "Counts words in value."
                      }
                    )
                    (length . Text.words))
  -- , ("wordwrap", undefined)
  -- , ("xmlattr", undefined)
  ]

builtinGlobalsNonJinja :: forall m. Monad m
                       => (Expr -> GingerT m (Value m))
                       -> Map Identifier (Value m)
builtinGlobalsNonJinja _evalE = Map.fromList $
  [ ("strip", ProcedureV fnStrStrip)
  , ("regex", regexModule)
  , ("date", ProcedureV fnDateFormat)
  , ("dateformat", ProcedureV fnDateFormat)
  , ("help", ProcedureV fnHelp)
  ]

builtinIntAttribs :: forall m. Monad m => BuiltinAttribs Integer m
builtinIntAttribs = Map.fromList
  [ ("denominator", intProp (const (1 :: Integer)))
  , ("bit_count", intAttrib
                    "builtin:int:bit_count"
                    (Just ProcedureDoc
                      { procedureDocName = "int.bit_count"
                      , procedureDocArgs = mempty
                      , procedureDocReturnType = (Just $ TypeDocSingle "int")
                      , procedureDocDescription =
                          Text.unlines
                            [ "Bit count (popcount)."
                            , "Counts the number of set bits in an integer."
                            ]
                      }
                    )
                    popCount)
  -- , ("bit_length", ?)
  -- , ("conjugate", ?)
  -- , ("from_bytes", ?)
  -- , ("to_bytes", ?)
  , ("imag", intProp (const (0 :: Integer)))
  , ("numerator", intProp id)
  , ("real", intProp id)
  ]

builtinFloatAttribs :: Monad m => BuiltinAttribs Double m
builtinFloatAttribs = Map.fromList
  [ ("imag", floatProp (const (0 :: Double)))
  , ("real", floatProp id)
  -- , ("is_integer", ?)
  -- , ("hex", ?)
  -- , ("as_integer_ratio", ?)
  -- , ("conjugate", ?)
  ]

builtinBoolAttribs :: Monad m => BuiltinAttribs Bool m
builtinBoolAttribs = Map.fromList
  [ ("denominator", boolProp (const (1 :: Integer)))
  , ("bit_count", boolAttrib
                    "builtin:bool:bit_count"
                    (Just ProcedureDoc
                      { procedureDocName = "bool.bit_count"
                      , procedureDocReturnType = (Just $ TypeDocSingle "int")
                      , procedureDocArgs = mempty
                      , procedureDocDescription =
                          Text.unlines
                            [ "Bit count (popcount)."
                            , "Counts the number of set bits."
                            , "Since a boolean only has one bit, this will " <>
                              "always be either 0 or 1."
                            ]
                      }
                    )
                    popCount)
  -- , ("bit_length", ?)
  -- , ("conjugate", ?)
  -- , ("from_bytes", ?)
  , ("to_bytes", boolProp (BS.singleton . fromIntegral . fromEnum))
  , ("imag", boolProp (const (0 :: Integer)))
  , ("numerator", boolProp fromEnum)
  , ("real", boolProp fromEnum)
  ]

builtinStringAttribs :: forall m. Monad m => BuiltinAttribs Text m
builtinStringAttribs = Map.fromList
  [ ("length", textProp Text.length)
  , ("capitalize", textAttrib
                      "builtin:string:capitalize"
                      (Just ProcedureDoc
                        { procedureDocName = "string.capitalize"
                        , procedureDocArgs = mempty
                        , procedureDocReturnType = (Just $ TypeDocSingle "string")
                        , procedureDocDescription = "Convert `value` to title case."
                        }
                      )
                      Text.toTitle)
  , ("casefold", textAttrib
                      "builtin:string:casefold"
                      (Just ProcedureDoc
                        { procedureDocName = "string.casefold"
                        , procedureDocArgs = mempty
                        , procedureDocReturnType = (Just $ TypeDocSingle "string")
                        , procedureDocDescription =
                            "Convert `value` to canonical case for " <>
                            "case-insensitive comparison"
                        }
                      )
                      Text.toCaseFold)
  , ("center", textProcAttrib fnCenter)
  , ("count", textProcAttrib fnStrCount)
  , ("encode", textProcAttrib fnStrEncode)
  , ("endswith", textProcAttrib fnStrEndswith)
  -- , ("expandtabs", ?)
  -- , ("find", ?)
  , ("format", textProcAttrib fnStrFormat)
  -- , ("format_map", ?)
  -- , ("index", ?)
  , ("isalnum", textAttrib
                  "builtin:string:isalnum"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isalnum"
                    , procedureDocArgs = mempty
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocDescription = "Check whether a string is alpha-numeric (a letter or a digit)."
                    }
                  )
                  (Text.all isAlphaNum)
                )
  , ("isalpha", textAttrib
                  "builtin:string:isalpha"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isalpha"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string is alphabetic (consists solely of letters)."
                    }
                  )
                  (Text.all isAlpha))
  , ("isascii", textAttrib
                  "builtin:string:isascii"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isascii"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string consists solely of 7-bit ASCII characters."
                    }
                  )
                  (Text.all ((< 128) . ord)))
  , ("isdecimal", textAttrib
                  "builtin:string:isdecimal"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isdecimal"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string is a decimal number"
                    }
                  )
                  isDecimal)
  , ("isdigit", textAttrib
                  "builtin:string:isdigit"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isdigit"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string consists solely of digits."
                    }
                  )
                  (Text.all isDigit))
  -- , ("isidentifier", ?)
  , ("islower", textNProcAttrib
                  "builtin:string:islower"
                  (Just ProcedureDoc
                    { procedureDocName = "string.islower"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string is all-lowercase"
                    }
                  )
                  isLowerVal)
  -- , ("isnumeric", ?)
  , ("isprintable", textAttrib
                  "builtin:string:isprintable"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isprintable"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string contains only printable characters."
                    }
                  )
                  (Text.all isPrint))
  , ("isspace", textAttrib
                  "builtin:string:isspace"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isspace"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string contains only whitespace."
                    }
                  )
                  (Text.all isSpace))
  , ("isupper", textNProcAttrib
                  "builtin:string:isupper"
                  (Just ProcedureDoc
                    { procedureDocName = "string.isupper"
                    , procedureDocReturnType = (Just $ TypeDocSingle "bool")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Check whether a string is all-uppercase."
                    }
                  )
                  isUpperVal)
  , ("join", textProcAttrib fnStrJoin)
  -- , ("ljust", ?)
  , ("lower", textAttrib
                  "builtin:lower"
                  (Just ProcedureDoc
                    { procedureDocName = "string.lower"
                    , procedureDocArgs = mempty
                    , procedureDocReturnType = (Just $ TypeDocSingle "string")
                    , procedureDocDescription = "Convert `value` to lowercase."
                    }
                  )
                  Text.toLower)
  , ("lstrip", textProcAttrib fnStrLStrip)
  -- , ("maketrans", ?)
  -- , ("partition", ?)
  -- , ("removeprefix", ?)
  -- , ("removesuffix", ?)
  , ("replace", textProcAttrib fnStrReplace)
  -- , ("rfind", ?)
  -- , ("rindex", ?)
  -- , ("rjust", ?)
  -- , ("rpartition", ?)
  , ("rstrip", textProcAttrib fnStrRStrip)
  , ("split", textProcAttrib fnStrSplit)
  , ("splitlines", textAttrib
                  "builtin:string:splitlines()"
                  (Just ProcedureDoc
                    { procedureDocName = "string.splitlines"
                    , procedureDocReturnType = (Just $ TypeDocSingle "string")
                    , procedureDocArgs = mempty
                    , procedureDocDescription = "Split a string into lines."
                    }
                  )
                  Text.lines)
  , ("startswith", textProcAttrib fnStrStartswith)
  , ("strip", textProcAttrib fnStrStrip)
  -- , ("swapcase", ?)
  , ("title", textAttrib
                  "builtin:title"
                  (Just ProcedureDoc
                    { procedureDocName = "string.title"
                    , procedureDocArgs = mempty
                    , procedureDocReturnType = (Just $ TypeDocSingle "string")
                    , procedureDocDescription = "Convert `value` to title case."
                    }
                  )
                  Text.toTitle)
  -- , ("translate", ?)
  , ("upper", textAttrib
                  "builtin:upper"
                  (Just ProcedureDoc
                    { procedureDocName = "string.upper"
                    , procedureDocArgs = mempty
                    , procedureDocReturnType = (Just $ TypeDocSingle "string")
                    , procedureDocDescription = "Convert `value` to uppercase."
                    }
                  )
                  Text.toUpper)
  -- , ("zfill", ?)
  ]

builtinListAttribs :: Monad m => BuiltinAttribs (Vector (Value m)) m
builtinListAttribs = Map.fromList
  [
  ]

builtinDictAttribs :: Monad m => BuiltinAttribs (Map Scalar (Value m)) m
builtinDictAttribs = Map.fromList
  [ ("items", dictAttrib
                "builtin:dict:items"
                (Just ProcedureDoc
                  { procedureDocName = "dict.items"
                  , procedureDocArgs = mempty
                  , procedureDocReturnType = (Just $ TypeDocSingle "list")
                  , procedureDocDescription = "Get a list of key/value pairs from dictionary `value` as a list"
                  }
                )
                Map.toList)
  , ("values", dictAttrib
                "builtin:dict:values"
                (Just ProcedureDoc
                  { procedureDocName = "dict.values"
                  , procedureDocArgs = mempty
                  , procedureDocReturnType = (Just $ TypeDocSingle "list")
                  , procedureDocDescription = "Extract the values from dictionary `value` as a list"
                  }
                )
                Map.elems)
  , ("keys", dictAttrib
                "builtin:dict:keys"
                (Just ProcedureDoc
                  { procedureDocName = "dict.keys"
                  , procedureDocArgs = mempty
                  , procedureDocReturnType = (Just $ TypeDocSingle "list")
                  , procedureDocDescription = "Get a list of all keys in dict `value`"
                  }
                )
                Map.keys)
  , ("get", dictProcAttrib
                fnDictGet)
  ]

--------------------------------------------------------------------------------
-- Built-in function implementations
--------------------------------------------------------------------------------

regexModule :: forall m. Monad m => Value m
regexModule = dictV
  [ ("match", ProcedureV fnReMatch)
  , ("matches", ProcedureV fnReMatches)
  , ("test", ProcedureV fnReTest)
  ]

runReWith :: forall a m. Monad m
          => (RE.Regex -> Text -> a)
          -> Text
          -> Text
          -> Text
          -> ExceptT RuntimeError m a
runReWith matchFunc regexText haystack optsText = do
    let opts = parseCompOpts optsText
    let regex = RE.makeRegexOpts opts RE.defaultExecOpt (Text.unpack regexText)
    pure $ matchFunc regex haystack

fnReMatch :: forall m. Monad m => Procedure m
fnReMatch = mkFn3 "regex.match"
              (Text.unlines
                [ "Match a regular expression against a string."
                , "Returns an array where the first element is the entire " <>
                  "match, and subsequent elements are matches on " <>
                  "subexpressions (capture groups)."
                ]
              )
              ( "regex"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "haystack"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "opts"
              , Just ""
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "list")
  $ runReWith (\r h -> convertMatchOnceText $ RE.matchOnceText r h)

fnReMatches :: forall m. Monad m => Procedure m
fnReMatches = mkFn3 "regex.matches"
              (Text.unlines
                  [ "Match a regular expression against a string."
                  , "Returns an array of matches, where each match is an " <>
                    "array where the first element is the entire match, and " <>
                    "subsequent elements are matches on subexpressions " <>
                    "(capture groups)."
                  ]
              )
              ( "regex"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "haystack"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "opts"
              , Just ""
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "list")
  $ runReWith (\r h -> fmap convertMatchText $ RE.matchAllText r h)

fnReTest :: forall m. Monad m => Procedure m
fnReTest = mkFn3 "regex.test"
              (Text.unlines
                  [ "Match a regular expression against a string."
                  , "Returns true if at least one match exists, false otherwise."
                  ]
              )
              ( "regex"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "haystack"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              ( "opts"
              , Just ""
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "bool")
  $ runReWith (\r h -> isJust $ RE.matchOnceText r h)

parseCompOpts :: Text -> RE.CompOption
parseCompOpts = do
  Text.foldl'
    (\x ->
      \case
        'i' -> x { RE.caseSensitive = False }
        'm' -> x { RE.multiline = True }
        _ -> x
    )
    RE.blankCompOpt

convertMatchOnceText :: Maybe (Text, RE.MatchText Text, Text) -> Maybe [Text]
convertMatchOnceText Nothing = Nothing
convertMatchOnceText (Just (_, m, _)) = Just (convertMatchText m)

convertMatchText :: RE.MatchText Text -> [Text]
convertMatchText matches =
  map fst $ Array.elems matches

fnLength :: forall m. Monad m => Procedure m
fnLength = mkFn1 "length"
              (Text.unlines
                  [ "Get the length of a string, list, or dictionary."
                  ]
              )
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAlternatives [ "string", "list", "dict" ]
              , ""
              )
              (Just $ TypeDocSingle "int")
  $ \case
      StringV s -> pure $ Text.length s
      ListV xs -> pure $ length xs
      DictV xs -> pure $ Map.size xs
      x ->
          throwError $
            ArgumentError
              "length"
              "value"
              "iterable"
              (tagNameOf x)

fnEscape :: forall m. Monad m => Procedure m
fnEscape = mkFn1' "escape"
              (Text.unlines
                  [ "Escape the argument."
                  ]
              )
              ( "value"
              , Nothing
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "encoded")
  $ \ctx _ value ->
        (EncodedV @m) <$>
          encodeWith ctx value

fnToList :: forall m. Monad m => Procedure m
fnToList = mkFn1 "list"
              (Text.unlines
                  [ "Convert `value` to a list, if possible"
                  ]
              )
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "list")
  $ \case
    ListV xs ->
      pure xs
    DictV xs ->
      pure (V.fromList $ Map.elems xs)
    StringV txt ->
      pure $ fmap (toValue . Text.singleton) . V.fromList $ Text.unpack txt
    EncodedV (Encoded txt) ->
      pure $ fmap (toValue . Text.singleton) . V.fromList $ Text.unpack txt
    NativeV obj ->
      native (Right <$> nativeObjectAsList obj) >>=
        maybe
          (throwError $
            ArgumentError
              "list"
              "value"
              "iterable"
              "non-iterable native object"
          )
          pure
    BytesV bytes ->
      pure $ fmap toValue . V.fromList $ BS.unpack bytes
    x -> throwError $
            ArgumentError
              "list"
              "value"
              "iterable"
              (tagNameOf x)


fnToFloat :: forall m. Monad m => Procedure m
fnToFloat = mkFn2 "float"
              (Text.unlines
                  [ "Convert `value` to float."
                  , "If `default` is given, values that cannot be converted " <>
                    " to floats will be replaced with this default value."
                  ]
              )
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAny
              , ""
              )
              ( "default"
              , Just 0
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "float")
  $ \value def ->
  case value of
      IntV i -> pure $ fromIntegral i
      BoolV b -> pure $ fromIntegral $ fromEnum b
      FloatV f -> pure f
      NoneV -> pure 0
      StringV s ->
          pure . fromMaybe def $ readMaybe (Text.unpack s)
      _ -> pure def

fnToInt :: forall m. Monad m => Procedure m
fnToInt = mkFn3 "int"
              (Text.unlines
                  [ "Convert `value` to int."
                  , "If `default` is given, values that cannot be converted " <>
                    " to integers will be replaced with this default value."
                  ]
              )
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAny
              , ""
              )
              ( "default"
              , Just 0
              , Just $ TypeDocAny
              , ""
              )
              ( "base"
              , Just 10 :: Maybe Integer
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "int")
  $ \value def _base ->
  case value of
      IntV i -> pure i
      BoolV b -> pure $ fromIntegral $ fromEnum b
      FloatV f -> pure $ round f
      NoneV -> pure 0
      StringV s ->
        pure . fromMaybe def $ readMaybe (Text.unpack s)
      _ -> pure def

fnToString :: forall m. Monad m => Procedure m
fnToString = mkFn1 "string"
              "Convert argument to string"
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "string")
  $ \value ->
    stringify value

fnMin :: forall m. Monad m => Procedure m
fnMin = mkFn3 "min"
              "Get the minimum value from a list"
              ( "value"
              , Nothing
              , Just $ TypeDocSingle "list"
              , ""
              )
              ( "case_sensitive"
              , Just False
              , Just $ TypeDocSingle "bool"
              , "Treat upper and lowercase strings as distinct."
              )
              ( "attr"
              , Just (Nothing :: Maybe Text)
              , Just $ TypeDocSingle "string"
              , "Get the object with the min value of this attribute."
              )
              (Just TypeDocAny)
  $ \xs caseSensitive attrMay -> do
      let caseProjection =
            if caseSensitive then
              id
            else
              caseFoldValue
          attrProjection =
            case attrMay of
              Nothing -> pure
              Just attr -> fmap (fromMaybe NoneV) . eitherExceptM . flip getAttrOrItemRaw (Identifier attr)
      xs' <- mapM (fmap caseProjection . attrProjection) $ V.toList xs
      if null xs' then
        pure NoneV
      else
        pure . snd . minimumBy (\a b -> compare (fst a) (fst b)) $ zip xs' (V.toList xs)

fnMax :: forall m. Monad m => Procedure m
fnMax = mkFn3 "max"
              "Get the maximum value from a list"
              ( "value"
              , Nothing
              , Just $ TypeDocSingle "list"
              , ""
              )
              ( "case_sensitive"
              , Just False
              , Just $ TypeDocSingle "bool"
              , "Treat upper and lowercase strings as distinct."
              )
              ( "attr"
              , Just (Nothing :: Maybe Identifier)
              , Just $ TypeDocSingle "string"
              , "Get the object with the max value of this attribute."
              )
              (Just TypeDocAny)
  $ \xs caseSensitive attrMay -> do
      let caseProjection =
            if caseSensitive then
              id
            else
              caseFoldValue
          attrProjection =
            case attrMay of
              Nothing -> pure
              Just attr -> fmap (fromMaybe NoneV) . eitherExceptM . flip getAttrOrItemRaw attr
      xs' <- mapM (fmap caseProjection . attrProjection) $ V.toList xs
      if null xs' then
        pure NoneV
      else
        pure . snd . maximumBy (\a b -> compare (fst a) (fst b)) $ zip xs' (V.toList xs)

fnSum :: forall m. Monad m => Procedure m
fnSum = mkFn3 "sum"
              "Get the sum of the values in a list"
              ( "value"
              , Nothing
              , Just $ TypeDocSingle "list"
              , ""
              )
              ( "attr"
              , Just (Nothing :: Maybe Identifier)
              , Just $ TypeDocSingle "string"
              , "Use this attribute from each object in the list"
              )
              ( "start"
              , Just (Nothing :: Maybe Int)
              , Just $ TypeDocSingle "int"
              , "Start at this offset into the list"
              )
              (Just TypeDocAny)
  $ \xs attrMay startMay -> do
      let startTransform =
            case startMay of
              Nothing -> id
              Just start -> V.drop start
          attrProjection =
            case attrMay of
              Nothing -> pure
              Just attr -> fmap (fromMaybe NoneV) . eitherExceptM . flip getAttrOrItemRaw attr
      xs' <- mapM attrProjection . V.toList . startTransform $ xs
      pure . valueSum $ xs'

valueSum :: [Value m] -> Value m
valueSum = foldl' valueAdd NoneV

valueAdd :: Value m -> Value m -> Value m
valueAdd (IntV a) (IntV b) = IntV (a + b)
valueAdd NoneV x = x
valueAdd x NoneV = x
valueAdd (FloatV x) (FloatV y) = FloatV (x + y)
valueAdd x y = FloatV (asFloatValLenient 0 x + asFloatValLenient 0 y)

caseFoldValue :: Value m -> Value m
caseFoldValue (StringV t) = StringV (Text.toCaseFold t)
caseFoldValue (EncodedV (Encoded t)) = EncodedV (Encoded (Text.toCaseFold t))
caseFoldValue x = x

fnRandom :: forall m. Monad m => Procedure m
fnRandom = mkFn1' "random"
              "Pick a random element from a list"
              ( "value"
              , Nothing :: Maybe (Vector (Value m))
              , Just $ TypeDocSingle "list"
              , ""
              )
              (Just $ TypeDocAny)
  $ \_ctx rng xs -> do
    if V.null xs then
      pure NoneV
    else do
      let (i, _) = uniformR (0, V.length xs - 1) rng
      pure $ xs V.! i

fnReverse :: forall m. Monad m => Procedure m
fnReverse = mkFn1 "reverse"
              "Reverse a list or string"
              ( "value"
              , Nothing
              , Just $ TypeDocAlternatives [ "list", "string" ]
              , ""
              )
              (Just $ TypeDocAlternatives [ "list", "string" ])
  $ \case
      Left t -> pure $ StringV (Text.reverse t)
      Right xs -> pure $ ListV (V.reverse (xs :: Vector (Value m)))

fnItems :: forall m. Monad m => Procedure m
fnItems = mkFn1 "items"
            "Convert a dict to a list of its elements without the keys."
            ( "value"
            , Nothing
            , Just $ TypeDocSingle "dict"
            , ""
            )
            (Just $ TypeDocSingle "list")
  $ \value ->
      pure (Map.toAscList value :: [(Scalar, Value m)])

data DictSortBy
  = ByKey
  | ByValue
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToValue DictSortBy m where
  toValue ByKey = StringV "key"
  toValue ByValue = StringV "value"

instance (Monad m) => FromValue DictSortBy m where
  fromValue (StringV "key") = pure . Right $ ByKey
  fromValue (StringV "value") = pure . Right $ ByValue
  fromValue (StringV x) = pure . Left $ TagError "conversion to dictsort target" "'key' or 'value'" ("string " <> Text.show x)
  fromValue x = pure . Left $ TagError "conversion to dictsort target" "string" (tagNameOf x)

fnSort :: forall m. Monad m => Procedure m
fnSort = mkFn4 "sort"
              ""
              ( "value"
              , Nothing :: Maybe [Value m]
              , Just $ TypeDocSingle "list"
              , ""
              )
              ( "reverse"
              , Just False
              , Just $ TypeDocSingle "bool"
              , ""
              )
              ( "case_sensitive"
              , Just False
              , Just $ TypeDocSingle "bool"
              , ""
              )
              ( "attribute"
              , Just Nothing :: Maybe (Maybe (Value m))
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "list")
  $ \value reverseSort caseSensitive attributeMay -> do
    let cmp a b = if caseSensitive then
                    compare (fst a) (fst b)
                  else
                    compare (Text.toCaseFold (fst a)) (Text.toCaseFold (fst b))
        cmp' = if reverseSort then flip cmp else cmp
        proj x = do
          sk <- case attributeMay of
                  Nothing ->
                    stringify x
                  Just a -> do
                    v <- fmap (fromMaybe NoneV) $ eitherExceptM $ getItemOrAttrRaw x a
                    stringify v
          pure (sk, x)
    (items' :: [(Text, Value m)]) <- mapM proj value
    pure $ map snd $ sortBy cmp' items'



fnDictsort :: forall m. Monad m => Procedure m
fnDictsort = mkFn4 "dictsort"
              "Sort a dict, returning a list of key-value pairs."
              ( "value"
              , Nothing :: Maybe (Map Scalar (Value m))
              , Just $ TypeDocSingle "dict"
              , ""
              )
              ( "case_sensitive"
              , Just False
              , Just $ TypeDocSingle "bool"
              , ""
              )
              ( "by"
              , Just ByKey
              , Just $ TypeDocSingle "string"
              , "One of 'key', 'value'"
              )
              ( "reverse"
              , Just False
              , Just $ TypeDocSingle "bool"
              , ""
              )
              (Just $ TypeDocSingle "list")
  $ \value caseSensitive by reverseSort -> do
    let cmp a b = if caseSensitive then
                    compare (fst a) (fst b)
                  else
                    compare (Text.toCaseFold (fst a)) (Text.toCaseFold (fst b))
        cmp' = if reverseSort then flip cmp else cmp
        proj (k, v) = do
            sk <- case by of
                        ByKey ->
                          case k of
                            StringScalar s -> pure s
                            IntScalar i -> pure $ Text.show i
                            FloatScalar f -> pure $ Text.show f
                            NoneScalar -> pure ""
                            BoolScalar b -> pure . Text.toLower $ Text.show b
                            EncodedScalar (Encoded e) -> pure e
                            BytesScalar bs -> pure $ Text.decodeUtf8 bs
                        ByValue ->
                          stringify v
            pure (sk, (k, v))
        itemsRaw = Map.toList value
    items' <- mapM proj itemsRaw
    pure $ map snd $ sortBy cmp' items'

fnSelect :: forall m. Monad m
         => (Expr -> GingerT m (Value m))
         -> Filter m
fnSelect = fnSelectReject False "select" "select"

fnReject :: forall m. Monad m
         => (Expr -> GingerT m (Value m))
         -> Filter m
fnReject = fnSelectReject True "reject" "reject"

fnSelectReject :: forall m. Monad m
               => Bool
               -> Text
               -> Text
               -> (Expr -> GingerT m (Value m))
               -> Filter m
fnSelectReject invert procName procDescName evalE =
  NativeFilter
    (Just ProcedureDoc
      { procedureDocName =
          procName
      , procedureDocArgs =
          [ ArgumentDoc
              "value"
              (Just $ TypeDocSingle "list")
              Nothing
              ""
          , ArgumentDoc
              "filter"
              (Just $ TypeDocAlternatives [ "string", "filter", "test", "procedure" ])
              (Just "none")
              ( "A filter or test to apply to each element to determine " <>
                "whether to " <> procDescName <> " it or not."
              )
          , ArgumentDoc
              "attribute"
              (Just $ TypeDocAlternatives [ "string", "none" ])
              (Just "none")
              ( "If specified, the name of an attribute to extract from each " <>
                "element for testing.\n" <>
                "This argument can only be passed by keyword, not positionally."
              )
          ]
      , procedureDocReturnType = (Just $ TypeDocSingle "list")
      , procedureDocDescription = Text.unlines
          [ Text.toTitle procDescName <> " by a test or filter, and/or an attribute."
          ]
      }
    ) $
    \scrutineeE args ctx env rng -> runExceptT $ do
      -- This one is quite a monster, because it accepts arguments in so many
      -- different ways.
      -- Specifically:
      --
      -- @scrutinee|select('foobar', args...)@ - interpret the string
      -- @'foobar'@ as the name of a filter (or procedure), and pass @args...@
      -- on to that filter.
      --
      -- @scrutinee|select(foobar, args...)@ - interpret @foobar@ as a filter
      -- (or a procedure), and pass @args...@ on to that filter.
      --
      -- @scrutinee|select(attribute='foobar', {default=value})@ - interpret
      -- @'foobar' as the name of an attribute in each list element, extract
      -- that list element, use the @default=@ value if the attribute is
      -- absent.
      let funcName = "select"
      argValues <- eitherExcept $
        resolveArgs
          funcName
          []
          args
      varargs <- fnArg funcName "*" argValues
      (kwargs :: Map Scalar (Value m)) <- fnArg funcName "**" argValues
      (scrutinee :: Value m) <- eitherExceptM $
                                  runGingerT (evalE scrutineeE) ctx env rng
      (xs :: Vector (Value m)) <- eitherExceptM $ fromValue scrutinee

      case varargs of
        [] ->
          -- No argument = error.
          throwError $
            ArgumentError
              funcName
              "attribute/callee"
              "attribute=identifier or callable"
              "no argument"
        (test:varargs') -> do
          -- Re-pack the remaining arguments
          let args' = zip (repeat Nothing) varargs' ++
                      Map.toList (Map.mapKeys toIdentifier kwargs)

          -- Determine how to handle each list element.
          let apply' testV x =
                case testV of
                  StringV name -> do
                    -- If it's a string, we interpret it as a filter name, and
                    -- try to look up the corresponding filter in the current
                    -- scope.
                    testV' <- eitherExceptM $
                      runGingerT
                        (withJinjaTests $ evalE (VarE $ Identifier name))
                        ctx env rng
                    apply' testV' x
                  DictV m -> do
                    -- If it's a dict, try to find a @"__call__"@ item.
                    case Map.lookup "__call__" m of
                      Nothing -> throwError $
                                    NonCallableObjectError
                                      (tagNameOf test <> " " <> Text.show testV)
                      Just v -> apply' v x
                  NativeV obj -> do
                    -- If it's a native object, use its @nativeObjectCall@
                    -- method, if available.
                    case nativeObjectCall obj of
                      Nothing -> throwError $
                                    NonCallableObjectError
                                      "non-callable native object"
                      Just f -> eitherExceptM (f obj args') >>=
                                eitherExcept . asTruthVal "native object to bool conversion"
                  TestV f -> do
                    -- If it's a test, we apply it as such, mapping the
                    -- current list element to the variable "@" (which cannot
                    -- be used as a normal identifier, because the syntax
                    -- doesn't allow it). We need to bind it, because
                    -- 'runFilter' takes an unevaluated expression as its
                    -- scrutinee argument, but we have an already-evaluated
                    -- value.
                    let env' = env { envVars = Map.insert "@" x $ envVars env }
                    eitherExceptM $ runTest f (VarE "@") args' ctx env' rng
                  ProcedureV (NativeProcedure _ _ f) -> do
                    -- If it's a native procedure, we can just call it without
                    -- binding anything.
                    eitherExceptM (f ((Nothing, x):args') ctx rng) >>=
                      eitherExcept . asTruthVal "native procedure"
                  ProcedureV (GingerProcedure env' argSpecs body) -> do
                    -- If it's a ginger procedure, we need to prepend the
                    -- current list element to the argument list (so it becomes
                    -- the first positional argument), and then resolve and
                    -- bind all the arguments into the environment where we
                    -- then run the ginger procedure.
                    args'' <- eitherExcept $
                                resolveArgs
                                  "select callback"
                                  argSpecs
                                  ((Nothing, x):args')
                    eitherExceptM (runGingerT (setVars args'' >> evalE body) ctx env' rng) >>=
                        eitherExcept . asTruthVal "ginger procedure"
                  _ ->
                    -- Not something we can call.
                      throwError $
                        NonCallableObjectError
                          (tagNameOf test <> " " <> Text.show testV)

              invertFun = if invert then not else id
              apply = fmap invertFun . apply' test

          ListV <$> V.filterM apply xs
  where
    toIdentifier :: Scalar -> Maybe Identifier
    toIdentifier (StringScalar s) = Just $ Identifier s
    toIdentifier (IntScalar i) = Just $ Identifier (Text.show i)
    toIdentifier (FloatScalar f) = Just $ Identifier (Text.show f) -- dubious
    toIdentifier _ = Nothing

fnMap :: forall m. Monad m
      => (Expr -> GingerT m (Value m))
      -> Filter m
fnMap evalE =
  NativeFilter
    (Just ProcedureDoc
      { procedureDocName = "builtins:map"
      , procedureDocArgs = mempty
      , procedureDocReturnType = Just $ TypeDocSingle "list"
      , procedureDocDescription = Text.unlines
          [ "Map a filter or procedure over a list."
          ]
      }
    ) $
  \scrutineeE args ctx env rng -> runExceptT $ do
    -- This one is quite a monster, because it accepts arguments in so many
    -- different ways.
    -- Specifically:
    --
    -- @scrutinee|map('foobar', args...)@ - interpret the string @'foobar'@ as
    -- the name of a filter (or procedure), and pass @args...@ on to that filter.
    --
    -- @scrutinee|map(foobar, args...)@ - interpret @foobar@ as a filter (or a
    -- procedure), and pass @args...@ on to that filter.
    --
    -- @scrutinee|map(attribute='foobar', {default=value})@ - interpret @'foobar'
    -- as the name of an attribute in each list element, extract that list
    -- element, use the @default=@ value if the attribute is absent.
    let funcName = "map"
    argValues <- eitherExcept $
      resolveArgs
        funcName
        []
        args
    varargs <- fnArg funcName "*" argValues
    (kwargs :: Map Scalar (Value m)) <- fnArg funcName "**" argValues
    (scrutinee :: Value m) <- eitherExceptM $ runGingerT (evalE scrutineeE) ctx env rng
    (xs :: Vector (Value m)) <- eitherExceptM $ fromValue scrutinee

    -- First, let's see if an attribute was specified.
    let attributeMay = Map.lookup "attribute" kwargs
    case attributeMay of
      Just attribute -> do
        -- Attribute was specified, so let's extract that.
        -- We also need to find the default value.
        let defVal = fromMaybe NoneV (Map.lookup "default" kwargs) :: Value m
        ListV <$> V.mapM
          (\x -> do
            attributeIdent <- Identifier <$> eitherExceptM (fromValue attribute)
            fromMaybe defVal <$>
              eitherExceptM
                (getAttrOrItemRaw x attributeIdent)
          )
          xs
      Nothing ->
        -- Attribute was not specified, so we will use the first of the
        -- positional arguments as a mapping function/filter.
        case varargs of
          [] ->
            -- No argument = error.
            throwError $
              ArgumentError
                funcName
                "attribute/callee"
                "attribute=identifier or callable"
                "no argument"
          (callee:varargs') -> do
            -- Re-pack the remaining arguments
            let args' = zip (repeat Nothing) varargs' ++
                        Map.toList (Map.mapKeys toIdentifier kwargs)

            -- Determine how to handle each list element.
            let apply' filterV x =
                  case filterV of
                    StringV name -> do
                      -- If it's a string, we interpret it as a filter name, and
                      -- try to look up the corresponding filter in the current
                      -- scope.
                      filterV' <- eitherExceptM $
                        runGingerT
                          (withJinjaFilters $ evalE (VarE $ Identifier name))
                          ctx env rng
                      apply' filterV' x
                    DictV m -> do
                      -- If it's a dict, try to find a @"__call__"@ item.
                      case Map.lookup "__call__" m of
                        Nothing -> throwError $
                                      NonCallableObjectError
                                        (tagNameOf callee <> " " <> Text.show filterV)
                        Just v -> apply' v x
                    NativeV obj -> do
                      -- If it's a native object, use its @nativeObjectCall@
                      -- method, if available.
                      case nativeObjectCall obj of
                        Nothing -> throwError $
                                      NonCallableObjectError
                                        "non-callable native object"
                        Just f -> eitherExceptM $ f obj args'
                    FilterV f -> do
                      -- If it's a filter, we apply it as such, mapping the
                      -- current list element to the variable "@" (which cannot
                      -- be used as a normal identifier, because the syntax
                      -- doesn't allow it). We need to bind it, because
                      -- 'runFilter' takes an unevaluated expression as its
                      -- scrutinee argument, but we have an already-evaluated
                      -- value.
                      let env' = env { envVars = Map.insert "@" x $ envVars env }
                      eitherExceptM $ runFilter f (VarE "@") args' ctx env' rng
                    ProcedureV (NativeProcedure _ _ f) -> do
                      -- If it's a native procedure, we can just call it without
                      -- binding anything.
                      eitherExceptM $ f ((Nothing, x):args') ctx rng
                    ProcedureV (GingerProcedure env' argSpecs body) -> do
                      -- If it's a ginger procedure, we need to prepend the
                      -- current list element to the argument list (so it becomes
                      -- the first positional argument), and then resolve and
                      -- bind all the arguments into the environment where we
                      -- then run the ginger procedure.
                      args'' <- eitherExcept $
                                  resolveArgs
                                    "map callback"
                                    argSpecs
                                    ((Nothing, x):args')
                      eitherExceptM $
                        runGingerT (setVars args'' >> evalE body) ctx env' rng
                    _ ->
                      -- Not something we can call.
                        throwError $
                          NonCallableObjectError
                            (tagNameOf callee <> " " <> Text.show filterV)

                apply = apply' callee

            ListV <$> mapM apply xs
  where
    toIdentifier :: Scalar -> Maybe Identifier
    toIdentifier (StringScalar s) = Just $ Identifier s
    toIdentifier (IntScalar i) = Just $ Identifier (Text.show i)
    toIdentifier (FloatScalar f) = Just $ Identifier (Text.show f) -- dubious
    toIdentifier _ = Nothing

fnRound :: forall m. Monad m => Procedure m
fnRound = mkFn3 "round"
              "Round a floating-point value."
              ( "value"
              , Nothing :: Maybe Double
              , Just $ TypeDocSingle "float"
              , ""
              )
              ( "precision"
              , Just 0 :: Maybe Integer
              , Just $ TypeDocSingle "int"
              , ""
              )
              ( "method"
              , Just "common"
              , Just $ TypeDocSingle "string"
              , "One of 'common', 'ceil', 'floor'."
              )
              (Just $ TypeDocAlternatives ["int", "float"])
              $ \value precision method -> do
  (r :: Double -> Integer) <- case method of
    "common" -> pure (floor . (+ 0.5))
    "ceil" -> pure ceiling
    "floor" -> pure floor
    x -> throwError $ ArgumentError "round" "method" "one of 'common', 'floor', 'ceil'" x
  if precision == 0 then
    pure $ fromIntegral $ r value
  else do
    let factor :: Double = 10 ** (fromIntegral precision)
    pure $ fromIntegral (r (value * factor) :: Integer) / factor

fnStrReplace :: Monad m => Procedure m
fnStrReplace = mkFn4 "replace"
                "String search-and-replace."
                ( "value"
                , Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "old"
                , Nothing
                , Just $ TypeDocSingle "string"
                , "String to search for"
                )
                ( "new"
                , Nothing
                , Just $ TypeDocSingle "string"
                , "Replacement"
                )
                ( "count"
                , Just (Nothing :: Maybe Int)
                , Just $ TypeDocAny
                , "Maximum number of replacements."
                )
                (Just $ TypeDocSingle "string")
  $ \value old new countMay -> do
    let parts = Text.splitOn old value
    case countMay of
      Nothing -> pure $ Text.intercalate new parts
      Just 0 -> pure value
      Just count | count < length parts ->
        pure $
          Text.intercalate new (take count parts) <>
          new <>
          Text.intercalate old (drop count parts)
      Just _ -> pure $ Text.intercalate new parts


fnStrStrip :: Monad m => Procedure m
fnStrStrip = mkFn2 "strip"
                "Strip whitespace or selected characters from both ends of a string."
                ( "value"
                , Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "chars"
                , Just Nothing
                , Just $ TypeDocSingle "string"
                , "If specified: characters to strip."
                )
                (Just $ TypeDocSingle "string")
  $ \value charsMay -> do
    case charsMay of
      Nothing -> pure $ Text.strip value
      Just charsText -> do
        let chars = Text.unpack charsText
        pure $
          Text.dropWhile (`elem` chars) .
          Text.dropWhileEnd (`elem` chars) $
          value

fnStrLStrip :: Monad m => Procedure m
fnStrLStrip = mkFn2 "lstrip"
                "Strip whitespace or selected characters from the beginning of a string."
                ( "value"
                , Nothing
                , Just $ TypeDocAny
                , ""
                )
                ( "chars"
                , Just Nothing
                , Just $ TypeDocAny
                , "If specified: characters to strip."
                )
                (Just $ TypeDocSingle "string")
  $ \value charsMay -> do
    case charsMay of
      Nothing -> pure $ Text.stripStart value
      Just charsText -> do
        let chars = Text.unpack charsText
        pure $ Text.dropWhile (`elem` chars) value

fnStrRStrip :: Monad m => Procedure m
fnStrRStrip = mkFn2 "rstrip"
                "Strip whitespace or selected characters from the end of a string."
                ( "value"
                , Nothing
                , Just $ TypeDocAny
                , ""
                )
                ( "chars"
                , Just Nothing
                , Just $ TypeDocAny
                , "If specified: characters to strip."
                )
                (Just $ TypeDocSingle "string")
  $ \value charsMay -> do
    case charsMay of
      Nothing -> pure $ Text.stripEnd value
      Just charsText -> do
        let chars = Text.unpack charsText
        pure $ Text.dropWhileEnd (`elem` chars) value

fnToJSON :: forall m. Monad m => Procedure m
fnToJSON = mkFn2 "tojson"
              "Convert `value` to JSON"
              ( "value"
              , Nothing :: Maybe (Value m)
              , Just $ TypeDocAny
              , ""
              )
              ( "indent"
              , Just (Nothing :: Maybe Int)
              , Just $ TypeDocAny
              , ""
              )
              (Just $ TypeDocSingle "string")
  $ \value _indentMay ->
    pure . Text.decodeUtf8 . LBS.toStrict $ JSON.encode value

fnJoin :: forall m. Monad m => Procedure m
fnJoin = mkFn3 "join"
                "Join an iterable into a string"
                ( "iterable"
                , Nothing :: Maybe [Value m]
                , Just $ TypeDocSingle "list"
                , ""
                )
                ( "d"
                , Just "" :: Maybe Text
                , Just $ TypeDocSingle "string"
                , "Default value to use to replace empty elements"
                )
                ( "attr"
                , Just Nothing :: Maybe (Maybe Text)
                , Just $ TypeDocAny
                , "If given, an attribute to pick from each element"
                )
                (Just $ TypeDocSingle "string")
                $ \iterable d attrMay -> do
  iterable' <- case attrMay of
    Nothing -> pure iterable
    Just attr ->
      catMaybes <$>
        mapM
          (\x -> eitherExceptM $ getAttrOrItemRaw x (Identifier attr))
          iterable
  Text.intercalate d <$> mapM (eitherExcept . asTextVal) iterable'

fnStrJoin :: Monad m => Procedure m
fnStrJoin = mkFn2 "join"
                ("`str.join(iterable)` joins `iterable` into a string, using " <>
                 "`str` as a separator."
                )
                ( "value"
                , Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "iterable"
                , Just []
                , Just $ TypeDocSingle "list"
                , ""
                )
                (Just $ TypeDocSingle "string")
                $ \value iterable -> do
  pure $ Text.intercalate value iterable

fnStrSplit :: Monad m => Procedure m
fnStrSplit = mkFn3 "split"
                "Split a string by a separator."
                ( "value"
                , Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "sep"
                , Just Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "maxsplit"
                , Just Nothing
                , Just $ TypeDocSingle "int"
                , "Maximum number of splits. Unlimited if not specified."
                )
                (Just $ TypeDocSingle "list")
                $ \value sepMay maxsplitMay -> do
  items <- case sepMay of
    Nothing -> pure . Text.words . Text.strip $ value
    Just "" -> throwError $
                  ArgumentError
                    "split"
                    "sep"
                    "non-empty string"
                    "empty string"
    Just sep -> pure $ Text.splitOn sep value
  case maxsplitMay of
    Nothing ->
      pure items
    Just maxsplit ->
      pure $ take maxsplit items ++ [Text.unwords $ drop maxsplit items]

fnStrStartswith :: Monad m => Procedure m
fnStrStartswith = mkFn4 "startswith"
                  "Check whether a string starts with a given prefix."
                  ( "value"
                  , Nothing
                  , Just $ TypeDocSingle "string"
                  , ""
                  )
                  ( "prefix"
                  , Nothing
                  , Just $ TypeDocSingle "string"
                  , ""
                  )
                  ( "start"
                  , Just 0
                  , Just $ TypeDocSingle "int"
                  , ""
                  )
                  ( "end"
                  , Just Nothing
                  , Just $ TypeDocSingle "int"
                  , ""
                  )
                  (Just $ TypeDocSingle "bool")
                  $ \value prefix start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ prefix `Text.isPrefixOf` value'

fnStrEndswith :: Monad m => Procedure m
fnStrEndswith = mkFn4 "endswith"
                  "Check whether a string ends with a given suffix."
                  ( "value"
                  , Nothing
                  , Just $ TypeDocSingle "string"
                  , ""
                  )
                  ( "suffix"
                  , Nothing
                  , Just $ TypeDocSingle "string"
                  , ""
                  )
                  ( "start"
                  , Just 0
                  , Just $ TypeDocSingle "int"
                  , ""
                  )
                  ( "end"
                  , Just Nothing
                  , Just $ TypeDocSingle "int"
                  , ""
                  )
                  (Just $ TypeDocSingle "bool")
                  $ \value suffix start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ suffix `Text.isSuffixOf` value'

fnStrEncode :: Monad m => Procedure m
fnStrEncode = mkFn3 "encode"
                "Encode string into the selected encoding."
                ( "value"
                , Nothing
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "encoding"
                , Just "utf-8"
                , Just $ TypeDocSingle "string"
                , "Encoding. One of 'ascii', 'utf8' (default), 'utf16le', 'utf16be', 'utf32le', 'utf32be'"
                )
                ( "errors"
                , Just ("strict" :: Text)
                , Just $ TypeDocAny
                , ""
                )
                (Just $ TypeDocSingle "encoded")
                $ \value encoding _errors -> do
  func <- case Text.filter isAlphaNum . Text.toCaseFold $ encoding of
    "ascii" -> pure encodeASCII
    "utf8" -> pure Text.encodeUtf8
    "utf16le" -> pure Text.encodeUtf16LE
    "utf16be" -> pure Text.encodeUtf16BE
    "utf32le" -> pure Text.encodeUtf32LE
    "utf32be" -> pure Text.encodeUtf32BE
    _ -> throwError $ ArgumentError "encode" "encoding" "valid encoding" encoding
  pure $ func value
  where
    encodeASCII =
      BS.pack .
      map fromIntegral .
      filter (<= 127) .
      map ord .
      Text.unpack

fnStrCount :: Monad m => Procedure m
fnStrCount = mkFn4 "count"
              "Count the number of occurrences of a substring."
              ( "value"
              , Nothing
              , Just $ TypeDocSingle "string"
              , ""
              )
              ( "sub"
              , Nothing
              , Just $ TypeDocSingle "string"
              , "Substring to search for"
              )
              ( "start"
              , Just 0
              , Just $ TypeDocSingle "int"
              , ""
              )
              ( "end"
              , Just Nothing
              , Just $ TypeDocSingle "int"
              , ""
              )
              (Just $ TypeDocSingle "int")
              $ \value sub start endMay -> do
    let value' = case endMay of
          Nothing -> Text.drop start value
          Just end -> Text.drop start . Text.take end $ value
    pure $ Text.count sub value'

fnCenter :: Monad m => Procedure m
fnCenter = mkFn3 "center"
            "Pad string on both sides to center it in the given space"
            ( "value"
            , Nothing
            , Just $ TypeDocSingle "string"
            , ""
            )
            ( "width"
            , Just 80
            , Just $ TypeDocSingle "int"
            , ""
            )
            ( "fillchar"
            , Just " "
            , Just $ TypeDocSingle "string"
            , ""
            )
            (Just $ TypeDocSingle "string")
            $ \value width fillchar' -> do
    let fillchar = Text.take 1 . (<> " ") $ fillchar'
    let paddingTotal = max 0 $ fromInteger width - Text.length value
        paddingLeft = paddingTotal `div` 2
        paddingRight = paddingTotal - paddingLeft
    pure $
      Text.replicate paddingLeft fillchar <>
      value <>
      Text.replicate paddingRight fillchar

newtype FileSize = FileSize Integer

instance ToValue FileSize m where
  toValue (FileSize i) = IntV i

instance Monad m => FromValue FileSize m where
  fromValue (IntV i) =
    pure . Right $ FileSize i
  fromValue (FloatV f) =
    pure . Right $ FileSize (round f)
  fromValue (StringV txt) =
    case readMaybe (Text.unpack txt) of
      Nothing ->
        case readMaybe (Text.unpack txt) of
          Nothing ->
            pure . Left $ ArgumentError "int" "value" "numeric value" (Text.show txt)
          Just (f :: Double) ->
            pure . Right $ FileSize (round f)
      Just i ->
        pure . Right $ FileSize i
  fromValue x =
    pure . Left $ ArgumentError "int" "value" "numeric value" (tagNameOf x)


fnFormat :: Monad m => Procedure m
fnFormat = mkFn2 "format"
            "Apply printf-style formatting"
            ( "value"
            , Nothing
            , Just $ TypeDocSingle "string"
            , "`printf`-style formatting string."
            )
            ( "*args"
            , Nothing :: Maybe (Value m)
            , Just $ TypeDocAny
            , ""
            )
            (Just $ TypeDocSingle "string")
  $ \fmt args -> do
    printfValues fmt args

fnStrFormat :: Monad m => Procedure m
fnStrFormat = mkFn3 "format"
            (Text.unlines
              [ "Apply python-style string formatting."
              , "Formatting string syntax by and large follows " <>
                "[the Python formatstring specification]" <>
                "(https://docs.python.org/3/library/string.html#formatstrings)."
              , "Notable differences:"
              , "* The `!r` modifier does not always produce the same " <>
                "output as the `repr()` function in Python would. " <>
                "However, strings will be quoted, and lists and " <>
                "dictionaries will be rendered in Python-like syntax, not " <>
                "Haskell `show` syntax."
              , "* The `!a` modifier simply uses the same conversion as " <>
                "`!s`, and then deletes all non-ASCII characters."
              , "* Locale-aware number formatting (`n` formatting type) " <>
                "is not supported; `n` is exactly equivalent to `g`."
              , "* Floating-point precisions, if not specified explicitly, " <>
                "may default to different values than they do in Python."
              , "* When formatting floating-point numbers using the `g` " <>
                "(general) format, the cutoff point between fixed-point " <>
                "notation and scientific notation may not be the same as in " <>
                "Python."
              , "* No difference is made between attributes and items."
              ]
            )
            ( "value"
            , Nothing
            , Just $ TypeDocSingle "string"
            , "Python `str.format()`-style formatting string."
            )
            ( "*args"
            , Just [] :: Maybe [Value m]
            , Just $ TypeDocAny
            , ""
            )
            ( "**kwargs"
            , Just mempty :: Maybe (Map Scalar (Value m))
            , Just $ TypeDocAny
            , ""
            )
            (Just $ TypeDocSingle "string")
  $ \fmt args kwargs -> do
    let allArgs = [ (Nothing, v) | v <- args ] ++
                  (Map.toList $ Map.mapKeys (Just . scalarToText) kwargs)
    formatValues valueToFormatArg fmt allArgs

valueToFormatArg :: ( Monad m
                    , MonadTrans t
                    , MonadError RuntimeError (t m)
                    )
               => Value m
               -> t m FormatArg
valueToFormatArg (IntV i) = pure $ IntArg i
valueToFormatArg (FloatV f) = pure $ FloatArg f
valueToFormatArg (BoolV b) = pure $ IntArg (fromIntegral $ fromEnum b)
valueToFormatArg (StringV s) = pure $ StringArg s
valueToFormatArg (EncodedV (Encoded s)) = pure $ StringArg s
valueToFormatArg (BytesV bs) =
  pure $ PolyArg
            (Just $ Text.decodeUtf8 bs)
            (Just $ byteStringToInteger bs)
            Nothing
            (Just . V.fromList . map (IntArg . fromIntegral) $ BS.unpack bs)
            Nothing
valueToFormatArg NoneV =
  pure $ PolyArg
            (Just "")
            (Just 0)
            Nothing
            (Just mempty)
            (Just mempty)
valueToFormatArg (ListV xs) = ListArg <$> mapM valueToFormatArg xs
valueToFormatArg (DictV xs) =
  DictArg <$> valueDictToFormatDict xs
valueToFormatArg (NativeV obj) = do
  listVal <- lift (nativeObjectAsList obj) >>= mapM (V.mapM valueToFormatArg)
  dictVal <- lift (nativeObjectAsDict obj) >>= mapM valueDictToFormatDict
  stringVal <- lift (nativeObjectStringified obj)
  pure $ PolyArg
    (Just stringVal)
    Nothing
    Nothing
    listVal
    dictVal
valueToFormatArg v = throwError . GenericError $
   "Cannot convert " <> tagNameOf v <> " to formatting argument"

valueDictToFormatDict :: ( Monad m
                         , MonadTrans t
                         , MonadError RuntimeError (t m)
                         )
                      => Map Scalar (Value m)
                      -> t m (Map Text FormatArg)
valueDictToFormatDict xs =
  Map.fromList <$>
    mapM (\(k, v) -> (,) <$> pure (scalarToText k) <*> valueToFormatArg v) (Map.toList xs)

-- | Interpret bytestring as big-endian integer number
byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger = go 0 . BS.unpack
  where
    go n [] = n
    go n (x:xs) = go ((n `shiftL` 8) + fromIntegral x) xs

fnFilesizeFormat :: Monad m => Procedure m
fnFilesizeFormat = mkFn2 "filesizeformat"
                    "Format `value` as a human-readable file size."
                    ( "value"
                    , Nothing
                    , Just $ TypeDocSingle "number"
                    , ""
                    )
                    ( "binary"
                    , Just False
                    , Just $ TypeDocSingle "bool"
                    , "If set, use binary units (kiB, MiB, ...) instead of decimal (kB, MB, ...)"
                    )
                    (Just $ TypeDocSingle "string")
  $ \(FileSize value) binary -> do
      let (multiplier, units) =
            if binary then
              (1024, ["ki", "Mi", "Gi", "Ti", "Pi"])
            else
              (1000, ["k", "M", "G", "T", "P"])
      let str = go multiplier units value
      pure $ str <> "B"
  where
    go :: Integer -> [Text] -> Integer -> Text
    go _ [] value
      = Text.show value
    go multiplier units value | value < 0
      = "-" <> go multiplier units (abs value)
    go multiplier (unit:units) value
      | value < multiplier
      = Text.show value
      | value < multiplier * multiplier || null units
      = Text.pack $
          printf "%i.%01i%s"
            (value `div` multiplier)
            ((value * 10 `div` multiplier) `mod` 10)
            unit
      | otherwise
      = go multiplier units (value `div` multiplier)

fnGroupBy :: forall m. Monad m => Procedure m
fnGroupBy = mkFn4 "groupby"
              (Text.unlines
                [ "Group a list of objects by an attribute."
                , "The attribute can use dot notation for nested access, " <>
                  "e.g. `'address.city'`."
                ]
              )
              ( "value"
              , Nothing :: Maybe [Value m]
              , Just $ TypeDocAny
              , ""
              )
              ( "attribute"
              , Nothing
              , Just $ TypeDocAlternatives [ "string", "int" ]
              , "Attribute to group by."
              )
              ( "default"
              , Just Nothing
              , Just $ TypeDocAny
              , "Default value to use if an object doesn't have the " <>
                "requested attribute."
              )
              ( "case_sensitive"
              , Just True
              , Just $ TypeDocSingle "bool"
              , "Use case-sensitive comparisons for grouping objects."
              )
              (Just $ TypeDocSingle "dict")
  $ \value attrib (defValMay :: Maybe (Value m)) caseSensitive -> do
    let getAttribPath :: Value m -> [Text] -> ExceptT RuntimeError m (Maybe (Value m))
        getAttribPath o [] = pure $ Just o
        getAttribPath o (p:ps) = do
          o'May <- eitherExceptM $ getAttrOrItemRaw o (Identifier p)
          case o'May of
            Just o' -> getAttribPath o' ps
            Nothing -> pure $ Nothing

    let attribProjection :: Value m -> ExceptT RuntimeError m (Maybe (Value m))
        attribProjection = fmap (maybe (Just NoneV) Just) . case attrib of
          Left i -> \obj ->
            lift $ getItemRaw obj (IntV i)
          Right t -> \obj ->
            getAttribPath obj $ Text.splitOn "." t

        attribCaseFold :: Scalar -> Scalar
        attribCaseFold (StringScalar t) =
          if caseSensitive then
            StringScalar t
          else
            StringScalar (Text.toCaseFold t)
        attribCaseFold x = x

        append :: Map Scalar [Value m] -> Maybe (Scalar, Value m) -> Map Scalar [Value m]
        append m Nothing = m
        append m (Just (k, v)) =
          Map.insertWith (flip (++)) k [v] m

        prepare :: Value m -> ExceptT RuntimeError m (Maybe (Scalar, Value m))
        prepare v = do
          kMay <- attribProjection v
          skMay <- mapM
                    (fmap attribCaseFold . eitherExcept . asScalarVal)
                    (kMay <|> defValMay)
          case skMay of
            Nothing -> pure Nothing
            Just k -> pure $ Just (k, v)

    foldl' append mempty <$> mapM prepare value

fnBatch :: forall m. Monad m => Procedure m
fnBatch = mkFn3 "batch"
            "Split up a list into chunks of length `linecount`."
            ( "value"
            , Nothing
            , Just $ TypeDocAny
            , ""
            )
            ( "linecount"
            , Nothing
            , Just $ TypeDocSingle "int"
            , "Number of items per chunk. Unlimited if not specified."
            )
            ( "fill_with"
            , Just Nothing
            , Just $ TypeDocAny
            , "Filler to pad shorter chunks with. If not specified, don't pad."
            )
            (Just $ TypeDocSingle "list")
            $ \value linecount fillWithMay -> do
  pure $ chunksOf fillWithMay linecount value
  where
    chunksOf :: Maybe (Value m) -> Int -> [Value m] -> [[Value m]]
    chunksOf _ _ [] = []
    chunksOf fillMay n xs =
      case take n xs of
        xs' | length xs' < n ->
          let paddingLength = n - length xs'
              padding = case (fillMay, paddingLength) of
                          (Just fill, p) | p > 0 ->
                            replicate paddingLength fill
                          _ -> []
          in [xs' ++ padding]
        xs' -> xs' : chunksOf fillMay n (drop n xs)

fnFirst :: forall m. Monad m => Procedure m
fnFirst = mkFn1 "first"
            "Get the first element from a list, or the first character from a string."
            ( "value"
            , Nothing :: Maybe (Value m)
            , Just $ TypeDocAlternatives [ "list", "string", "bytes" ]
            , ""
            )
            (Just $ TypeDocAny)
  $ \case
    ListV v -> case V.uncons v of
                  Just (x, _) -> pure x
                  Nothing -> pure NoneV
    StringV txt -> pure $ StringV $ Text.take 1 txt
    EncodedV (Encoded txt) -> pure $ EncodedV . Encoded $ Text.take 1 txt
    BytesV arr -> pure . toValue $ BS.indexMaybe arr 0
    x -> throwError $ ArgumentError "first" "value" "list or string" (tagNameOf x)

fnLast :: forall m. Monad m => Procedure m
fnLast = mkFn1 "last"
            "Get the last element from a list, or the last character from a string."
            ( "value"
            , Nothing :: Maybe (Value m)
            , Just $ TypeDocAlternatives [ "list", "string", "bytes" ]
            , ""
            )
            (Just $ TypeDocAny)
  $ \case
    ListV v -> case V.unsnoc v of
                  Just (_, x) -> pure x
                  Nothing -> pure NoneV
    StringV txt -> pure $ StringV $ Text.takeEnd 1 txt
    BytesV arr -> pure . toValue $ BS.indexMaybe arr (BS.length arr - 1)
    EncodedV (Encoded txt) -> pure $ EncodedV . Encoded $ Text.takeEnd 1 txt
    x -> throwError $ ArgumentError "first" "value" "list or string" (tagNameOf x)

autoParseDate :: TimeZone -> Text -> Maybe ZonedTime
autoParseDate defTZ input =
  asum [ parse t (Text.unpack input) | (parse, t) <- formats ]
  where
    ztparse :: String -> String -> Maybe ZonedTime
    ztparse fmt = parseTimeM True defaultTimeLocale fmt
    utcparse :: String -> String -> Maybe ZonedTime
    utcparse fmt i = do
        lt <- parseTimeM True defaultTimeLocale fmt i
        return $ ZonedTime lt defTZ
    formats =
        [ (ztparse, "%Y-%m-%dT%H:%M:%S%Q%Z")
        , (utcparse, "%Y-%m-%d %H:%M:%S%Q")
        , (ztparse, "%Y-%m-%d %H:%M:%S%Q%z")
        , (ztparse, "%Y-%m-%d %H:%M:%S%Q%Z")
        , (utcparse, "%Y-%m-%d")
        ]

dateFromParts :: forall m. Monad m
              => TimeZone
              -> [Value m]
              -> Maybe ZonedTime
dateFromParts defTZ parts = do
  year <- case parts of
            (IntV y : _) -> Just y
            (StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _) -> Nothing
            _ -> Just 2000
  month <- case parts of
            (_ : IntV m : _) -> Just m
            (_ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _) -> Nothing
            _ -> Just 1
  day <- case parts of
            (_ : _ : IntV d : _) -> Just d
            (_ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _) -> Nothing
            _ -> Just 1
  hour <- case parts of
            (_ : _ : _ : IntV h : _) -> Just h
            (_ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  minute <- case parts of
            (_ : _ : _ : _ : IntV m : _) -> Just m
            (_ : _ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  second <- case parts of
            (_ : _ : _ : _ : _ : IntV s : _) -> Just s
            (_ : _ : _ : _ : _ : StringV x : _) -> readMaybe . Text.unpack $ x
            (_ : _ : _ : _ : _ : _ : _) -> Nothing
            _ -> Just 0
  tz <- case parts of
            (_ : _ : _ : _ : _ : _ : v : _) -> parseTZ v
            _ -> Just defTZ
  pure $ ZonedTime
          (LocalTime
            (fromGregorian year (fromInteger month) (fromInteger day))
            (TimeOfDay (fromInteger hour) (fromInteger minute) (fromInteger second)))
          tz

parseTZ :: Value m -> Maybe TimeZone
parseTZ (StringV s) =
  parseTimeM True defaultTimeLocale "%z" $ Text.unpack s
parseTZ (IntV i) =
  Just $ TimeZone (fromInteger i) False ""
parseTZ (ListV v) =
  case V.toList v of
    [IntV minutes, BoolV summerOnly, StringV name] ->
      Just $ TimeZone (fromInteger minutes) summerOnly (Text.unpack name)
    _ -> Nothing
parseTZ _ = Nothing

convertTZ :: Maybe TimeZone -> ZonedTime -> ZonedTime
convertTZ Nothing = id
convertTZ (Just tz) = utcToZonedTime tz . zonedTimeToUTC

fnDateFormat :: forall m. Monad m => Procedure m
fnDateFormat = mkFn4 "dateformat"
                (Text.unlines
                  [ "Format a date/time value."
                  , "Format strings follow the specification found here: " <>
                    "[Date.Time.Format.formatTime](https://hackage.haskell.org/package/time-1.14/docs/Data-Time-Format.html#v:formatTime)"
                  , "Accepted input formats:"
                  , "- `%Y-%m-%dT%H:%M:%S%Q%Z` (2025-11-28T23:54:32.1234UTC)"
                  , "- `%Y-%m-%d %H:%M:%S%Q` (2025-11-28 23:54:32.1234UTC)"
                  , "- `%Y-%m-%d %H:%M:%S%Q%z` (2025-11-28 23:54:32.1234+0100)"
                  , "- `%Y-%m-%d %H:%M:%S%Q%Z` (2025-11-28 23:54:32.1234UTC)"
                  , "- `%Y-%m-%d` (2025-11-28)"
                  ]
                )
                ( "date"
                , Nothing :: Maybe (Either Text [Value m])
                , Just $ TypeDocAlternatives [ "string", "list" ]
                , "May be given as a formatted date, or as a list " <>
                  "of `[ year, month, day, hours, minutes, seconds, " <>
                  "timezone ]`. " <>
                  "Partial lists will be padded with appropriate defaults."
                )
                ( "format"
                , Just "%c"
                , Just $ TypeDocSingle "string"
                , ""
                )
                ( "tz"
                , Just Nothing :: Maybe (Maybe (Value m))
                , Just $ TypeDocAlternatives [ "string", "int", "list" ]
                , "Time zone. May be given as a string specifying an offset " <>
                  "(±HHMM), an integer offset in minutes, or a list " <>
                  "containing 3 elements: offset in minues (int), " <>
                  "summer-only (bool), and timezone name (string)."
                )
                ( "locale"
                , Just Nothing :: Maybe (Maybe Text)
                , Just $ TypeDocAny
                , "Select a locale. Not yet implemented, ignored."
                )
                (Just $ TypeDocSingle "string")
    $ \dateRaw fmt tzVal _localeMay -> do
      let tzMay = parseTZ =<< tzVal
          defTZ = fromMaybe utc tzMay
          locale = defaultTimeLocale -- TODO: use getlocale
      date <- maybe
                (throwError $
                  ArgumentError
                    "dateformat"
                    "date"
                    "date string or date array"
                    (Text.show dateRaw)
                )
                pure $
                case dateRaw of
                  Left str -> autoParseDate defTZ str
                  Right parts -> dateFromParts defTZ parts
      pure . Text.pack . formatTime locale (Text.unpack fmt) . convertTZ tzMay $ date

fnHelp :: forall m. Monad m => Procedure m
fnHelp = mkFn1 "help"
          "Get documentation for the given value, if available."
          ( "value"
          , Nothing :: Maybe (Value m)
          , Just $ TypeDocAny
          , ""
          )
          (Just $ TypeDocSingle "dict")
    $ \value -> do
      case value of
        ProcedureV (NativeProcedure _ doc _) ->
          pure (toValue doc :: Value m)
        _ ->
          pure NoneV

fnDictGet :: forall m. Monad m => Procedure m
fnDictGet = mkFn3 "dict.get"
            "Get an item from a dictionary."
            ( "value"
            , Nothing :: Maybe (Map Scalar (Value m))
            , Just $ TypeDocSingle "dict"
            , ""
            )
            ( "key"
            , Nothing
            , Just $ TypeDocSingle "scalar"
            , ""
            )
            ( "default"
            , Just NoneV :: Maybe (Value m)
            , Just $ TypeDocAny
            , ""
            )
            (Just $ TypeDocAny)
            $ \value key defval -> do
  case Map.lookup key value of
    Nothing -> pure defval
    Just v -> pure v

isUpperVal :: Value m -> Value m
isUpperVal (StringV txt) = BoolV (Text.all isUpper txt)
isUpperVal (EncodedV (Encoded txt)) = BoolV (Text.all isUpper txt)
isUpperVal _ = FalseV

isLowerVal :: Value m -> Value m
isLowerVal (StringV txt) = BoolV (Text.all isLower txt)
isLowerVal (EncodedV (Encoded txt)) = BoolV (Text.all isLower txt)
isLowerVal _ = FalseV

isBoolean :: Bool -> Value m -> Value m
isBoolean b1 (BoolV b2) = BoolV (b1 == b2)
isBoolean _ _ = FalseV

isNone :: Value m -> Value m
isNone NoneV = TrueV
isNone _ = FalseV

isDecimal :: Text -> Bool
isDecimal "" = False
isDecimal t = case Text.splitOn "." t of
  ["0"] ->
    True
  [intpart] ->
    not (Text.null intpart) &&
    Text.all isDigit intpart &&
    not ("0" `Text.isPrefixOf` intpart)
  ["0", fracpart] ->
    Text.all isDigit fracpart
  [intpart, fracpart] ->
    not (Text.null intpart && Text.null fracpart) &&
    Text.all isDigit intpart &&
    not ("0" `Text.isPrefixOf` intpart) &&
    Text.all isDigit fracpart
  _ -> False

--------------------------------------------------------------------------------
-- Text conversion
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

allEitherBool :: [(Either a Bool)] -> Either a Bool
allEitherBool [] = Right True
allEitherBool (Right True : xs) = allEitherBool xs
allEitherBool (x : _) = x

--------------------------------------------------------------------------------
-- Attribute and item helpers
--------------------------------------------------------------------------------

getAttrRaw :: Monad m
        => Value m
        -> Identifier
        -> m (Either RuntimeError (Maybe (Value m)))
getAttrRaw (NativeV n) v =
  Right <$> nativeObjectGetAttribute n v
getAttrRaw (StringV s) v =
  case (Map.lookup v builtinStringAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib s
getAttrRaw (BoolV x) v =
  case (Map.lookup v builtinBoolAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (IntV x) v =
  case (Map.lookup v builtinIntAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (FloatV x) v =
  case (Map.lookup v builtinFloatAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib x
getAttrRaw (ListV xs) v =
  case (Map.lookup v builtinListAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib xs
getAttrRaw (DictV xs) v =
  case (Map.lookup v builtinDictAttribs) of
    Nothing -> pure $ Right Nothing
    Just attrib -> fmap Just <$> attrib xs
getAttrRaw _ _ = pure $ Right Nothing

getItemRaw :: Monad m
           => Value m
           -> Value m
           -> m (Maybe (Value m))
getItemRaw a b = case a of
  DictV m -> case b of
    ScalarV k -> pure $ toValue <$> k `Map.lookup` m
    _ -> pure Nothing
  ListV xs -> case b of
    IntV i -> pure . fmap toValue $ xs V.!? (fromInteger i)
    _ -> pure Nothing
  StringV str -> case b of
    IntV i -> pure
              . fmap (toValue . Text.singleton)
              . listToMaybe
              . Text.unpack
              . Text.take 1
              . Text.drop (fromInteger i)
              $ str
    _ -> pure Nothing
  NativeV n -> case b of
    ScalarV k -> nativeObjectGetField n k
    _ -> pure Nothing
  _ -> pure Nothing

getAttrOrItemRaw :: Monad m
                 => Value m
                 -> Identifier
                 -> m (Either RuntimeError (Maybe (Value m)))
getAttrOrItemRaw a i = runExceptT $ do
  xMay <- eitherExceptM $ getAttrRaw a i
  case xMay of
    Just x -> pure . Just $ x
    Nothing -> lift $ getItemRaw a (StringV . identifierName $ i)

getItemOrAttrRaw :: Monad m
                 => Value m
                 -> Value m
                 -> m (Either RuntimeError (Maybe (Value m)))
getItemOrAttrRaw a b = runExceptT $ do
  xMay <- lift $ getItemRaw a b
  case xMay of
    Just x -> pure . Just $ x
    Nothing -> case b of
      StringV i -> eitherExceptM $ getAttrRaw a (Identifier $ i)
      _ -> pure Nothing


--------------------------------------------------------------------------------
-- Method and property conversion helpers
--------------------------------------------------------------------------------

nativeMethod :: Procedure m -> Value m -> Value m
nativeMethod (NativeProcedure oid doc f) self =
  ProcedureV . NativeProcedure oid doc $ \args -> f ((Just "value", self) : args)
nativeMethod (GingerProcedure env argSpec body) self =
  ProcedureV $ GingerProcedure env' (drop 1 argSpec) body
  where
    env' = env { envVars = Map.insert "value" self (envVars env) }
nativeMethod NamespaceProcedure _self =
  error "'namespace' cannot be used as a method"

nativePureMethod :: Monad m
                 => ObjectID
                 -> Maybe ProcedureDoc
                 -> (Value m -> Either RuntimeError (Value m))
                 -> Value m
                 -> Value m
nativePureMethod oid doc = nativeMethod . pureNativeFunc oid doc

toNativeMethod :: ToNativeProcedure m a
               => ObjectID
               -> Maybe ProcedureDoc
               -> a
               -> Value m
               -> Value m
toNativeMethod oid doc f = nativeMethod (NativeProcedure oid doc $ toNativeProcedure f)

pureAttrib :: Applicative m => (s -> a) -> s -> m (Either RuntimeError a)
pureAttrib f x = pure . Right $ f x

textBuiltin :: (Monad m, ToValue a m)
            => ObjectID
            -> Maybe ProcedureDoc
            -> (Text -> a)
            -> Value m
textBuiltin oid doc f =
  ProcedureV .
  pureNativeFunc oid doc .
  textFunc $
  (Right . f)

intBuiltin :: (Monad m, ToValue a m)
            => ObjectID
            -> Maybe ProcedureDoc
            -> (Integer -> a)
            -> Value m
intBuiltin oid doc f =
  ProcedureV .
  pureNativeFunc oid doc .
  intFunc $
  (Right . f)

numericBuiltin :: (Monad m)
            => ObjectID
            -> Maybe ProcedureDoc
            -> (Integer -> Integer)
            -> (Double -> Double)
            -> Value m
numericBuiltin oid doc f g =
  ProcedureV .
  pureNativeFunc oid doc $
  numericFunc f g

anyBuiltin :: (Monad m, FromValue a m, ToValue b m)
            => ObjectID
            -> Maybe ProcedureDoc
            -> (a -> b)
            -> Value m
anyBuiltin oid doc f =
  ProcedureV .
  nativeFunc oid doc $ \x -> runExceptT $
    toValue . f <$> eitherExceptM (fromValue x)


boolProp :: (Monad m, ToValue a m)
         => (Bool -> a)
         -> Bool
         -> m (Either RuntimeError (Value m))
boolProp f t = pure . Right . toValue $ f t

boolAttrib :: (Monad m, ToValue a m)
           => ObjectID
           -> Maybe ProcedureDoc
           -> (Bool -> a)
           -> Bool
           -> m (Either RuntimeError (Value m))
boolAttrib oid doc f =
  pureAttrib $ nativePureMethod oid doc (boolFunc f) . BoolV

boolNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => ObjectID
                -> Maybe ProcedureDoc
                -> (Value m -> a)
                -> Bool
                -> m (Either RuntimeError (Value m))
boolNProcAttrib oid doc f =
  pureAttrib $ toNativeMethod oid doc f . BoolV

boolProcAttrib :: Monad m
               => Procedure m
               -> Bool
               -> m (Either RuntimeError (Value m))
boolProcAttrib f =
  pureAttrib $ nativeMethod f . BoolV


intProp :: (Monad m, ToValue a m)
         => (Integer -> a)
         -> Integer
         -> m (Either RuntimeError (Value m))
intProp f t = pure . Right . toValue $ f t

intAttrib :: (Monad m, ToValue a m)
           => ObjectID
           -> Maybe ProcedureDoc
           -> (Integer -> a)
           -> Integer
           -> m (Either RuntimeError (Value m))
intAttrib oid doc f =
  pureAttrib $ nativePureMethod oid doc (intFunc (pure . f)) . IntV

intNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => ObjectID
                -> Maybe ProcedureDoc
                -> (Value m -> a)
                -> Integer
                -> m (Either RuntimeError (Value m))
intNProcAttrib oid doc f =
  pureAttrib $ toNativeMethod oid doc f . IntV

intProcAttrib :: Monad m
               => Procedure m
               -> Integer
               -> m (Either RuntimeError (Value m))
intProcAttrib f =
  pureAttrib $ nativeMethod f . IntV


floatProp :: (Monad m, ToValue a m)
         => (Double -> a)
         -> Double
         -> m (Either RuntimeError (Value m))
floatProp f t = pure . Right . toValue $ f t

floatAttrib :: (Monad m, ToValue a m)
           => ObjectID
           -> Maybe ProcedureDoc
           -> (Double -> a)
           -> Double
           -> m (Either RuntimeError (Value m))
floatAttrib oid doc f =
  pureAttrib $ nativePureMethod oid doc (floatFunc (pure . f)) . FloatV

floatNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => ObjectID
                -> Maybe ProcedureDoc
                -> (Value m -> a)
                -> Double
                -> m (Either RuntimeError (Value m))
floatNProcAttrib oid doc f =
  pureAttrib $ toNativeMethod oid doc f . FloatV

floatProcAttrib :: Monad m
               => Procedure m
               -> Double
               -> m (Either RuntimeError (Value m))
floatProcAttrib f =
  pureAttrib $ nativeMethod f . FloatV


textProp :: (Monad m, ToValue a m)
         => (Text -> a)
         -> Text
         -> m (Either RuntimeError (Value m))
textProp f t = pure . Right . toValue $ f t

textAttrib :: (Monad m, ToValue a m)
           => ObjectID
           -> Maybe ProcedureDoc
           -> (Text -> a)
           -> Text
           -> m (Either RuntimeError (Value m))
textAttrib oid doc f =
  pureAttrib $ nativePureMethod oid doc (textFunc (pure . f)) . StringV

textNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => ObjectID
                -> Maybe ProcedureDoc
                -> (Value m -> a)
                -> Text
                -> m (Either RuntimeError (Value m))
textNProcAttrib oid doc f =
  pureAttrib $ toNativeMethod oid doc f . StringV

textProcAttrib :: Monad m
               => Procedure m
               -> Text
               -> m (Either RuntimeError (Value m))
textProcAttrib f =
  pureAttrib $ nativeMethod f . StringV

dictProp :: (Monad m, ToValue a m)
         => (Map Scalar (Value m) -> a)
         -> Map Scalar (Value m)
         -> m (Either RuntimeError (Value m))
dictProp f t = pure . Right . toValue $ f t

dictAttrib :: (Monad m, ToValue a m)
           => ObjectID
           -> Maybe ProcedureDoc
           -> (Map Scalar (Value m) -> a)
           -> Map Scalar (Value m)
           -> m (Either RuntimeError (Value m))
dictAttrib oid doc f =
  pureAttrib $ nativePureMethod oid doc (dictFunc (pure . f)) . DictV

dictNProcAttrib :: (Monad m, ToNativeProcedure m a)
                => ObjectID
                -> Maybe ProcedureDoc
                -> (Value m -> a)
                -> Map Scalar (Value m)
                -> m (Either RuntimeError (Value m))
dictNProcAttrib oid doc f =
  pureAttrib $ toNativeMethod oid doc f . DictV

dictProcAttrib :: Monad m
               => Procedure m
               -> Map Scalar (Value m)
               -> m (Either RuntimeError (Value m))
dictProcAttrib f =
  pureAttrib $ nativeMethod f . DictV

builtinNotImplemented :: Monad m => Text -> Value m
builtinNotImplemented name =
  ProcedureV $
    NativeProcedure
      (ObjectID $ "builtin:not_implemented:" <> name)
      Nothing
      $ \_ _ _ ->
        pure . Left $ NotImplementedError name

fnMaybeArg :: Monad m => Text -> Text -> Maybe b -> ExceptT RuntimeError m b
fnMaybeArg context name =
  maybe
    (throwError $
        ArgumentError
          context
          name
          "argument"
          "end of arguments"
    )
    pure

fnArg :: (Monad m, FromValue a m)
      => Text
      -> Identifier
      -> Map Identifier (Value m)
      -> ExceptT RuntimeError m a
fnArg context name argValues = do
  argV <- fnMaybeArg context (identifierName name) $ Map.lookup name argValues
  eitherExceptM $ fromValue argV

describeArg :: Identifier
            -> Maybe (Value m)
            -> Maybe TypeDoc
            -> Text
            -> ArgumentDoc
describeArg name defMay ty descr =
  ArgumentDoc
    { argumentDocName = identifierName name
    , argumentDocType = ty
    , argumentDocDefault = renderSyntaxText <$> defMay
    , argumentDocDescription = descr
    }

mkFn0' :: ( Monad m
         , ToValue r m
         )
      => Text
      -> Text
      -> Maybe TypeDoc
      -> (Context m -> SomePRNG -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn0' funcName desc retType f =
  NativeProcedure (ObjectID $ "builtin:" <> funcName)
    (Just ProcedureDoc
      { procedureDocName = funcName
      , procedureDocArgs = mempty
      , procedureDocReturnType = retType
      , procedureDocDescription = desc
      }
    )
    $ \args ctx rng -> runExceptT $ do
      _ <- eitherExcept $
        resolveArgs
          funcName
          []
          args
      toValue <$> f ctx rng

mkFn0 :: ( Monad m
         , ToValue r m
         )
      => Text
      -> Text
      -> Maybe TypeDoc
      -> (ExceptT RuntimeError m r)
      -> Procedure m
mkFn0 funcName desc retType f =
  mkFn0' funcName desc retType (const . const $ f)

mkFn1' :: forall m a r.
          ( Monad m
          , ToValue a m
          , FromValue a m
          , ToValue r m
          )
       => Text
       -> Text
       -> (Identifier, Maybe a, Maybe TypeDoc, Text)
       -> Maybe TypeDoc
       -> (Context m -> SomePRNG -> a -> ExceptT RuntimeError m r)
       -> Procedure m
mkFn1' funcName desc (argname1, default1, typedoc1, argdesc1) retType f =
  NativeProcedure (ObjectID $ "builtin:" <> funcName)
    (Just ProcedureDoc
      { procedureDocName = funcName
      , procedureDocArgs =
        [ describeArg @m argname1 (toValue <$> default1) typedoc1 argdesc1
        ]
      , procedureDocReturnType = retType
      , procedureDocDescription = desc
      }
    )
    $ \args ctx rng -> runExceptT $ do
      argValues <- eitherExcept $
        resolveArgs
          funcName
          [ (argname1, toValue <$> default1)
          ]
          args
      arg1 <- fnArg funcName argname1 argValues
      toValue <$> f ctx rng arg1

mkFn1 :: ( Monad m
         , ToValue a m
         , FromValue a m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (a -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn1 funcName a desc retType f =
  mkFn1' funcName a desc retType (const . const $ f)

mkFn2' :: forall m a1 a2 r.
         ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (Context m -> SomePRNG -> a1 -> a2 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn2' funcName desc
    (argname1, default1, typedoc1, argdesc1)
    (argname2, default2, typedoc2, argdesc2)
    retType
    f =
  NativeProcedure (ObjectID $ "builtin:" <> funcName)
    (Just ProcedureDoc
      { procedureDocName = funcName
      , procedureDocArgs =
        [ describeArg @m argname1 (toValue <$> default1) typedoc1 argdesc1
        , describeArg @m argname2 (toValue <$> default2) typedoc2 argdesc2
        ]
      , procedureDocReturnType = retType
      , procedureDocDescription = desc
      }
    )
    $ \args ctx rng -> runExceptT $ do
      argValues <- eitherExcept $
        resolveArgs
          funcName
          [ (argname1, toValue <$> default1)
          , (argname2, toValue <$> default2)
          ]
          args
      arg1 <- fnArg funcName argname1 argValues
      arg2 <- fnArg funcName argname2 argValues
      toValue <$> f ctx rng arg1 arg2

mkFn2 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (a1 -> a2 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn2 funcName desc a b retType f =
  mkFn2' funcName desc a b retType (const . const $ f)

mkFn3' :: forall m a1 a2 a3 r.
         ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a3, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (Context m -> SomePRNG -> a1 -> a2 -> a3 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn3' funcName desc
    (argname1, default1, typedoc1, argdesc1)
    (argname2, default2, typedoc2, argdesc2)
    (argname3, default3, typedoc3, argdesc3)
    retType
    f =
  NativeProcedure (ObjectID $ "builtin:" <> funcName)
    (Just ProcedureDoc
      { procedureDocName = funcName
      , procedureDocArgs =
        [ describeArg @m argname1 (toValue <$> default1) typedoc1 argdesc1
        , describeArg @m argname2 (toValue <$> default2) typedoc2 argdesc2
        , describeArg @m argname3 (toValue <$> default3) typedoc3 argdesc3
        ]
      , procedureDocReturnType = retType
      , procedureDocDescription = desc
      }
    )
    $ \args ctx rng -> runExceptT $ do
      argValues <- eitherExcept $
        resolveArgs
          funcName
          [ (argname1, toValue <$> default1)
          , (argname2, toValue <$> default2)
          , (argname3, toValue <$> default3)
          ]
          args
      arg1 <- fnArg funcName argname1 argValues
      arg2 <- fnArg funcName argname2 argValues
      arg3 <- fnArg funcName argname3 argValues
      toValue <$> f ctx rng arg1 arg2 arg3

mkFn3 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a3, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (a1 -> a2 -> a3 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn3 funcName desc a b c retType f =
  mkFn3' funcName desc a b c retType (const . const $ f)

mkFn4' :: forall m a1 a2 a3 a4 r.
         ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue a4 m
         , FromValue a4 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a3, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a4, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (Context m -> SomePRNG -> a1 -> a2 -> a3 -> a4 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn4' funcName desc
    (argname1, default1, typedoc1, argdesc1)
    (argname2, default2, typedoc2, argdesc2)
    (argname3, default3, typedoc3, argdesc3)
    (argname4, default4, typedoc4, argdesc4)
    retType
    f =
  NativeProcedure (ObjectID $ "builtin:" <> funcName)
    (Just ProcedureDoc
      { procedureDocName = funcName
      , procedureDocArgs =
        [ describeArg @m argname1 (toValue <$> default1) typedoc1 argdesc1
        , describeArg @m argname2 (toValue <$> default2) typedoc2 argdesc2
        , describeArg @m argname3 (toValue <$> default3) typedoc3 argdesc3
        , describeArg @m argname4 (toValue <$> default4) typedoc4 argdesc4
        ]
      , procedureDocReturnType = retType
      , procedureDocDescription = desc
      }
    )
    $ \args ctx rng -> runExceptT $ do
      argValues <- eitherExcept $
        resolveArgs
          funcName
          [ (argname1, toValue <$> default1)
          , (argname2, toValue <$> default2)
          , (argname3, toValue <$> default3)
          , (argname4, toValue <$> default4)
          ]
          args
      arg1 <- fnArg funcName argname1 argValues
      arg2 <- fnArg funcName argname2 argValues
      arg3 <- fnArg funcName argname3 argValues
      arg4 <- fnArg funcName argname4 argValues
      toValue <$> f ctx rng arg1 arg2 arg3 arg4

mkFn4 :: ( Monad m
         , ToValue a1 m
         , FromValue a1 m
         , ToValue a2 m
         , FromValue a2 m
         , ToValue a3 m
         , FromValue a3 m
         , ToValue a4 m
         , FromValue a4 m
         , ToValue r m
         )
      => Text
      -> Text
      -> (Identifier, Maybe a1, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a2, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a3, Maybe TypeDoc, Text)
      -> (Identifier, Maybe a4, Maybe TypeDoc, Text)
      -> Maybe TypeDoc
      -> (a1 -> a2 -> a3 -> a4 -> ExceptT RuntimeError m r)
      -> Procedure m
mkFn4 funcName desc a b c d retType f =
  mkFn4' funcName desc a b c d retType (const . const $ f)
