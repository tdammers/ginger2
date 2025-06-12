{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Ginger.BuiltinsAutodoc
where

import Control.Monad.Random (MonadRandom (..), RandomGen (..), random, randoms)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Language.Haskell.TH (getDoc, putDoc, DocLoc (..), DecsQ, runIO)

import Language.Ginger.Interpret
import Language.Ginger.Interpret.Builtins
    ( builtinGlobals
    , builtinGlobalsNonJinja
    , builtinBoolAttribs
    , builtinIntAttribs
    , builtinFloatAttribs
    , builtinStringAttribs
    , builtinListAttribs
    , builtinDictAttribs
    , BuiltinAttribs
    )
import Language.Ginger.Interpret.DefEnv
    ( builtinTests
    , builtinFilters
    )
import Language.Ginger.AST
import Language.Ginger.Value

newtype Document a = Document { unDocument :: Identity a }
  deriving (Functor, Applicative, Monad)

runDocument :: Document a -> a
runDocument = runIdentity . unDocument

data MockGen = MockGen

instance RandomGen MockGen where
  genWord64 g = (0, g)
  genWord32 g = (0, g)
  split g = (g, g)

instance MonadRandom Document where
  getRandomR (a, _) = pure a
  getRandomRs (a, _) = pure $ repeat a
  getRandom = pure . fst $ random MockGen
  getRandoms = pure $ randoms MockGen

markdownToHaddock :: Text -> Text
markdownToHaddock =
  Text.replace "'" "\\'" .
  Text.replace "\"" "\\\"" .
  Text.replace "`" "@" .
  Text.replace "```" "@"

addHaddockFromFile :: FilePath -> DecsQ
addHaddockFromFile path = do
  src <- runIO $ readFile path
  doc <- fromMaybe "" <$> getDoc ModuleDoc
  putDoc ModuleDoc $ doc ++ src
  pure []

extractAttribs :: Monoid a
               => BuiltinAttribs a Document
               -> [(Identifier, Value Document)]
extractAttribs =
  extractAttribsWith mempty

extractAttribsWith :: a
                   -> BuiltinAttribs a Document
                   -> [(Identifier, Value Document)]
extractAttribsWith dummy =
  Map.toAscList .
  fmap (
    either (error . show) id .
    runDocument .
    ($ dummy)
  )

builtinsAutodoc :: DecsQ
builtinsAutodoc = do
  doc <- fromMaybe "" <$> getDoc ModuleDoc
  putDoc ModuleDoc $
    doc ++
    "\n\n== __List Of Builtin Globals__\n" ++
    "\nThese are available in Jinja, and work (mostly) the same in Ginger.\n" ++
    unlines (map (goItem Nothing "globals_jinja_") (Map.toAscList $ builtinGlobals evalE)) ++

    "\n\n== __List Of Extension Globals__\n" ++
    "\nThese are not available in Jinja\n" ++
    unlines (map (goItem Nothing "globals_ginger_") (Map.toAscList $ builtinGlobalsNonJinja evalE)) ++

    "\n\n== __List Of Builtin Attributes__\n" ++
    "\n\n=== Bool\n" ++
    unlines
      (map
        (goItem (Just "bool") "globals_bool_")
        (extractAttribsWith False builtinBoolAttribs)) ++
    "\n\n=== Int\n" ++
    unlines
      (map
        (goItem (Just "int") "globals_int_")
        (extractAttribsWith 0 builtinIntAttribs)) ++
    "\n\n=== Float\n" ++
    unlines
      (map
        (goItem (Just "float") "globals_float_")
        (extractAttribsWith 0 builtinFloatAttribs)) ++
    "\n\n=== String\n" ++
    unlines
      (map
        (goItem (Just "string") "globals_string_")
        (extractAttribs builtinStringAttribs)) ++
    "\n\n=== List\n" ++
    unlines
      (map
        (goItem (Just "list") "globals_list_")
        (extractAttribs builtinListAttribs)) ++
    "\n\n=== Dict\n" ++
    unlines
      (map
        (goItem (Just "dict") "globals_dict_")
        (extractAttribs builtinDictAttribs)) ++

    "\n\n== __List Of Builtin Filters__\n" ++
    "\nThese will only work in a filter context, not via procedure call syntax.\n" ++
    unlines (map (goItem Nothing "filters_") (Map.toAscList $ builtinFilters)) ++

    "\n\n== __List Of Builtin Tests__\n" ++
    "\nThese will only work in a test context (e.g., an @is@-expression).\n\n" ++
    "\nSome of these tests shadow globals of the same name but different functionality.\n\n" ++
    unlines (map (goItem Nothing "tests_") (Map.toAscList $ builtinTests))
  pure []
  where
    goTy :: TypeDoc -> Text
    goTy TypeDocAny = "any"
    goTy TypeDocNone = "none"
    goTy (TypeDocSingle t) = t
    goTy (TypeDocAlternatives ts) =
      "[" <> Text.intercalate " | " (Vector.toList $ ts) <> "]"

    goItemHeading :: Maybe Text -> Text -> Text -> [Text]
    goItemHeading namespaceMay prefix name =
        [ ""
        , "#" <> prefix <> maybe "" (<> ".") namespaceMay <> name <> "#"
        , ""
        , "=== " <> maybe "" (<> ".") namespaceMay <> name
        ]

    goArgSig :: ArgumentDoc -> Text
    goArgSig arg =
      argumentDocName arg <>
      maybe "" ("=" <>) (argumentDocDefault arg) <>
      maybe "" ((" : " <>) . goTy) (argumentDocType arg)

    goArgDesc :: ArgumentDoc -> [Text]
    goArgDesc arg =
      [ "[@" <> argumentDocName arg <> "@]:" <> argumentDocDescription arg ]

    goDocumentedItem :: Maybe Text -> Text -> (Identifier, ProcedureDoc) -> String
    goDocumentedItem namespaceMay prefix (name, d) =
      let qualifiedName = maybe "" (<> ".") namespaceMay <> identifierName name
      in
        Text.unpack . Text.unlines $
          goItemHeading namespaceMay prefix
            (identifierName name)
            ++
          [ ""
          , "@" <> identifierName name
                <> "(" <> (Text.intercalate ", " . map goArgSig . Vector.toList $ procedureDocArgs d) <> ")"
                <> maybe "" ((" → " <>) . goTy) (procedureDocReturnType d)
                <> "@"
          , ""
          ]
            ++
          ( if qualifiedName /= procedureDocName d &&
               identifierName name /= procedureDocName d then
              [ "Alias for [" <> procedureDocName d <> "](#" <> prefix <> procedureDocName d <> ")"
              ]
            else
              ( if Vector.null (procedureDocArgs d) then
                  [ ]
                else
                  [ ""
                  , "==== Arguments"
                  , ""
                  ] ++
                  (concatMap goArgDesc . Vector.toList $ procedureDocArgs d) ++
                  [ "" ]
              ) ++
              [ ""
              , markdownToHaddock $ procedureDocDescription d
              ]
          )

    goItem :: Maybe Text -> Text -> (Identifier, Value Document) -> String
    goItem namespaceMay prefix (name, DictV subitems) =
      let qualifiedName = maybe "" (<> ".") namespaceMay <> identifierName name
          heading = Text.unpack . Text.unlines $
                      [ ""
                      , "=== Module \\'" <> qualifiedName <> "\\'"
                      ]
      in
        heading ++
        unlines
          [ goItem (Just qualifiedName) prefix (Identifier k, v)
          | (StringScalar k, v) <- Map.toAscList subitems
          ]

    goItem namespaceMay prefix (name, ProcedureV (NativeProcedure _ (Just d) _)) =
      goDocumentedItem namespaceMay prefix (name, d)
    goItem namespaceMay prefix (name, ProcedureV NamespaceProcedure) =
      goDocumentedItem namespaceMay prefix (name, namespaceProcedureDoc)
    goItem namespaceMay prefix (name, FilterV (NativeFilter (Just d) _)) =
      goDocumentedItem namespaceMay prefix (name, d)
    goItem namespaceMay prefix (name, TestV (NativeTest (Just d) _)) =
      goDocumentedItem namespaceMay prefix (name, d)
    goItem namespaceMay prefix (name, _) =
      Text.unpack . Text.unlines $
        goItemHeading namespaceMay prefix (identifierName name)
