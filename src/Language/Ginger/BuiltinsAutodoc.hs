{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger.BuiltinsAutodoc
where

import Control.Monad.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Language.Haskell.TH (getDoc, putDoc, DocLoc (..), DecsQ, runIO)

import Language.Ginger.Interpret
import Language.Ginger.Interpret.Builtins (builtinGlobals, builtinGlobalsNonJinja)
import Language.Ginger.Interpret.DefEnv
        ( builtinTests
        , builtinFilters
        )
import Language.Ginger.AST
import Language.Ginger.Value

markdownToHaddock :: Text -> Text
markdownToHaddock =
  Text.replace "'" "\\'" .
  Text.replace "\"" "\\\"" .
  Text.replace "\n" "\n\n" .
  Text.replace "`" "@"

addHaddockFromFile :: FilePath -> DecsQ
addHaddockFromFile path = do
  src <- runIO $ readFile path
  doc <- fromMaybe "" <$> getDoc ModuleDoc
  putDoc ModuleDoc $ doc ++ src
  pure []

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
    goTy TypeDocAny = "@any@"
    goTy TypeDocNone = "@none@"
    goTy (TypeDocSingle t) = "@" <> t <> "@"
    goTy (TypeDocAlternatives ts) =
      case Vector.unsnoc ts of
        Just (ts', t) ->
          "@" <> Text.intercalate "@, @" (Vector.toList $ ts') <> "@" <>
          ", or @" <> t <> "@"
        Nothing ->
          "@" <> Text.intercalate "@ or @" (Vector.toList $ ts) <> "@"

    goItemHeading :: Maybe Text -> Text -> Text -> [Text]
    goItemHeading namespaceMay prefix name =
        [ ""
        , "#" <> prefix <> maybe "" (<> ".") namespaceMay <> name <> "#"
        , ""
        , "=== " <> maybe "" (<> ".") namespaceMay <> name
        ]


    goDocumentedItem :: Maybe Text -> Text -> (Identifier, ProcedureDoc) -> String
    goDocumentedItem namespaceMay prefix (name, d) =
      let qualifiedName = maybe "" (<> ".") namespaceMay <> identifierName name
      in
        Text.unpack . Text.unlines $
          goItemHeading namespaceMay prefix (identifierName name) ++
          ( if qualifiedName /= procedureDocName d then
              [ "Alias for [" <> procedureDocName d <> "](#" <> prefix <> procedureDocName d <> ")"
              ]
            else
              [ ""
              , "Arguments:"
              , ""
              ] ++
              [ "* @" <> argumentDocName arg
              <> case argumentDocDefault arg of
                  Nothing -> ""
                  Just defval -> "=" <> defval
              <> "@"
              <> maybe "" ((" : " <>) . goTy) (argumentDocType arg)
              <> case argumentDocDefault arg of
                  Nothing -> " __(required)__"
                  Just _ -> ""
              | arg <- Vector.toList (procedureDocArgs d)
              ] ++
              [ ""
              , "Return type: " <> maybe "n/a" goTy (procedureDocReturnType d)
              , ""
              , markdownToHaddock $ procedureDocDescription d
              ]
          )

    goItem :: Maybe Text -> Text -> (Identifier, Value Identity) -> String
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
    goItem namespaceMay prefix (name, FilterV (NativeFilter (Just d) _)) =
      goDocumentedItem namespaceMay prefix (name, d)
    goItem namespaceMay prefix (name, TestV (NativeTest (Just d) _)) =
      goDocumentedItem namespaceMay prefix (name, d)
    goItem namespaceMay prefix (name, _) =
      Text.unpack . Text.unlines $
        goItemHeading namespaceMay prefix (identifierName name)
