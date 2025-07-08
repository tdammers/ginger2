{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Werror #-}

module Main where

import Language.Ginger
import Language.Ginger.Interpret.Builtins (textBuiltin)
import Language.Ginger.Value

import qualified CMark
import CMark (commonmarkToHtml)
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, takeFileName, takeExtension, (</>) )
import System.IO (hPutStrLn, stderr)
import qualified System.Random as R
import Control.Monad.Random (MonadTrans)

import qualified Data.Yaml as YAML

data VarValue
  = StringVar !Text
  | IntVar !Integer
  | NullVar
  deriving (Show, Eq)

instance ToValue VarValue m where
  toValue (StringVar t) = StringV t
  toValue (IntVar i) = IntV i
  toValue NullVar = NoneV

data EncoderChoice
  = HtmlEncoder
  | TextEncoder
  | AutoEncoder
  deriving (Show, Eq, Ord, Enum, Bounded)

data ProgramOptions =
  ProgramOptions
    { poDataFiles :: [(Maybe Identifier, FilePath)]
    , poDataItems :: [(Identifier, VarValue)]
    , poSourceFile :: Maybe FilePath
    , poOutputFile :: Maybe FilePath
    , poTrimBlocks :: BlockTrimming
    , poStripBlocks :: BlockStripping
    , poEncoder :: EncoderChoice
    , poDialect :: JinjaDialect
    }
    deriving (Show, Eq)

defProgramOptions :: ProgramOptions
defProgramOptions =
  ProgramOptions
    { poDataFiles = []
    , poDataItems = []
    , poSourceFile = Nothing
    , poOutputFile = Nothing
    , poTrimBlocks = pstateTrimBlocks defPOptions
    , poStripBlocks = pstateStripBlocks defPOptions
    , poEncoder = HtmlEncoder
    , poDialect = DialectGinger2
    }

keyValuePair :: ReadM (Identifier, VarValue)
keyValuePair = parseKeyValuePair <$> str

parseKeyValuePair :: String -> (Identifier, VarValue)
parseKeyValuePair s =
  case (kStr, vStr) of
    (k, ':' : v) | all isDigit v -> (Identifier $ Text.pack k, IntVar $ read v)
    (k, ':' : v) -> (Identifier $ Text.pack k, StringVar $ Text.pack v)
    (k, _) -> (Identifier $ Text.pack k, NullVar)
  where
    (kStr, vStr) = break (== ':') s

dataFileEntry :: ReadM (Maybe Identifier, FilePath)
dataFileEntry = parseDataFileEntry <$> str

parseDataFileEntry :: String -> (Maybe Identifier, FilePath)
parseDataFileEntry s =
  case (fStr, pStr) of
    (f, '@' : p) -> (Just . Identifier $ Text.pack p, f)
    (f, _) -> (Nothing, f)
  where
    (fStr, pStr) = break (== '@') s

programOptions :: Parser ProgramOptions
programOptions =
  ProgramOptions
    <$> many
          (
            argument dataFileEntry
              ( metavar "DATAFILE{@KEY}"
              <> help "JSON or YAML data file"
              )
          <|>
            option dataFileEntry
              ( metavar "DATAFILE{@KEY}"
              <> short 'd'
              <> long "data-file"
              <> help "JSON or YAML data file"
              )
          )
    <*> many
          ( option keyValuePair
              ( long "data-item"
              <> short 'D'
              <> help "Define one template variable"
              <> metavar "VAR:VALUE"
              )
          )
    <*> option (Just <$> str)
          ( long "template"
          <> short 't'
          <> metavar "TEMPLATE"
          <> help "Template file (STDIN if not provided)"
          <> value Nothing
          )
    <*> option (Just <$> str)
          ( long "output"
          <> short 'o'
          <> metavar "OUTFILE"
          <> help "Output file (STDOUT if not provided)"
          <> value Nothing
          )
    <*> ( flag' TrimBlocks
            ( long "trim-blocks"
            <> help "Enable block trimming"
            )
          <|>
          flag' NoTrimBlocks
            ( long "no-trim-blocks"
            <> help "Disable block trimming"
            )
          <|> pure (pstateTrimBlocks defPOptions)
        )
    <*> ( flag' StripBlocks
            ( long "strip-blocks"
            <> help "Enable block stripping"
            )
          <|>
          flag' NoStripBlocks
            ( long "no-strip-blocks"
            <> help "Disable block stripping"
            )
          <|> pure (pstateStripBlocks defPOptions)
        )
    <*> option encoderReader
          ( long "encoder"
          <> metavar "ENCODER"
          <> help
              ( "Output encoding ('html', 'text', or 'auto'). " ++
                "'auto' will guess encoding from output file extension, then " ++
                "template file extension, then default to 'html'")
          <> value AutoEncoder
          )
    <*> option dialectReader
          ( long "dialect"
          <> metavar "DIALECT"
          <> help
              ( "Jinja dialect. Valid options: " ++
                "'jinja' (compatibility mode), " ++
                "'ginger' (ginger2-specific extensions, default)"
              )
          <> value DialectGinger2
          )

encoderReader :: ReadM EncoderChoice
encoderReader = eitherReader $ \case
  "html" -> Right HtmlEncoder
  "text" -> Right TextEncoder
  "auto" -> Right AutoEncoder
  s -> Left $ "Invalid encoder: " ++ show s

dialectReader :: ReadM JinjaDialect
dialectReader = eitherReader $ \case
  "ginger" -> Right DialectGinger2
  "ginger2" -> Right DialectGinger2

  "jinja" -> Right DialectJinja2
  "jinja2" -> Right DialectJinja2
  "compat" -> Right DialectJinja2
  s -> Left $ "Invalid dialect: " ++ show s

main :: IO ()
main = do
  po <- execParser $
          info (programOptions <**> helper)
            ( fullDesc
            <> header "ginger - command-line jinja2 template interpreter"
            )
  runWithOptions po

fileOrStdinLoader :: FilePath -> TemplateLoader IO
fileOrStdinLoader baseDir templateName =
  case templateName of
    "" -> Just <$> Text.getContents
    n -> Just <$> Text.readFile (baseDir </> Text.unpack n)

textEncoder :: Encoder IO
textEncoder txt = do
  pure $ Encoded txt

unpackToKey :: [Identifier] -> Value (t IO) -> Map Identifier (Value (t IO))
unpackToKey [] _ = error "Empty key"
unpackToKey [x] v = Map.singleton x v
unpackToKey (x:xs) v =
  Map.singleton x (toValue $ unpackToKey xs v)

splitIdentifier :: Identifier -> [Identifier]
splitIdentifier (Identifier path) = map Identifier $ Text.splitOn "." path

splitIdentifierMaybe :: Maybe Identifier -> [Identifier]
splitIdentifierMaybe = maybe [] splitIdentifier

loadDataFile :: MonadTrans t => (Maybe Identifier, FilePath) -> IO (Map Identifier (Value (t IO)))
loadDataFile (Nothing, path) =
  YAML.decodeFileThrow path
loadDataFile (Just k, path) = do
  fileData <- YAML.decodeFileThrow path
  pure $ unpackToKey (splitIdentifier k) fileData

unpackVars :: MonadTrans t => [(Identifier, VarValue)] -> Map Identifier (Value (t IO))
unpackVars = mconcat . map unpackVar

unpackVar :: MonadTrans t => (Identifier, VarValue) -> Map Identifier (Value (t IO))
unpackVar (k, varVal) = unpackVal (k, toValue varVal)

unpackVal :: MonadTrans t => (Identifier, Value (t IO)) -> Map Identifier (Value (t IO))
unpackVal (k, val) = unpackValToKey (splitIdentifier k) val

unpackValToKey :: MonadTrans t => [Identifier] -> Value (t IO) -> Map Identifier (Value (t IO))
unpackValToKey [] _ = mempty
unpackValToKey [x] val = Map.singleton x val
unpackValToKey (x:xs) val = Map.singleton x (toValue $ unpackValToKey xs val)

runWithOptions :: ProgramOptions -> IO ()
runWithOptions po = do
  (baseDir, templateName) <- case poSourceFile po of
    Nothing -> (,) <$> getCurrentDirectory <*> pure ""
    Just path -> pure (takeDirectory path, Text.pack $ takeFileName path)
  fileVars <- mconcat <$> mapM loadDataFile (poDataFiles po)
  let definedVars = unpackVars (poDataItems po)
  let vars = definedVars <> fileVars
  let encoder = case poEncoder po of
        HtmlEncoder -> htmlEncoder
        TextEncoder -> textEncoder
        AutoEncoder ->
          let outputExt = maybe "" takeExtension $ poOutputFile po
              ext = case outputExt of
                      "" -> takeExtension (Text.unpack templateName)
                      _ -> outputExt
          in case ext of
            ".txt" -> textEncoder
            ".text" -> textEncoder
            _ -> htmlEncoder
  rng <- R.initStdGen
  ginger
    (fileOrStdinLoader baseDir)
    defPOptions
      { pstateTrimBlocks = poTrimBlocks po
      , pstateStripBlocks = poStripBlocks po
      }
    (poDialect po)
    rng
    encoder
    templateName
    (vars <> extensions) >>= printResultTo (poOutputFile po)

printResult :: Either RuntimeError Encoded -> IO ()
printResult = printResultTo Nothing

printResultTo :: Maybe FilePath -> Either RuntimeError Encoded -> IO ()
printResultTo _ (Left err) =
  hPutStrLn stderr $ prettyRuntimeError err
printResultTo Nothing (Right output) =
  Text.putStrLn $ encoded output
printResultTo (Just outputPath) (Right output) =
  Text.writeFile outputPath $ encoded output

demo :: IO ()
demo = runWithOptions $ defProgramOptions
  { poSourceFile = Just "./test.html"
  , poEncoder = TextEncoder
  }

extensions :: forall m. Monad m => Map Identifier (Value m)
extensions = Map.fromList
  [ ("markdown"
    , textBuiltin
        "extensions:markdown"
        (Just ProcedureDoc
          { procedureDocName = "markdown"
          , procedureDocArgs =
              [ ArgumentDoc
                  "value"
                  (Just $ TypeDocSingle "string")
                  Nothing
                  "Markdown source (CommonMark)"
              ]
          , procedureDocReturnType = (Just $ TypeDocSingle "encoded")
          , procedureDocDescription = "Convert CommonMark to HTML"
          }
        )
        (EncodedV @m . Encoded . commonmarkToHtml [CMark.optSafe])
    )
  ]
