{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror #-}

module Main where

import Language.Ginger
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath (takeDirectory, takeFileName, takeExtension, (</>) )
import System.Directory (getCurrentDirectory)
import System.IO (hPrint, stderr)
import Options.Applicative

data EncoderChoice
  = HtmlEncoder
  | TextEncoder
  | AutoEncoder
  deriving (Show, Eq, Ord, Enum, Bounded)

data ProgramOptions =
  ProgramOptions
    { poSourceFile :: Maybe FilePath
    , poOutputFile :: Maybe FilePath
    , poTrimBlocks :: BlockTrimming
    , poStripBlocks :: BlockStripping
    , poEncoder :: EncoderChoice
    }
    deriving (Show, Eq)

defProgramOptions :: ProgramOptions
defProgramOptions =
  ProgramOptions
    { poSourceFile = Nothing
    , poOutputFile = Nothing
    , poTrimBlocks = pstateTrimBlocks defPOptions
    , poStripBlocks = pstateStripBlocks defPOptions
    , poEncoder = HtmlEncoder
    }

programOptions :: Parser ProgramOptions
programOptions =
  ProgramOptions
    <$> argument (Just <$> str)
          ( metavar "TEMPLATE"
          <> value Nothing
          <> help "Template source file"
          )
    <*> option (Just <$> str)
          ( long "output"
          <> short 'o'
          <> metavar "OUTFILE"
          <> help "Output file"
          <> value Nothing
          )
    <*> ( flag' TrimBlocks
            ( long "trim-blocks"
            <> help "enable block trimming"
            )
          <|>
          flag' NoTrimBlocks
            ( long "no-trim-blocks"
            <> help "disable block trimming"
            )
          <|> pure (pstateTrimBlocks defPOptions)
        )
    <*> ( flag' StripBlocks
            ( long "strip-blocks"
            <> help "enable block stripping"
            )
          <|>
          flag' NoStripBlocks
            ( long "no-strip-blocks"
            <> help "disable block stripping"
            )
          <|> pure (pstateStripBlocks defPOptions)
        )
    <*> option encoderReader
          ( long "encoder"
          <> metavar "ENCODER"
          <> help "output encoding ('html', 'text', or 'auto')"
          <> value AutoEncoder
          )

encoderReader :: ReadM EncoderChoice
encoderReader = eitherReader $ \case
  "html" -> Right HtmlEncoder
  "text" -> Right TextEncoder
  "auto" -> Right AutoEncoder
  s -> Left $ "Invalid reader: " ++ show s
        

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

runWithOptions :: ProgramOptions -> IO ()
runWithOptions po = do
  (baseDir, templateName) <- case poSourceFile po of
    Nothing -> (,) <$> getCurrentDirectory <*> pure ""
    Just path -> pure (takeDirectory path, Text.pack $ takeFileName path)
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
  ginger
    (fileOrStdinLoader baseDir)
    defPOptions
      { pstateTrimBlocks = poTrimBlocks po
      , pstateStripBlocks = poStripBlocks po
      }
    encoder
    templateName
    mempty >>= printResultTo (poOutputFile po)

printResult :: Either RuntimeError Encoded -> IO ()
printResult = printResultTo Nothing

printResultTo :: Maybe FilePath -> Either RuntimeError Encoded -> IO ()
printResultTo _ (Left err) =
  hPrint stderr err
printResultTo Nothing (Right output) =
  Text.putStrLn $ encoded output
printResultTo (Just outputPath) (Right output) =
  Text.writeFile outputPath $ encoded output

demo :: IO ()
demo = runWithOptions $ defProgramOptions
  { poSourceFile = Just "./test.html"
  , poEncoder = TextEncoder
  }
