{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger.StringFormatting.Printf
where

import Control.Applicative ( (<|>) )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf (printf, PrintfArg)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Char (isDigit)

printfList :: PrintfArg a => String -> [a] -> String
printfList fmt args =
  leader ++ concat (zipWith printf fmts args)
  where
    (leader, fmts) = splitPrintfFormat fmt

splitPrintfFormat :: String -> (String, [String])
splitPrintfFormat fmt =
  fromMaybe ("", []) $ P.parseMaybe pPrintfFormats fmt

type P a = P.Parsec Void String a

pPrintfFormats :: P (String, [String])
pPrintfFormats = (,) <$> pLeader <*> P.many pPrintfFormat

pLeader :: P String
pLeader = concat <$> P.many (("%" <$ pDoublePercent) <|> pPrintfPlainChars)

pTrailer :: P String
pTrailer = concat <$> P.many (pDoublePercent <|> pPrintfPlainChars)

pDoublePercent :: P String
pDoublePercent = P.chunk "%%"

pPrintfPlainChars :: P String
pPrintfPlainChars = P.takeWhile1P (Just "plain characters") (/= '%')

pPrintfFormat :: P String
pPrintfFormat = do
  leadingPercent <- P.chunk "%"
  flags <- P.takeWhileP (Just "flags") (`elem` flagChars)
  fieldWidth <- pFieldWidth
  precision <- P.option "" $ (P.chunk "." *> pFieldWidth)
  widthModifier <- P.takeWhileP (Just "width modifier") (`elem` widthChars)
  formatChar <- P.choice
    [ 'd' <$ P.char 'i'
    , 'v' <$ P.satisfy (`elem` stringFormatChars)
    , P.satisfy (`elem` formatChars)
    , pure 'v'
    ]
  remaining <- pTrailer
  pure $ concat
    [ leadingPercent
    , flags
    , fieldWidth
    , precision
    , widthModifier
    , [formatChar]
    , remaining
    ]

  where
    pFieldWidth :: P String
    pFieldWidth = P.chunk "*" <|> pNumericFieldWidth

    pNumericFieldWidth :: P String
    pNumericFieldWidth =
      (++) <$> P.option "" (P.chunk "-")
           <*> P.takeWhileP (Just "numeric field width") isDigit

    flagChars :: [Char]
    flagChars = " +-0#"

    widthChars :: [Char]
    widthChars = "lLhH"

    stringFormatChars :: [Char]
    stringFormatChars = "rsa"

    formatChars :: [Char]
    formatChars = "cdobuxXfFgGeEsv"

