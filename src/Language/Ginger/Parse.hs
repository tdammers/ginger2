{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Ginger.Parse
where

import Control.Monad (void)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isAlphaNum, isAlpha)

-- import Language.Ginger.Value
import Language.Ginger.AST

type P = Parsec Void Text

identifierCharP :: P Char
identifierCharP = satisfy isIdentifierChar

isIdentifierChar :: Char -> Bool
isIdentifierChar c =
  isAlphaNum c || c == '_'

identifierInitialCharP :: P Char
identifierInitialCharP = satisfy isIdentifierChar

isIdentifierInitialChar :: Char -> Bool
isIdentifierInitialChar c =
  isAlpha c || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c =
  c `elem` ("&|%^*+-/~.=!" :: [Char])

operatorCharP :: P Char
operatorCharP = satisfy isOperatorChar

operatorP :: Text -> P ()
operatorP op = try $ do
  void $ chunk op
  notFollowedBy operatorCharP
  space

keywordP :: Text -> P ()
keywordP kw = try $ do
  void $ chunk kw
  notFollowedBy identifierCharP
  space

identifierNameP :: P Text
identifierNameP = do
  Text.cons <$> identifierInitialCharP
            <*> takeWhileP (Just "identifier char") isIdentifierChar
            <* space

identifierP :: P Identifier
identifierP = Identifier <$> identifierNameP

stringLitP :: P Text
stringLitP = do
  char '"' *> (Text.pack <$> many stringLitCharP) <* char '"' <* space

stringLitCharP :: P Char
stringLitCharP = escapedStringLitCharP <|> plainStringLitCharP

escapedStringLitCharP :: P Char
escapedStringLitCharP = do
  void $ char '\\'
  c <- satisfy (const True)
  case c of
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'v' -> pure '\v'
    'b' -> pure '\b'
    _ -> pure c

plainStringLitCharP :: P Char
plainStringLitCharP = satisfy (`notElem` ['\\', '"'])

exprP :: P Expr
exprP = booleanExprP

binaryExprP :: P BinaryOperator -> P Expr -> P Expr
binaryExprP opP subP = do
  lhs <- subP
  tailP lhs
  where
    tailP lhs = choice
      [ do
          op <- opP
          rhs <- subP
          tailP (BinaryE op lhs rhs)
      , pure lhs
      ]

booleanExprP :: P Expr
booleanExprP = binaryExprP booleanOpP comparativeExprP

booleanOpP :: P BinaryOperator
booleanOpP = choice
  [ BinopAnd <$ operatorP "&&"
  , BinopOr <$ operatorP "||"
  ]

comparativeExprP :: P Expr
comparativeExprP = binaryExprP comparativeOpP concatExprP

comparativeOpP :: P BinaryOperator
comparativeOpP = choice
  [ BinopEqual <$ operatorP "=="
  , BinopNotEqual <$ operatorP "!="
  , BinopGT <$ operatorP ">"
  , BinopGTE <$ operatorP ">="
  , BinopLT <$ operatorP "<"
  , BinopLTE <$ operatorP "<="
  , BinopIn <$ keywordP "in"
  ]

concatExprP :: P Expr
concatExprP = binaryExprP concatOpP additiveExprP

concatOpP :: P BinaryOperator
concatOpP = BinopConcat <$ operatorP "~"

additiveExprP :: P Expr
additiveExprP = binaryExprP additiveOpP multiplicativeExprP

additiveOpP :: P BinaryOperator
additiveOpP = choice
  [ BinopPlus <$ operatorP "+"
  , BinopMinus <$ operatorP "-"
  ]

multiplicativeExprP :: P Expr
multiplicativeExprP = binaryExprP multiplicativeOpP powerExprP

multiplicativeOpP :: P BinaryOperator
multiplicativeOpP = choice
  [ BinopIntDiv <$ operatorP "//"
  , BinopDiv <$ operatorP "/"
  , BinopMod <$ operatorP "%"
  , BinopMul <$ operatorP "*"
  ]

powerExprP :: P Expr
powerExprP = binaryExprP powerOpP memberAccessExprP

powerOpP :: P BinaryOperator
powerOpP = BinopPower <$ operatorP "**"

memberAccessExprP :: P Expr
memberAccessExprP = do
  lhs <- simpleExprP
  tailP lhs
  where
    tailP lhs = dotTailP lhs
              <|> bracketsTailP lhs
              <|> callTailP lhs
              <|> pure lhs

    dotTailP lhs = do
      operatorP "."
      selector <- StringLitE <$> identifierNameP
      tailP (BinaryE BinopIndex lhs selector)

    bracketsTailP lhs = do
      selector <- bracketed exprP
      tailP (BinaryE BinopIndex lhs selector)

    callTailP lhs = do
      (posArgs, kwArgs) <- callArgsP
      tailP (CallE lhs posArgs kwArgs)

callArgsP :: P ([Expr], [(Identifier, Expr)])
callArgsP = do
  args <- parenthesized $ argPairP `sepBy` commaP
  let posArgs = [ e | (Nothing, e) <- args ]
      kwArgs = [ (k, e) | (Just k, e) <- args ]
  pure (posArgs, kwArgs)

argPairP :: P (Maybe Identifier, Expr)
argPairP = try kwArgPairP <|> ((Nothing ,) <$> exprP)

kwArgPairP :: P (Maybe Identifier, Expr)
kwArgPairP =
  (,) <$> (Just <$> identifierP) <*> (equalsP *> exprP)
  
simpleExprP :: P Expr
simpleExprP = choice
  [ parenthesized exprP
  , StringLitE <$> stringLitP
  , BoolE True <$ (keywordP "true" <|> keywordP "True")
  , BoolE False <$ (keywordP "false" <|> keywordP "False")
  , NoneE <$ (keywordP "none" <|> keywordP "None")
  , VarE <$> identifierP
  ]

equalsP :: P ()
equalsP = char '=' *> space

commaP :: P ()
commaP = char ',' *> space

parenthesized :: P a -> P a
parenthesized = betweenT "(" ")"

bracketed :: P a -> P a
bracketed = betweenT "[" "]"

braced :: P a -> P a
braced = between "{" "}"

betweenT :: Text -> Text -> P c -> P c
betweenT o c =
  between (chunk o *> space) (chunk c *> space)

binopP :: P BinaryOperator
binopP = choice
  [ BinopPlus <$ operatorP "+"
  , BinopMinus <$ operatorP "-"
  , BinopDiv <$ operatorP "/"
  , BinopIntDiv <$ operatorP "//"
  , BinopMod <$ operatorP "%"
  , BinopMul <$ operatorP "*"
  , BinopPower <$ operatorP "**"
  , BinopEqual <$ operatorP "=="
  , BinopNotEqual <$ operatorP "!="
  , BinopGT <$ operatorP ">"
  , BinopGTE <$ operatorP ">="
  , BinopLT <$ operatorP "<"
  , BinopLTE <$ operatorP "<="
  , BinopAnd <$ operatorP "&&"
  , BinopOr <$ operatorP "||"
  , BinopIn <$ keywordP "in"
  , BinopConcat <$ operatorP "~"
  ]
