{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Ginger.Parse
( P
, expr
, statement
, template
, parseGinger
, parseGingerFile
)
where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Ginger.AST

type P = Parsec Void Text

--------------------------------------------------------------------------------
-- Running Parsers
--------------------------------------------------------------------------------

parseGinger :: P a -> Text -> Either String a
parseGinger p input =
  mapLeft errorBundlePretty $ parse p "<input>" input

parseGingerFile :: P a -> FilePath -> IO (Either String a)
parseGingerFile p filename =
  mapLeft errorBundlePretty . parse p filename <$> Text.readFile filename

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

--------------------------------------------------------------------------------
-- Primitives etc.
--------------------------------------------------------------------------------

identifierChar :: P Char
identifierChar = satisfy isIdentifierChar

isIdentifierChar :: Char -> Bool
isIdentifierChar c =
  isAlphaNum c || c == '_'

identifierInitialChar :: P Char
identifierInitialChar = satisfy isIdentifierInitialChar

isIdentifierInitialChar :: Char -> Bool
isIdentifierInitialChar c =
  isAlpha c || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c =
  c `elem` ("&|%^*+-/~.=![]{}()," :: [Char])

operatorChar :: P Char
operatorChar = satisfy isOperatorChar

operator :: Text -> P ()
operator op = try $ do
  void $ chunk op
  notFollowedBy operatorChar
  space

keyword :: Text -> P ()
keyword kw = try $ do
  void $ chunk kw
  notFollowedBy identifierChar
  space

identifierRaw :: P Text
identifierRaw = do
  Text.cons <$> identifierInitialChar
            <*> takeWhileP (Just "identifier char") isIdentifierChar
            <* space

identifier :: P Identifier
identifier = Identifier <$> identifierRaw

stringLit :: P Text
stringLit =
  choice
    [ char q *> (Text.pack <$> many (stringLitChar q)) <* char q <* space
    | q <- ['"', '\'']
    ]

stringLitChar :: Char -> P Char
stringLitChar q = escapedStringLitChar <|> plainStringLitChar q

escapedStringLitChar :: P Char
escapedStringLitChar = do
  void $ char '\\'
  c <- satisfy (const True)
  case c of
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'v' -> pure '\v'
    'b' -> pure '\b'
    _ -> pure c

plainStringLitChar :: Char -> P Char
plainStringLitChar q = satisfy (`notElem` ['\\', q])

intLit :: P Integer
intLit = do
  read <$> intDigits

intDigits :: P String
intDigits = do
  sign <- option "" $ "-" <$ char '-'
  x <- satisfy (\c -> isDigit c && c /= '0')
  xs <- many digit
  space
  pure (sign ++ (x : xs))

floatLit :: P Double
floatLit = do
  m <- do
    sign <- option "" $ "-" <$ char '-'
    intPart <- many digit
    void $ char '.'
    fracPart <- many digit
    when (null intPart && null fracPart)
      (unexpected $ Label $ 'd' :| "ot-only float")
    pure $ sign ++
           (if null intPart then "0" else intPart) ++
           "." ++
           (if null fracPart then "0" else fracPart)
  e <- option "" $ do
    void $ char 'E' <|> char 'e'
    ('e' :) <$> intDigits
  pure . read $ m ++ e


digit :: P Char
digit = satisfy isDigit

equals :: P ()
equals = char '=' *> space

comma :: P ()
comma = char ',' *> space

colon :: P ()
colon = char ':' *> space

parenthesized :: P a -> P a
parenthesized = betweenT "(" ")"

bracketed :: P a -> P a
bracketed = betweenT "[" "]"

braced :: P a -> P a
braced = betweenT "{" "}"

betweenT :: Text -> Text -> P c -> P c
betweenT o c =
  between (chunk o *> space) (chunk c *> space)

--------------------------------------------------------------------------------
-- Argument lists
--------------------------------------------------------------------------------

callArgs :: P ([Expr], [(Identifier, Expr)])
callArgs = do
  args <- parenthesized $ argPair `sepBy` comma
  let posArgs = [ e | (Nothing, e) <- args ]
      kwArgs = [ (k, e) | (Just k, e) <- args ]
  pure (posArgs, kwArgs)

argPair :: P (Maybe Identifier, Expr)
argPair = try kwArgPair <|> ((Nothing ,) <$> expr)

kwArgPair :: P (Maybe Identifier, Expr)
kwArgPair =
  (,) <$> (Just <$> identifier) <*> (equals *> expr)

argsSig :: P [(Identifier, Maybe Expr)]
argsSig = parenthesized $ argSig `sepBy` comma

argSig :: P (Identifier, Maybe Expr)
argSig =
  (,) <$> identifier
      <*> optional (operator "=" *> expr)
  

--------------------------------------------------------------------------------
-- Expression parsers
--------------------------------------------------------------------------------

expr :: P Expr
expr = ternaryExpr

ternaryExpr :: P Expr
ternaryExpr = do
  lhs <- booleanExpr
  option lhs $ exprTail lhs
  where
    exprTail lhs = try $ do
      keyword "if"
      cond <- booleanExpr
      keyword "else"
      rhs <- ternaryExpr
      pure $ TernaryE cond lhs rhs

binaryExpr :: P BinaryOperator -> P Expr -> P Expr
binaryExpr op sub = do
  lhs <- sub
  exprTail lhs
  where
    exprTail lhs = choice
      [ do
          o <- op
          rhs <- sub
          exprTail (BinaryE o lhs rhs)
      , pure lhs
      ]

booleanExpr :: P Expr
booleanExpr = binaryExpr booleanOp unaryNotExpr

booleanOp :: P BinaryOperator
booleanOp = choice
  [ BinopAnd <$ keyword "and"
  , BinopOr <$ keyword "or"
  ]

unaryNotExpr :: P Expr
unaryNotExpr =
  (keyword "not" *> (NotE <$> unaryNotExpr))
  <|>
  comparativeExpr

comparativeExpr :: P Expr
comparativeExpr = binaryExpr comparativeOp testExpr

comparativeOp :: P BinaryOperator
comparativeOp = choice
  [ BinopEqual <$ operator "=="
  , BinopNotEqual <$ operator "!="
  , BinopGT <$ operator ">"
  , BinopGTE <$ operator ">="
  , BinopLT <$ operator "<"
  , BinopLTE <$ operator "<="
  , BinopIn <$ keyword "in"
  ]

testExpr :: P Expr
testExpr = do
  lhs <- sub
  option lhs $ exprTail lhs
  where
    sub = concatExpr

    exprTail lhs = do
      keyword "is"
      wrapper <- option id $ NotE <$ keyword "not"
      test <- VarE <$> identifier
      (posArgs, kwArgs) <- option ([], []) (callArgs <|> soloArg)
      pure . wrapper $ IsE lhs test posArgs kwArgs

soloArg :: P ([Expr], [(Identifier, Expr)])
soloArg = do
  arg <- expr
  pure ([arg], [])


concatExpr :: P Expr
concatExpr = binaryExpr concatOp additiveExpr

concatOp :: P BinaryOperator
concatOp = BinopConcat <$ operator "~"

additiveExpr :: P Expr
additiveExpr = binaryExpr additiveOp multiplicativeExpr

additiveOp :: P BinaryOperator
additiveOp = choice
  [ BinopPlus <$ operator "+"
  , BinopMinus <$ operator "-"
  ]

multiplicativeExpr :: P Expr
multiplicativeExpr = binaryExpr multiplicativeOp powerExpr

multiplicativeOp :: P BinaryOperator
multiplicativeOp = choice
  [ BinopIntDiv <$ operator "//"
  , BinopDiv <$ operator "/"
  , BinopMod <$ operator "%"
  , BinopMul <$ operator "*"
  ]

powerExpr :: P Expr
powerExpr = binaryExpr powerOp memberAccessExpr

powerOp :: P BinaryOperator
powerOp = BinopPower <$ operator "**"

memberAccessExpr :: P Expr
memberAccessExpr = do
  lhs <- simpleExpr
  exprTail lhs
  where
    exprTail lhs = dotTail lhs
              <|> bracketsTail lhs
              <|> callTail lhs
              <|> filterTail lhs
              <|> pure lhs

    dotTail lhs = do
      operator "."
      selector <- StringLitE <$> identifierRaw
      exprTail (IndexE lhs selector)

    bracketsTail lhs = do
      selector <- bracketed expr
      exprTail (IndexE lhs selector)

    callTail lhs = do
      (posArgs, kwArgs) <- callArgs
      exprTail (CallE lhs posArgs kwArgs)

    filterTail lhs = do
      operator "|"
      callable <- simpleExpr
      (posArgs, kwArgs) <- option ([], []) $ callArgs
      exprTail (FilterE lhs callable posArgs kwArgs)

list :: P [Expr]
list = bracketed (expr `sepBy` comma)

dict :: P [(Expr, Expr)]
dict = braced (dictPair `sepBy` comma)

dictPair :: P (Expr, Expr)
dictPair =
  (,) <$> expr <*> (colon *> expr)

simpleExpr :: P Expr
simpleExpr = choice
  [ parenthesized expr
  , StringLitE <$> stringLit
  , ListE <$> list
  , DictE <$> dict
  , FloatLitE <$> try floatLit
  , IntLitE <$> try intLit
  , operator "-" *> (NegateE <$> (parenthesized expr <|> (VarE <$> identifier)))
  , BoolE True <$ (keyword "true" <|> keyword "True")
  , BoolE False <$ (keyword "false" <|> keyword "False")
  , NoneE <$ (keyword "none" <|> keyword "None")
  , VarE <$> identifier
  ]

-------------------------------------------------------------------------------- 
-- Statement-level tokens
--------------------------------------------------------------------------------

openComment :: P ()
openComment = operator "{#"

closeComment :: P ()
closeComment = void $ chunk "#}"

openInterpolation :: P ()
openInterpolation = operator "{{"

closeInterpolation :: P ()
closeInterpolation = void $ chunk "}}"

openFlow :: P ()
openFlow = operator "{%"

closeFlow :: P ()
closeFlow = void $ chunk "%}"


-------------------------------------------------------------------------------- 
-- Statement parsers
--------------------------------------------------------------------------------

template :: P Statement
template = statement <* eof

statement :: P Statement
statement =
  wrap <$> many singleStatement
  where
    wrap [] = ImmediateS mempty
    wrap [x] = x
    wrap xs = GroupS xs

singleStatement :: P Statement
singleStatement =
  choice
    [ commentStatement
    , interpolationStatement
    , controlStatement
    , immediateStatement
    ]

immediateStatement :: P Statement
immediateStatement =
  ImmediateS . Encoded . mconcat <$> 
    some
    ( takeWhile1P Nothing (/= '{')
      <|>
      (notFollowedBy (openComment <|> openInterpolation <|> openFlow) >> chunk "{")
    )

commentStatement :: P Statement
commentStatement =
  between openComment closeComment $
    CommentS . Text.strip . Text.pack <$> many (notFollowedBy closeComment *> anySingle)

interpolationStatement :: P Statement
interpolationStatement =
  between openInterpolation closeInterpolation $
    InterpolationS <$> expr

controlStatement :: P Statement
controlStatement =
  choice
    [ ifStatement
    , forStatement
    , macroStatement
    , callStatement
    , filterStatement
    , setStatement
    , setBlockStatement
    , includeStatement
    , importStatement
    , extendsStatement
    , blockStatement
    , withStatement
    ]

flow :: Text -> P a -> P a
flow kw inner = do
  try (openFlow >> keyword kw)
  inner <* closeFlow

flow_ :: Text -> P ()
flow_ kw = flow kw nop

withFlow :: Text -> P a -> P b -> (a -> b -> c) -> P c
withFlow kw header body combine =
  combine <$> flow kw header <*> body <* flow_ ("end" <> kw)

nop :: P ()
nop = pure ()

ifStatement :: P Statement
ifStatement = do
  withFlow "if" expr body makeIf
  where
    body = do
      yes <- statement
      noMay <- optional $ flow_ "else" *> statement
      pure (yes, noMay)
    makeIf cond (yes, noMay) = IfS cond yes noMay

forStatement :: P Statement
forStatement = do
  withFlow "for" forHeader forBody makeFor
  where
    forHeader = do
      (keyMay, val) <- do
        a' <- identifier
        bMay' <- optional $ comma *> identifier
        pure $ case (a', bMay') of
          (a, Just b) -> (Just a, b)
          (a, Nothing) -> (Nothing, a)
      keyword "in"
      iteree <- expr
      filterMay <- optional $ keyword "if" *> expr
      recursivity <- option NotRecursive $ Recursive <$ keyword "recursive"
      pure (keyMay, val, iteree, filterMay, recursivity)
    forBody = do
      body <- statement
      elseMay <- optional $ flow_ "else" *> statement
      pure (body, elseMay)
    makeFor (keyMay, val, iteree, filterMay, recursivity) (body, elseMay) =
      ForS keyMay val iteree filterMay recursivity body elseMay

macroStatement :: P Statement
macroStatement = do
  withFlow "macro" macroHeader macroBody makeMacro
  where
    macroHeader :: P (Identifier, [MacroArg])
    macroHeader =
      (,) <$> identifier <*> option [] argsSig
    macroBody =
      statement
    makeMacro :: (Identifier, [MacroArg]) -> Statement -> Statement
    makeMacro (name, args) body = MacroS name args body

callStatement :: P Statement
callStatement = do
  withFlow "call" callHeader callBody makeCall
  where
    callHeader = do
      (,) <$> identifier <*> callArgs
    callBody = statement
    makeCall (callee, (args, kwargs)) body = CallS callee args kwargs body

filterStatement :: P Statement
filterStatement = do
  withFlow "filter" filterHeader filterBody makeFilter
  where
    filterHeader = do
      (,) <$> identifier <*> callArgs
    filterBody = statement
    makeFilter (filteree, (args, kwargs)) body = FilterS filteree args kwargs body

setPair :: P (Identifier, Expr)
setPair = (,) <$> identifier <* operator "=" <*> expr

setStatement :: P Statement
setStatement = try $ do
  flow "set" $ uncurry SetS <$> setPair

setBlockStatement :: P Statement
setBlockStatement = do
  withFlow "set" setBlockHeader setBlockBody makeSetBlock
  where
    setBlockHeader = (,) <$> identifier <*> optional (keyword "|" *> expr)
    setBlockBody = statement
    makeSetBlock (name, filterMay) body =
      SetBlockS name body filterMay

includeStatement :: P Statement
includeStatement =
  flow "include" $
    IncludeS
      <$> expr
      <*> option RequireMissing (IgnoreMissing <$ keyword "ignore" <* keyword "missing")
      <*> option WithContext (choice
            [ WithContext <$ keyword "with" <* keyword "context"
            , WithoutContext <$ keyword "without" <* keyword "context"
            ]
          )

importStatement :: P Statement
importStatement =
  wildcardImportStatement <|> explicitImportStatement

wildcardImportStatement :: P Statement
wildcardImportStatement =
  flow "import" $
    ImportS
      <$> expr
      <*> optional (keyword "as" *> identifier)
      <*> pure []
      <*> option RequireMissing (IgnoreMissing <$ keyword "ignore" <* keyword "missing")
      <*> option WithoutContext (choice
            [ WithContext <$ keyword "with" <* keyword "context"
            , WithoutContext <$ keyword "without" <* keyword "context"
            ]
          )

explicitImportStatement :: P Statement
explicitImportStatement =
  flow "from" $
    ImportS
      <$> expr
      <*> optional (keyword "as" *> identifier)
      <*  keyword "import"
      <*> (do
            notFollowedBy (choice $ map keyword ["ignore", "with", "without"])
            importPair `sepBy` comma
          )
      <*> option RequireMissing (IgnoreMissing <$ keyword "ignore" <* keyword "missing")
      <*> option WithoutContext (choice
            [ WithContext <$ keyword "with" <* keyword "context"
            , WithoutContext <$ keyword "without" <* keyword "context"
            ]
          )

importPair :: P (Identifier, Maybe Identifier)
importPair = (,) <$> identifier <*> optional (keyword "as" *> identifier)

extendsStatement :: P Statement
extendsStatement = flow "extends" $ ExtendsS <$> expr

withStatement :: P Statement
withStatement = do
  withFlow "with" withHeader withBody makeWith
  where
    withHeader = setPair `sepBy` comma
    withBody = statement
    makeWith = WithS

blockStatement :: P Statement
blockStatement = do
  (name, scopedness, requiredness) <- flow "block" $
    (,,) <$> identifier
         <*> (option NotScoped $ Scoped <$ keyword "scoped")
         <*> (option Optional $ Required <$ keyword "required")
  body <- statement
  void $ flow "endblock" (optional $ keyword (identifierName name))
  pure $ BlockS name body scopedness requiredness
