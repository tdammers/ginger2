{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

module Language.Ginger.Parse
( P
, expr
, statement
, template
, parseGinger
, parseGingerFile
, parseGingerWith
, parseGingerFileWith
, POptions (..)
, defPOptions
, BlockTrimming (..)
, BlockStripping (..)
, simplifyS
)
where

import Control.Monad (void, when, replicateM)
import Control.Monad.Reader (Reader, runReader, ask, asks)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace, chr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char

import Language.Ginger.AST as AST
import Language.Ginger.SourcePosition as AST

--------------------------------------------------------------------------------
-- Parser Type
--------------------------------------------------------------------------------

type P = ParsecT Void Text (Reader POptions)

data BlockTrimming
  = NoTrimBlocks
  | TrimBlocks
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data BlockStripping
  = NoStripBlocks
  | StripBlocks
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PolicyOverride
  = Default
  | Never
  | Always
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

class OverridablePolicy a where
  override :: PolicyOverride -> a -> a

instance OverridablePolicy BlockTrimming where
  override Default a = a
  override Never _ = NoTrimBlocks
  override Always _ = TrimBlocks

instance OverridablePolicy BlockStripping where
  override Default a = a
  override Never _ = NoStripBlocks
  override Always _ = StripBlocks

data POptions =
  POptions
    { pstateTrimBlocks :: !BlockTrimming
    , pstateStripBlocks :: !BlockStripping
    }
  deriving (Show, Read, Eq)

instance OverridablePolicy POptions where
  override o (POptions tr st) = POptions (override o tr) (override o st)


defPOptions :: POptions
defPOptions = POptions
  { pstateTrimBlocks = TrimBlocks
  , pstateStripBlocks = NoStripBlocks
  }

--------------------------------------------------------------------------------
-- Running Parsers
--------------------------------------------------------------------------------

parseGinger :: P a -> FilePath -> Text -> Either String a
parseGinger = parseGingerWith defPOptions

parseGingerWith :: POptions -> P a -> FilePath -> Text -> Either String a
parseGingerWith options p filename input =
  mapLeft errorBundlePretty $ runReader (runParserT p filename input) options

parseGingerFile :: P a -> FilePath -> IO (Either String a)
parseGingerFile = parseGingerFileWith defPOptions

parseGingerFileWith :: POptions -> P a -> FilePath -> IO (Either String a)
parseGingerFileWith options p filename = do
  input <- Text.readFile filename
  return $ mapLeft errorBundlePretty $ runReader (runParserT p filename input) options

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

--------------------------------------------------------------------------------
-- Primitives etc.
--------------------------------------------------------------------------------

positioned :: (SourcePosition -> a -> b) -> P a -> P b
positioned setPos inner = do
  pos <- convertSourcePos <$> getSourcePos
  setPos pos <$> inner

convertSourcePos :: SourcePos -> SourcePosition
convertSourcePos sp =
  SourcePosition
    { AST.sourceFile = Text.pack $ Megaparsec.sourceName sp
    , AST.sourceLine = unPos $ Megaparsec.sourceLine sp
    , AST.sourceColumn = unPos $ Megaparsec.sourceColumn sp
    }

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
  c `elem` ("&|%^*+-/~.=!,(){}[]" :: [Char])

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
    [ char q *> (Text.pack . catMaybes <$> many (stringLitChar q)) <* char q <* space
    | q <- ['"', '\'']
    ]

stringLitChar :: Char -> P (Maybe Char)
stringLitChar q =
  escapedStringLitChar <|> (Just <$> plainStringLitChar q)

escapedStringLitChar :: P (Maybe Char)
escapedStringLitChar = do
  void $ char '\\'
  c <- satisfy (const True)
  case c of
    '0' -> pure . Just $ '\0'
    'a' -> pure . Just $ '\a'
    'b' -> pure . Just $ '\b'
    'f' -> pure . Just $ '\f'
    'n' -> pure . Just $ '\n'
    'r' -> pure . Just $ '\r'
    't' -> pure . Just $ '\t'
    'v' -> pure . Just $ '\v'
    '"' -> pure . Just $ '"'
    '\'' -> pure . Just $ '\''
    '\\' -> pure . Just $ '\\'
    '&' -> pure Nothing
    'x' -> hexEscape 2
    'u' -> hexEscape 4
    'U' -> hexEscape 8
    t -> unexpected (Tokens $ t :| [])
  where
    hexEscape n = do
      ns <- replicateM n hexChar
      pure . Just . chr $ read ("0x" ++ ns)

hexChar :: P Char
hexChar = satisfy isHexChar

isHexChar :: Char -> Bool
isHexChar x =
  isDigit x ||
  (x >= 'a' && x <= 'z') || 
  (x >= 'A' && x <= 'Z')

plainStringLitChar :: Char -> P Char
plainStringLitChar q = satisfy (`notElem` ['\\', q])

intLit :: P Integer
intLit = do
  read <$> (intDigits <* space)

intDigits :: P String
intDigits = try $ do
  sign <- try $ (option "" $ "-" <$ char '-')
  str <- some digit
  pure (sign ++ str)

floatLit :: P Double
floatLit = do
  m <- do
    sign <- try $ (option "" $ "-" <$ char '-')
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
  space
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
      <*> optional (chunk "=" *> space *> expr)
  

--------------------------------------------------------------------------------
-- Expression parsers
--------------------------------------------------------------------------------

expr :: P Expr
expr = positioned PositionedE ternaryExpr

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
      chunk "." *> space
      selector <- identifier
      exprTail (DotE lhs selector)

    sliceCont lhs op1May = do
      try $ chunk ":" *> space
      op2May <- optional expr
      pure $ SliceE lhs op1May op2May

    bracketsTail lhs = do
      t <- bracketed $ do
        op1May <- optional expr
        case op1May of
          Nothing ->
            sliceCont lhs op1May
          Just op1 -> do
            choice
              [ sliceCont lhs op1May
              , pure $ IndexE lhs op1
              ]
      exprTail t

    callTail lhs = do
      (posArgs, kwArgs) <- callArgs
      exprTail (CallE lhs posArgs kwArgs)

    filterTail lhs = do
      chunk "|" *> space
      callable <- simpleExpr
      (posArgs, kwArgs) <- option ([], []) $ callArgs
      exprTail (FilterE lhs callable posArgs kwArgs)

list :: P (Vector Expr)
list = V.fromList <$> bracketed (expr `sepBy` comma)

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
  , try (operator "-") *> space *> (NegateE <$> (parenthesized expr <|> (VarE <$> identifier)))
  , BoolE True <$ (keyword "true" <|> keyword "True")
  , BoolE False <$ (keyword "false" <|> keyword "False")
  , NoneE <$ (keyword "none" <|> keyword "None")
  , VarE <$> identifier
  ]

-------------------------------------------------------------------------------- 
-- Statement-level tokens
--------------------------------------------------------------------------------

overrideToken :: P PolicyOverride
overrideToken =
  choice
    [ Always <$ chunk "-"
    , Never <$ chunk "+"
    , pure Default
    ]

openWithOverride :: Text -> P ()
openWithOverride base = do
  policy <- ask
  let defLeader = case pstateStripBlocks policy of
        StripBlocks -> inlineSpace
        NoStripBlocks -> pure ()
  void $ choice
        [ try $ inlineSpace *> chunk base *> chunk "-" <* notFollowedBy operatorChar
        , try $ chunk base *> chunk "+" <* notFollowedBy operatorChar
        , try $ chunk base <* notFollowedBy operatorChar
        , try $ defLeader *> chunk base <* notFollowedBy operatorChar
        ]
  space

inlineSpace :: P ()
inlineSpace =
  void $
    takeWhileP
      (Just "non-newline whitespace")
      (\c -> isSpace c && c `notElem` ['\r', '\n'])

anyNewline :: P Text
anyNewline =
  choice
    [ chunk "\r\n"
    , chunk "\n"
    , chunk "\r"
    ]

closeWithOverride :: Text -> P ()
closeWithOverride base = do
  ovr <- try $ overrideToken <* chunk base
  policy <- asks (override ovr)
  when (pstateTrimBlocks policy == TrimBlocks) $
    void . optional . try $ inlineSpace *> (void anyNewline <|> eof)

openComment :: P ()
openComment = openWithOverride "{#"

closeComment :: P ()
closeComment = closeWithOverride "#}"

openInterpolation :: P ()
openInterpolation = openWithOverride "{{"

closeInterpolation :: P ()
closeInterpolation = closeWithOverride "}}"

openFlow :: P ()
openFlow = openWithOverride "{%"

closeFlow :: P ()
closeFlow = closeWithOverride "%}"


-------------------------------------------------------------------------------- 
-- Statement parsers
--------------------------------------------------------------------------------

template :: P Template
template = do
  Template <$> extends <*> statement

extends :: P (Maybe Text)
extends =
  optional . flow "extends" $ do
    stringLit <* space

simplifyS :: Statement -> Statement
simplifyS = traverseS simplifyOne id
  where
    simplifyOne (GroupS xs) = wrap xs
    simplifyOne s = s

statement :: P Statement
statement =
  wrap <$> many singleStatement

joinImmediates :: [Statement] -> [Statement]
joinImmediates (ImmediateS a : ImmediateS b : xs) =
  joinImmediates (ImmediateS (a <> b) : xs)
joinImmediates (PositionedS pos (ImmediateS a) : PositionedS _ (ImmediateS b) : xs) =
  joinImmediates (PositionedS pos (ImmediateS (a <> b)) : xs)
joinImmediates (PositionedS pos (ImmediateS a) : ImmediateS b : xs) =
  joinImmediates (PositionedS pos (ImmediateS (a <> b)) : xs)
joinImmediates (x:xs) = x : joinImmediates xs
joinImmediates [] = []

wrap :: [Statement] -> Statement
wrap [] = ImmediateS mempty
wrap [x] = x
wrap xs = GroupS (joinImmediates xs)

singleStatement :: P Statement
singleStatement =
  positioned PositionedS $
    choice
      [ commentStatement
      , interpolationStatement
      , controlStatement
      , immediateStatement
      ]

immediateStatement :: P Statement
immediateStatement =
  ImmediateS . Encoded <$> 
    choice
      [ anyNewline
      , fmap mconcat . some $
          notFollowedBy (openComment <|> openInterpolation <|> openFlow) >> chunk "{"
          <|>
          takeWhile1P Nothing (`notElem` ['{', '\n', '\r'])
      ]

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
setPair = (,) <$> identifier <* chunk "=" <* space <*> expr

setTargetPair :: P (SetTarget, Expr)
setTargetPair = (,) <$> setTarget <* chunk "=" <* space <*> expr

setTarget :: P SetTarget
setTarget = do
  leader <- identifier
  selectorMay <- optional $ chunk "." *> identifier
  pure $ case selectorMay of
    Nothing -> SetVar leader
    Just selector -> SetMutable leader selector

setStatement :: P Statement
setStatement = try $ do
  flow "set" $ uncurry SetS <$> setTargetPair

setBlockStatement :: P Statement
setBlockStatement = do
  withFlow "set" setBlockHeader setBlockBody makeSetBlock
  where
    setBlockHeader = (,) <$> setTarget <*> optional (chunk "|" *> space *> expr)
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
      <*> pure Nothing
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
      <*> (Just <$> do
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
  pure $ BlockS name (Block body scopedness requiredness)
