{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Ginger.Render
where

import Language.Ginger.AST

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intersperse)
import Data.Bool (bool)
import Data.Char (isControl, ord)
import Text.Printf (printf)

class RenderSyntax a where
  renderSyntax :: a -> Builder

renderStringLit :: Text -> Builder
renderStringLit str =
  "\"" <> mconcat (map renderStringLitChar $ Text.unpack str) <> "\""

renderStringLitChar :: Char -> Builder
renderStringLitChar '\0' = "\\0"
renderStringLitChar '\a' = "\\a"
renderStringLitChar '\b' = "\\b"
renderStringLitChar '\f' = "\\f"
renderStringLitChar '\n' = "\\n"
renderStringLitChar '\r' = "\\r"
renderStringLitChar '\t' = "\\t"
renderStringLitChar '\v' = "\\v"
renderStringLitChar '"' = "\\\""
renderStringLitChar '\'' = "\\'"
renderStringLitChar '\\' = "\\\\"
renderStringLitChar c
  | isControl c || ord c >= 128
  = renderUnicodeEscape c
  | otherwise = Builder.fromString [c]

renderUnicodeEscape :: Char -> Builder
renderUnicodeEscape c =
  let o = ord c
      format = if o <= 0xFF then
                  "\\x%02x"
               else if o <= 0xFFFF then
                  "\\u%04x"
               else
                  "\\U%08x"
  in
    Builder.fromString $ printf format o

instance RenderSyntax Expr where
  renderSyntax NoneE = "none"
  renderSyntax TrueE = "true"
  renderSyntax FalseE = "false"
  renderSyntax (StatementE (InterpolationS e)) =
    renderSyntax e
  renderSyntax (StatementE (IfS cond yes noMay)) =
    renderSyntax (TernaryE cond (StatementE yes) (maybe NoneE StatementE noMay))
  renderSyntax (StatementE (ImmediateS (Encoded txt))) =
    renderSyntax (FilterE (StringLitE txt) (VarE "encode") [] [])
  renderSyntax (StatementE x) =
    error $ "Cannot render statement expression " ++ (LText.unpack . Builder.toLazyText $ renderSyntax x)
  renderSyntax (StringLitE str) = renderStringLit str
  renderSyntax (IntLitE i) = Builder.fromText $ Text.show i
  renderSyntax (FloatLitE f) = Builder.fromText $ Text.show f
  renderSyntax (ListE xs) =
    "[" <> (mconcat . intersperse ", " . map renderSyntax $ xs) <> "]"
  renderSyntax (DictE xs) =
    "{" <> (mconcat . intersperse ", " $ [ renderSyntax k <> ": " <> renderSyntax v | (k, v) <- xs ]) <> "}"
  renderSyntax (NotE expr) =
    "(not " <> renderSyntax expr <> ")"
  renderSyntax (NegateE expr) =
    "-" <> renderSyntax expr
  renderSyntax (IndexE a b) =
    renderSyntax a <> "[" <> renderSyntax b <> "]"
  renderSyntax (DotE (VarE a) (StringLitE s)) =
    renderSyntax a <> "." <> Builder.fromText s
  renderSyntax (DotE a (StringLitE s)) =
    "(" <> renderSyntax a <> ")." <> Builder.fromText s
  renderSyntax (BinaryE op a b) =
    "(" <> renderSyntax a <> renderSyntax op <> renderSyntax b <> ")"
  renderSyntax (IsE scrutinee test args kwargs) =
    "(" <> renderSyntax scrutinee <> " is " <> renderSyntax test <> renderArgs args kwargs <> ")"
  renderSyntax (CallE callee args kwargs) =
    "(" <> renderSyntax callee <> renderArgs args kwargs <> ")"
  renderSyntax (FilterE arg0 f args kwargs) =
    "(" <> renderSyntax arg0 <> "|" <> renderSyntax f <> renderArgs args kwargs <> ")"
  renderSyntax (TernaryE c y n) =
    "(" <> renderSyntax y <> " if " <> renderSyntax c <> " else " <> renderSyntax n <> ")"
  renderSyntax (VarE ident) =
    renderSyntax ident
  renderSyntax BoolE {} = "BoolE ???"
  renderSyntax UnaryE {} = "UnaryE ???"

instance RenderSyntax Identifier where
  renderSyntax (Identifier i) = Builder.fromText i

instance RenderSyntax IncludeMissingPolicy where
  renderSyntax RequireMissing = "require missing"
  renderSyntax IgnoreMissing = "ignore missing"

instance RenderSyntax IncludeContextPolicy where
  renderSyntax WithContext = "with context"
  renderSyntax WithoutContext = "without context"

instance RenderSyntax Scoped where
  renderSyntax Scoped = "scoped"
  renderSyntax NotScoped = ""

instance RenderSyntax Required where
  renderSyntax Required = "required"
  renderSyntax Optional = ""

instance RenderSyntax BinaryOperator where
  renderSyntax BinopPlus = " + "
  renderSyntax BinopMinus = " - "
  renderSyntax BinopDiv = " / "
  renderSyntax BinopIntDiv = " // "
  renderSyntax BinopMod = " % "
  renderSyntax BinopMul = " * "
  renderSyntax BinopPower = " ** "
  renderSyntax BinopEqual = " == "
  renderSyntax BinopNotEqual = " != "
  renderSyntax BinopGT = " > "
  renderSyntax BinopGTE = " >= "
  renderSyntax BinopLT = " < "
  renderSyntax BinopLTE = " <= "
  renderSyntax BinopAnd = " and "
  renderSyntax BinopOr = " or "
  renderSyntax BinopIn = " in "
  renderSyntax BinopIndex = " [] "
  renderSyntax BinopDot = "."
  renderSyntax BinopConcat = " ~ "

renderFlow :: Builder -> Builder
renderFlow inner = "{% " <> inner <> " %}\n"

-- | Most 'Encoded's can actually be converted as-is, but if there are any
-- curly braces, we need to handle them specially.
renderEncoded :: Encoded -> Builder
renderEncoded (Encoded txt) =
  Builder.fromText .
  Text.replace "{{" "{{'{{'}}" $
  Text.replace "{%" "{{'{%'}}" $
  Text.replace "{#" "{{'{#'}}" $
  txt

instance RenderSyntax Statement where
  renderSyntax (ImmediateS e) = renderEncoded e
  renderSyntax (InterpolationS e) = "{{ " <> renderSyntax e <> " }}"
  renderSyntax (CommentS msg) = "{# " <> Builder.fromText msg <> " #}"
  renderSyntax (ForS kMay v iteree condMay recursivity body elseBranchMay) =
    renderFlow (
      "for " <>
      maybe "" (\k -> renderSyntax k <> ", ") kMay <>
      renderSyntax v <>
      " in " <>
      renderSyntax iteree <>
      maybe "" (\cond -> " if " <> renderSyntax cond) condMay <>
      bool "" " recursive" (is recursivity)
    ) <>
    renderSyntax body <>
    maybe "" (\elseBranch -> renderFlow "else" <> renderSyntax elseBranch) elseBranchMay <>
    renderFlow "endfor"
  renderSyntax (IfS cond yes noMay) =
    renderFlow (
      "if " <>
      renderSyntax cond
    ) <>
    renderSyntax yes <>
    maybe "" (\no -> renderFlow "else" <> renderSyntax no) noMay <>
    renderFlow "endif"
  renderSyntax (MacroS name args body) =
    renderFlow (
      "macro " <>
      renderSyntax name <>
      renderArgSpec args
    ) <>
    renderSyntax body <>
    renderFlow "endmacro"
  renderSyntax (CallS name args kwargs body) =
    renderFlow (
      "call " <>
      renderSyntax name <>
      renderArgs args kwargs
    ) <>
    renderSyntax body <>
    renderFlow "endcall"
  renderSyntax (FilterS name args kwargs body) =
    renderFlow (
      "filter " <>
      renderSyntax name <>
      renderArgs args kwargs
    ) <>
    renderSyntax body <>
    renderFlow "endfilter"
  renderSyntax (SetS name val) =
    renderFlow (
      "set " <>
      renderSyntax name <>
      " = " <>
      renderSyntax val
    )
  renderSyntax (SetBlockS name body filterMay) =
    renderFlow (
      "set " <>
      renderSyntax name <>
      maybe "" (\f -> "|" <> renderSyntax f) filterMay
    ) <>
    renderSyntax body <>
    renderFlow "endset"
  renderSyntax (IncludeS includee missingPolicy contextPolicy) =
    renderFlow (
      "include " <>
      renderSyntax includee <>
      " " <>
      renderSyntax missingPolicy <>
      " " <>
      renderSyntax contextPolicy
    )
  renderSyntax (ImportS importee aliasMay Nothing missingPolicy contextPolicy) =
    renderFlow (
      "import " <>
      renderSyntax importee <>
      maybe "" (\alias -> " as " <> renderSyntax alias) aliasMay <>
      renderSyntax missingPolicy <>
      " " <>
      renderSyntax contextPolicy
    )
  renderSyntax (ImportS importee aliasMay (Just imports) missingPolicy contextPolicy) =
    renderFlow (
      "from " <>
      renderSyntax importee <>
      maybe " " (\alias -> " as " <> renderSyntax alias) aliasMay <>
      "import " <>
      renderImports imports <>
      " " <>
      renderSyntax missingPolicy <>
      " " <>
      renderSyntax contextPolicy
    )
  renderSyntax (ExtendsS e) =
    renderFlow (
      "extends " <>
      renderSyntax e
    )
  renderSyntax (BlockS name (Block body scopedness requiredness)) =
    renderFlow (
      "block " <>
      renderSyntax name <>
      renderSyntax scopedness <>
      renderSyntax requiredness
    ) <>
    renderSyntax body <>
    renderFlow "endblock"
  renderSyntax (WithS defs body) =
    renderFlow (
      "with " <>
      renderWithDefs defs
    ) <>
    renderSyntax body <>
    renderFlow "endwith"
  renderSyntax (GroupS ss) =
    mconcat . map renderSyntax $ ss

renderImports :: [(Identifier, Maybe Identifier)] -> Builder
renderImports = mconcat . intersperse ", " . map renderImport

renderImport :: (Identifier, Maybe Identifier) -> Builder
renderImport (i, Nothing) = renderSyntax i
renderImport (i, Just a) = renderSyntax i <> " as " <> renderSyntax a

renderWithDefs :: [(Identifier, Expr)] -> Builder
renderWithDefs args =
  mconcat . intersperse ", " $
    [ renderSyntax k <> "=" <> renderSyntax v | (k, v) <- args ]

renderArgs :: [Expr] -> [(Identifier, Expr)] -> Builder
renderArgs args kwargs =
  let allArgs = map (Nothing ,) args ++ [(Just k, v) | (k, v) <- kwargs]
  in
    "(" <>
    (mconcat . intersperse ", " $
        [ maybe "" (\k -> renderSyntax k <> "=") kMay <> renderSyntax v
        | (kMay, v) <- allArgs
        ]
    ) <>
    ")"

renderArgSpec :: [(Identifier, Maybe Expr)] -> Builder
renderArgSpec args =
  "(" <>
  (mconcat . intersperse ", " $
      [ renderSyntax k <> maybe "" (\v -> "=" <> renderSyntax v) vMay
      | (k, vMay) <- args
      ]
  ) <>
  ")"
