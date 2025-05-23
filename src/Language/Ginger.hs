{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Ginger
( Statement (..)
, Expr (..)
, Value (..)
, Template (..)
, ginger
, Context (..)
, defContext
, Encoder
, TemplateLoader
, Env (..)
, emptyEnv
, defEnv
, Eval (..)
, Encoded (..)
, RuntimeError (..)
, prettyRuntimeError
, Identifier (..)
, JinjaDialect (..)
, module M
)
where

import Language.Ginger.AST
import Language.Ginger.Value
import Language.Ginger.Interpret
import Language.Ginger.RuntimeError (prettyRuntimeError)
import Language.Ginger.FileLoader as M
        ( fileLoader
        )
import Language.Ginger.Parse as M
        ( POptions (..)
        , defPOptions
        , BlockTrimming (..)
        , BlockStripping (..)
        )
import qualified Language.Ginger.Parse as P
import Language.Ginger.Interpret.DefEnv as M
        ( htmlEncoder
        , defVars
        , defVarsCompat
        )

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Except (runExceptT, throwError)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)

data JinjaDialect
  = DialectGinger2
  | DialectJinja2
  deriving (Show, Eq, Ord, Enum, Bounded)

ginger :: forall m. Monad m
       => TemplateLoader m
       -> POptions
       -> JinjaDialect
       -> Encoder m
       -> Text
       -> Map Identifier (Value m)
       -> m (Either RuntimeError Encoded)
ginger loader parserOptions dialect encoder templateName vars = runExceptT $ do
  templateSrc <- maybe
                  (throwError $ TemplateFileNotFoundError templateName)
                  pure
                  =<< lift (loader templateName)
  let parseResult = P.parseGingerWith
                      parserOptions
                      P.template
                      (Text.unpack templateName)
                      templateSrc
  template <- either
                (throwError . TemplateParseError templateName . Text.pack)
                pure
                parseResult
  let ctx = defContext
              { contextEncode = encoder
              , contextLoadTemplateFile = loader
              }
      defVars' = case dialect of
                    DialectGinger2 -> defVars
                    DialectJinja2 -> defVarsCompat
      env = defEnv
              { envVars = defVars' <> vars
              }
  eitherExceptM $ runGingerT
    (evalT template >>= encode)
    ctx
    env
