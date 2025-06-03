{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger
( -- * Interpreting Templates
  ginger
, GingerT
, Eval (..)
, RuntimeError (..)
, Context (..)
, defContext
, Env (..)
, emptyEnv
, defEnv
, defVars
, defVarsCompat
  -- * AST
, Statement (..)
, Expr (..)
, Template (..)
, Block (..)
  -- * Representing Values
, Value (..)
, Scalar (..)
, Encoded (..)
, prettyRuntimeError
, Identifier (..)
  -- * Configuration
, Encoder
, htmlEncoder
, JinjaDialect (..)
  -- * Parser and Parser Options
, POptions (..)
, defPOptions
, BlockTrimming (..)
, BlockStripping (..)
  -- * Template Loaders
, TemplateLoader
, fileLoader
)
where

import Language.Ginger.AST
import Language.Ginger.BuiltinsAutodoc
import Language.Ginger.Value
import Language.Ginger.Interpret
import Language.Ginger.RuntimeError (prettyRuntimeError)
import Language.Ginger.FileLoader
        ( fileLoader
        )
import Language.Ginger.Parse
        ( POptions (..)
        , defPOptions
        , BlockTrimming (..)
        , BlockStripping (..)
        )
import qualified Language.Ginger.Parse as P
import Language.Ginger.Interpret.DefEnv
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

-- | One-stop function for parsing and interpreting a template.
ginger :: forall m. Monad m
       => TemplateLoader m
          -- ^ Template loader to use for loading the initial template and
          -- any included templates. For most use cases, 'fileLoader' should
          -- be appropriate.
       -> POptions
          -- ^ Parser options, determining parser behavior.
       -> JinjaDialect
          -- ^ Jinja dialect; currently determines which built-in globals to
          -- load into the initial namespace.
       -> Encoder m
          -- ^ Encoder to use for automatic encoding. Use 'htmlEncoder' for
          -- HTML templates.
       -> Text
          -- ^ Name of the initial template to load. For the 'fileLoader', this
          -- should be a filename, but for other loaders, it can be whatever
          -- the loader expects.
       -> Map Identifier (Value m)
          -- ^ Variables defined in the initial namespace.
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

$(addHaddockFromFile "src/Language/Ginger.haddock")
$(builtinsAutodoc)
