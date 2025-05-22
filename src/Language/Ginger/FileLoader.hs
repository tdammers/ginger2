{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger.FileLoader
where

import System.FilePath
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)

import Language.Ginger.Value

fileLoader :: forall m. MonadIO m
           => FilePath
           -> TemplateLoader m
fileLoader baseDir templateName = do
  let templateBasename =
          Text.unpack .
          Text.replace ".." "" .
          Text.replace "//" "/" .
          fromMaybe templateName .
          Text.stripPrefix "/" $
            templateName
      templateFilename =
          baseDir </> templateBasename
  liftIO $ Just <$> Text.readFile templateFilename
