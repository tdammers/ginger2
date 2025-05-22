module Language.Ginger.SourcePosition
where

import Data.Text (Text)
import qualified Data.Text as Text

data SourcePosition =
  SourcePosition
    { sourceFile :: !Text
    , sourceLine :: !Int
    , sourceColumn :: !Int
    }
    deriving (Show, Eq)

prettySourcePosition :: SourcePosition -> String
prettySourcePosition s =
  Text.unpack (sourceFile s) ++ ":" ++ show (sourceLine s) ++ ":" ++ show (sourceColumn s)
