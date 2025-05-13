{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.TestUtils
where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.QuickCheck
import Data.Maybe (listToMaybe)

import Language.Ginger.Value

newtype ArbitraryText = ArbitraryText Text
  deriving (Eq, Ord)

instance Show ArbitraryText where
  show (ArbitraryText t) = show t

instance Arbitrary ArbitraryText where
  arbitrary = ArbitraryText . Text.pack <$> listOf arbitrary

instance Monad m => ToValue ArbitraryText m where
  toValue (ArbitraryText t) = toValue t

safeAt :: Int -> [a] -> Maybe a
safeAt n = listToMaybe . drop n

justNonzero :: (Eq a, Num a) => a -> Maybe a
justNonzero 0 = Nothing
justNonzero n = Just n

justPositive :: (Eq a, Ord a, Num a) => a -> Maybe a
justPositive n | n > 0 = Just n
justPositive _ = Nothing  

