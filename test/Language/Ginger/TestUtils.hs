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

newtype PositiveInt i = PositiveInt i
  deriving (Eq, Ord)

instance Show i => Show (PositiveInt i) where
  show (PositiveInt t) = show t

instance (Arbitrary i, Integral i) => Arbitrary (PositiveInt i) where
  arbitrary = do
    s <- getSize
    PositiveInt . fromInteger <$> chooseInteger (1, fromIntegral $ max 1 s)
  shrink (PositiveInt i) = map PositiveInt . filter (> 0) $ shrink i

instance (Monad m, ToValue i m) => ToValue (PositiveInt i) m where
  toValue (PositiveInt t) = toValue t

safeAt :: Int -> [a] -> Maybe a
safeAt n = listToMaybe . drop n

justNonzero :: (Eq a, Num a) => a -> Maybe a
justNonzero 0 = Nothing
justNonzero n = Just n

justPositive :: (Eq a, Ord a, Num a) => a -> Maybe a
justPositive n | n > 0 = Just n
justPositive _ = Nothing  

