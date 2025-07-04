{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.TestUtils
where

import Control.Monad.Random (Rand, evalRand)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Test.Tasty.QuickCheck
import System.Random (mkStdGen, StdGen)

import Language.Ginger.Interpret
import Language.Ginger.Value
import Language.Ginger.AST

type GIdentity = Rand StdGen

newtype ArbitraryText = ArbitraryText { unArbitraryText :: Text }
  deriving (Eq, Ord)

instance Show ArbitraryText where
  show (ArbitraryText t) = show t

instance Arbitrary ArbitraryText where
  arbitrary = ArbitraryText . Text.pack <$> listOf arbitrary

instance Monad m => ToValue ArbitraryText m where
  toValue (ArbitraryText t) = toValue t

newtype NonEmptyText = NonEmptyText Text
  deriving (Eq, Ord)

instance Show NonEmptyText where
  show (NonEmptyText t) = show t

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . Text.pack <$> listOf1 arbitrary

instance Monad m => ToValue NonEmptyText m where
  toValue (NonEmptyText t) = toValue t

newtype ArbitraryByteString = ArbitraryByteString ByteString
  deriving (Eq, Ord)

instance Show ArbitraryByteString where
  show (ArbitraryByteString t) = show t

instance Arbitrary ArbitraryByteString where
  arbitrary = ArbitraryByteString . BS.pack <$> listOf arbitrary

instance Monad m => ToValue ArbitraryByteString m where
  toValue (ArbitraryByteString t) = toValue t

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

leftPRE :: RuntimeError -> Either PrettyRuntimeError a
leftPRE = Left . PrettyRuntimeError

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right (f x)

newtype PrettyRuntimeError = PrettyRuntimeError RuntimeError
  deriving (Eq)

instance Show PrettyRuntimeError where
  show (PrettyRuntimeError (TemplateParseError _ err)) = Text.unpack err
  show (PrettyRuntimeError e) = show e

runGingerIdentity :: Int -> GingerT GIdentity a -> a
runGingerIdentity rngSeed action =
  either (error . show) id $ runGingerIdentityEither rngSeed action

runGingerIdentityEither :: Int -> GingerT GIdentity a -> Either PrettyRuntimeError a
runGingerIdentityEither rngSeed action =
  mapLeft (PrettyRuntimeError . unPositionedError) $
    evalRand (runGingerT action defContext defEnv) (mkStdGen rngSeed)

runGingerIdentityWithLoader :: TemplateLoader GIdentity
                            -> Int
                            -> GingerT GIdentity a
                            -> a
runGingerIdentityWithLoader loader rngSeed action =
  either (error . show) id $ runGingerIdentityEitherWithLoader loader rngSeed action

runGingerIdentityEitherWithLoader :: TemplateLoader GIdentity
                                  -> Int
                                  -> GingerT GIdentity a
                                  -> Either PrettyRuntimeError a
runGingerIdentityEitherWithLoader loader rngSeed action =
  mapLeft (PrettyRuntimeError . unPositionedError) $
    evalRand
      (runGingerT
        action
        defContext { contextLoadTemplateFile = loader }
        defEnv
      )
      (mkStdGen rngSeed)

mockLoader :: [(Text, Text)] -> TemplateLoader GIdentity
mockLoader entries name =
  pure $ Map.lookup name tpls
  where
    tpls = Map.fromList entries

unPositionedS :: Statement -> Statement
unPositionedS = omapS go unPositionedE
  where
    go (PositionedS _ s) = unPositionedS s
    go s = s

unPositionedE :: Expr -> Expr
unPositionedE = omapE go unPositionedS
  where
    go (PositionedE _ e) = unPositionedE e
    go e = e

unPositionedError :: RuntimeError -> RuntimeError
unPositionedError (PositionedError _ e) = unPositionedError e
unPositionedError e = e
