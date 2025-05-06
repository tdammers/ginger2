{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ginger.Value
where

import Data.String (IsString (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Int
import GHC.Float (float2Double)
import Control.Exception (throw)
import Test.Tasty.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC

import Language.Ginger.AST
import Language.Ginger.RuntimeError

data Scalar
  = NoneScalar
  | BoolScalar Bool
  | StringScalar Text
  | EncodedScalar Encoded
  | BytesScalar ByteString
  | IntScalar Integer
  | FloatScalar Double
  deriving (Show, Eq, Ord)

data Value m
  = ScalarV Scalar
  | ListV [Value m]
  | DictV (Map Scalar (Value m))
  | NativeV (NativeObject m)
  | ProcedureV (Procedure m)

instance Show (Value m) where
  show (ScalarV s) = show s
  show (ListV xs) = show xs
  show (DictV m) = show m
  show (NativeV {}) = "<<native>>"
  show (ProcedureV {}) = "<<procedure>>"

tagNameOf :: Value m -> Text
tagNameOf ScalarV {} = "scalar"
tagNameOf ListV {} = "list"
tagNameOf DictV {} = "dict"
tagNameOf NativeV {} = "native"
tagNameOf ProcedureV {} = "procedure"

pattern NoneV :: Value m
pattern NoneV = ScalarV NoneScalar

pattern BoolV :: Bool -> Value m
pattern BoolV b = ScalarV (BoolScalar b)

pattern StringV :: Text -> Value m
pattern StringV v = ScalarV (StringScalar v)

pattern EncodedV :: Encoded -> Value m
pattern EncodedV v = ScalarV (EncodedScalar v)

pattern BytesV :: ByteString -> Value m
pattern BytesV v = ScalarV (BytesScalar v)

pattern IntV :: Integer -> Value m
pattern IntV v = ScalarV (IntScalar v)

pattern FloatV :: Double -> Value m
pattern FloatV v = ScalarV (FloatScalar v)

data Procedure m
  = NativeProcedure ([(Maybe Identifier, Value m)] -> m (Either RuntimeError (Value m)))
  | GingerProcedure [(Identifier, Maybe Expr)] Expr

data NativeObject m =
  NativeObject
    { nativeObjectGetFieldNames :: m [Scalar]
    , nativeObjectGetField :: Scalar -> m (Maybe (Value m))
    , nativeObjectStringified :: m Text
    , nativeObjectEncoded :: m Encoded
    , nativeObjectAsList :: m (Maybe [Value m])
    , nativeObjectCall :: NativeObject m -> [(Maybe Identifier, Value m)] -> m (Either RuntimeError (Value m))
    }

defNativeObject :: Monad m => NativeObject m
defNativeObject =
  NativeObject
    { nativeObjectGetFieldNames = pure []
    , nativeObjectGetField = \_ -> pure Nothing
    , nativeObjectStringified = pure "<native object>"
    , nativeObjectEncoded = pure (Encoded "[[native object]]")
    , nativeObjectAsList = pure Nothing
    , nativeObjectCall = \obj _ -> throw . NonCallableObjectError . Just =<< nativeObjectStringified obj
    }

instance IsString Scalar where
  fromString = StringScalar . Text.pack

instance IsString (Value m) where
  fromString = ScalarV . fromString

class ToScalar a where
  toScalar :: a -> Scalar

instance ToScalar () where
  toScalar () = NoneScalar

instance ToScalar Bool where
  toScalar = BoolScalar

instance ToScalar Integer where
  toScalar = IntScalar

toIntScalar :: Integral a => a -> Scalar
toIntScalar = IntScalar . fromIntegral

instance ToScalar Int where
  toScalar = toIntScalar

instance ToScalar Word where
  toScalar = toIntScalar

instance ToScalar Int8 where
  toScalar = toIntScalar

instance ToScalar Int16 where
  toScalar = toIntScalar

instance ToScalar Int32 where
  toScalar = toIntScalar

instance ToScalar Int64 where
  toScalar = toIntScalar

instance ToScalar Word8 where
  toScalar = toIntScalar

instance ToScalar Word16 where
  toScalar = toIntScalar

instance ToScalar Word32 where
  toScalar = toIntScalar

instance ToScalar Word64 where
  toScalar = toIntScalar

instance ToScalar Double where
  toScalar = FloatScalar

instance ToScalar Float where
  toScalar = FloatScalar . float2Double


instance ToScalar Text where
  toScalar = StringScalar

instance ToScalar LText.Text where
  toScalar = StringScalar . LText.toStrict

instance ToScalar ByteString where
  toScalar = BytesScalar

instance ToScalar LBS.ByteString where
  toScalar = BytesScalar . LBS.toStrict

instance ToScalar a => ToScalar (Maybe a) where
  toScalar Nothing = NoneScalar
  toScalar (Just x) = toScalar x

instance (ToScalar a, ToScalar b) => ToScalar (Either a b) where
  toScalar (Left x) = toScalar x
  toScalar (Right x) = toScalar x

--------------------------------------------------------------------------------
-- ToValue
--------------------------------------------------------------------------------

class ToValue a m where
  toValue :: a -> Value m

instance ToValue (Value m) m where
  toValue = id

--------------------------------------------------------------------------------
-- Scalar instances
--------------------------------------------------------------------------------

instance ToValue () a where
  toValue = ScalarV . toScalar

instance ToValue Bool a where
  toValue = ScalarV . toScalar

instance ToValue Integer a where
  toValue = ScalarV . toScalar

instance ToValue Int a where
  toValue = ScalarV . toScalar

instance ToValue Int8 a where
  toValue = ScalarV . toScalar

instance ToValue Int16 a where
  toValue = ScalarV . toScalar

instance ToValue Int32 a where
  toValue = ScalarV . toScalar

instance ToValue Int64 a where
  toValue = ScalarV . toScalar

instance ToValue Word a where
  toValue = ScalarV . toScalar

instance ToValue Word8 a where
  toValue = ScalarV . toScalar

instance ToValue Word16 a where
  toValue = ScalarV . toScalar

instance ToValue Word32 a where
  toValue = ScalarV . toScalar

instance ToValue Word64 a where
  toValue = ScalarV . toScalar

instance ToValue Text a where
  toValue = ScalarV . toScalar

instance ToValue LText.Text a where
  toValue = ScalarV . toScalar

instance ToValue ByteString a where
  toValue = ScalarV . toScalar

instance ToValue LBS.ByteString a where
  toValue = ScalarV . toScalar

--------------------------------------------------------------------------------
-- Compound / derived instances
--------------------------------------------------------------------------------

instance ToValue a m => ToValue (Maybe a) m where
  toValue Nothing = ScalarV NoneScalar
  toValue (Just x) = toValue x

instance (ToValue a m, ToValue b m) => ToValue (Either a b) m where
  toValue (Left x) = toValue x
  toValue (Right x) = toValue x

instance ToValue a m => ToValue [a] m where
  toValue = ListV . map toValue

instance (ToValue a1 m, ToValue a2 m)
         => ToValue (a1, a2) m where
  toValue (x1, x2) =
    ListV [toValue x1, toValue x2]

instance (ToValue a1 m, ToValue a2 m, ToValue a3 m)
         => ToValue (a1, a2, a3) m where
  toValue (x1, x2, x3) =
    ListV [toValue x1, toValue x2, toValue x3]

instance (ToValue a1 m, ToValue a2 m, ToValue a3 m, ToValue a4 m)
         => ToValue (a1, a2, a3, a4) m where
  toValue (x1, x2, x3, x4) =
    ListV [toValue x1, toValue x2, toValue x3, toValue x4]

instance (ToValue a1 m, ToValue a2 m, ToValue a3 m, ToValue a4 m, ToValue a5 m)
         => ToValue (a1, a2, a3, a4, a5) m where
  toValue (x1, x2, x3, x4, x5) =
    ListV [toValue x1, toValue x2, toValue x3, toValue x4, toValue x5]

instance (ToValue a1 m, ToValue a2 m, ToValue a3 m, ToValue a4 m, ToValue a5 m, ToValue a6 m)
         => ToValue (a1, a2, a3, a4, a5, a6) m where
  toValue (x1, x2, x3, x4, x5, x6) =
    ListV [toValue x1, toValue x2, toValue x3, toValue x4, toValue x5, toValue x6]

instance (ToValue a1 m, ToValue a2 m, ToValue a3 m, ToValue a4 m, ToValue a5 m, ToValue a6 m, ToValue a7 m)
         => ToValue (a1, a2, a3, a4, a5, a6, a7) m where
  toValue (x1, x2, x3, x4, x5, x6, x7) =
    ListV [toValue x1, toValue x2, toValue x3, toValue x4, toValue x5, toValue x6, toValue x7]

instance (ToScalar k, ToValue v m) => ToValue (Map k v) m where
  toValue = DictV . Map.mapKeys toScalar . Map.map toValue

instance ToValue v m => ToValue (Map LText.Text v) m where
  toValue = toValue . Map.mapKeys LText.toStrict

instance ToValue v m => ToValue (Map String v) m where
  toValue = toValue . Map.mapKeys Text.pack

--------------------------------------------------------------------------------
-- Function instances
--------------------------------------------------------------------------------

class ToNativeProcedure m a where
  toNativeProcedure :: a -> [(Maybe Identifier, Value m)] -> m (Either RuntimeError (Value m))

instance Applicative m => ToNativeProcedure m (Value m) where
  toNativeProcedure val [] =
    pure (Right val)
  toNativeProcedure _ _ =
    pure . Left $
      ArgumentError (Just "<native function>") Nothing (Just "end of arguments") (Just "value")

instance Applicative m => ToNativeProcedure m (m (Value m)) where
  toNativeProcedure action [] =
    Right <$> action
  toNativeProcedure _ _ =
    pure . Left $
      ArgumentError (Just "<native function>") Nothing (Just "end of arguments") (Just "value")

instance (Applicative m, ToNativeProcedure m a) => ToNativeProcedure m (Value m -> a) where
  toNativeProcedure _ [] =
    pure . Left $
      ArgumentError (Just "<native function>") Nothing (Just "value") (Just "end of arguments")
  toNativeProcedure _ ((Just _, _):_) =
    pure . Left $
      ArgumentError (Just "<native function>") Nothing (Just "positional argument") (Just "named argument")
  toNativeProcedure f ((Nothing, v):xs) =
    toNativeProcedure (f v) xs


instance Applicative m => ToValue (Value m -> Value m) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> Value m) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> Value m -> Value m) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> Value m -> Value m -> Value m) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure


instance Applicative m => ToValue (Value m -> m (Value m)) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> m (Value m)) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> m (Value m)) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> Value m -> m (Value m)) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

instance Applicative m => ToValue (Value m -> Value m -> Value m -> Value m -> Value m -> m (Value m)) m where
  toValue = ProcedureV . NativeProcedure . toNativeProcedure

--------------------------------------------------------------------------------
-- Dictionary helpers
--------------------------------------------------------------------------------

dictV :: [(Scalar, Value m)] -> Value m
dictV items = DictV $ Map.fromList items

infixr 8 .=

(.=) :: (ToValue v m) => Scalar -> v -> (Scalar, Value m)
k .= v = (k, toValue v)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Scalar where
  arbitrary =
    QC.oneof
      [ pure NoneScalar
      , BoolScalar <$> arbitrary
      , StringScalar . Text.pack <$> QC.listOf arbitrary
      , EncodedScalar . Encoded . Text.pack <$> QC.listOf arbitrary
      , BytesScalar . BS.pack <$> QC.listOf arbitrary
      , IntScalar <$> arbitrary
      , FloatScalar <$> arbitrary
      ]

  shrink = \case
    BoolScalar True -> [BoolScalar False]
    StringScalar str | Text.null str -> []
    StringScalar str -> [StringScalar $ Text.init str]
    EncodedScalar (Encoded str) | Text.null str -> []
    EncodedScalar (Encoded str) -> [EncodedScalar . Encoded $ Text.init str]
    BytesScalar str | BS.null str -> []
    BytesScalar str -> [BytesScalar $ BS.init str]
    IntScalar i -> NoneScalar : (IntScalar <$> shrink i)
    FloatScalar f -> NoneScalar : (FloatScalar <$> shrink f)
    NoneScalar -> []
    _ -> [NoneScalar]

instance Arbitrary (Value m) where
  arbitrary =
    QC.oneof
      [ pure NoneV
      , ScalarV <$> arbitrary
      , ListV <$> fuelledList arbitrary
      , DictV . Map.fromList <$> fuelledList arbitrary
      ]
