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
import Test.Tasty.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC

import Language.Ginger.AST
import Language.Ginger.RuntimeError

newtype Env m =
  Env
    { envVars :: Map Identifier (Value m)
    }

emptyEnv :: Env m
emptyEnv = Env mempty

instance Semigroup (Env m) where
  a <> b = Env { envVars = envVars a <> envVars b }

instance Monoid (Env m) where
  mempty = Env mempty

data Context m =
  Context
    { contextEncode :: Text -> m Encoded
    }

defContext :: Applicative m => Context m
defContext =
  Context
    { contextEncode = pure . Encoded
    }

data Scalar
  = NoneScalar
  | BoolScalar !Bool
  | StringScalar !Text
  | EncodedScalar !Encoded
  | BytesScalar !ByteString
  | IntScalar !Integer
  | FloatScalar !Double
  deriving (Show, Eq, Ord)

data Value m
  = ScalarV !Scalar
  | ListV ![Value m]
  | DictV !(Map Scalar (Value m))
  | NativeV !(NativeObject m)
  | ProcedureV !(Procedure m)
  | TestV !(Test m)
  | FilterV !(Filter m)

traverseValue :: Monoid a => (Value m -> a) -> Value m -> a
traverseValue p v@(ListV xs) =
  p v <> mconcat (map (traverseValue p) xs)
traverseValue p v@(DictV m) =
  p v <> mconcat (map (traverseValue p . snd) $ Map.toList m)
traverseValue p v = p v

instance Show (Value m) where
  show (ScalarV s) = show s
  show (ListV xs) = show xs
  show (DictV m) = show m
  show (NativeV {}) = "<<native>>"
  show (ProcedureV {}) = "<<procedure>>"
  show (TestV {}) = "<<test>>"
  show (FilterV {}) = "<<filter>>"

instance Eq (Value m) where
  ScalarV a == ScalarV b = a == b
  ListV a == ListV b = a == b
  DictV a == DictV b = a == b
  _ == _ = False

tagNameOf :: Value m -> Text
tagNameOf ScalarV {} = "scalar"
tagNameOf ListV {} = "list"
tagNameOf DictV {} = "dict"
tagNameOf NativeV {} = "native"
tagNameOf ProcedureV {} = "procedure"
tagNameOf TestV {} = "test"
tagNameOf FilterV {} = "filter"

pattern NoneV :: Value m
pattern NoneV = ScalarV NoneScalar

pattern BoolV :: Bool -> Value m
pattern BoolV b = ScalarV (BoolScalar b)

pattern TrueV :: Value m
pattern TrueV = BoolV True

pattern FalseV :: Value m
pattern FalseV = BoolV False

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
  = NativeProcedure !([(Maybe Identifier, Value m)] -> m (Either RuntimeError (Value m)))
  | GingerProcedure !(Env m) ![(Identifier, Maybe (Value m))] !Expr

pureNativeProcedure :: Applicative m
                    => ([(Maybe Identifier, Value m)] -> Either RuntimeError (Value m))
                    -> Procedure m
pureNativeProcedure f =
  NativeProcedure $ \args -> pure (f args)

pureNativeFunc :: (Applicative m)
               => (Value m -> Either RuntimeError (Value m))
               -> Procedure m
pureNativeFunc f =
  NativeProcedure $ \case
    [] ->
      pure . Left $
        ArgumentError (Just "<native function>") Nothing (Just "value") (Just "end of arguments")
    [(_, x)] ->
      pure $ f x
    (_:(name, x):_) ->
      pure . Left $
        ArgumentError (Just "<native function>") (identifierName <$> name) (Just "end of arguments") (Just . tagNameOf $ x)

pureNativeFunc2 :: (Applicative m)
               => (Value m -> Value m -> Either RuntimeError (Value m))
               -> Procedure m
pureNativeFunc2 f =
  NativeProcedure $ \case
    [] ->
      pure . Left $
        ArgumentError (Just "<native function>") Nothing (Just "value") (Just "end of arguments")
    [_] ->
      pure . Left $
        ArgumentError (Just "<native function>") Nothing (Just "value") (Just "end of arguments")
    [(_, x), (_, y)] ->
      pure $ f x y
    (_:_:(name, x):_) ->
      pure . Left $
        ArgumentError (Just "<native function>") (identifierName <$> name) (Just "end of arguments") (Just . tagNameOf $ x)

type MetaFunc m a =
     Expr
  -> [(Maybe Identifier, Value m)]
  -> Context m
  -> Env m
  -> m (Either RuntimeError a)

type TestFunc m = MetaFunc m Bool

type FilterFunc m = MetaFunc m (Value m)

newtype Test m = NativeTest { runTest :: TestFunc m }

newtype Filter m = NativeFilter { runFilter :: FilterFunc m }

data NativeObject m =
  NativeObject
    { nativeObjectGetFieldNames :: m [Scalar]
    , nativeObjectGetField :: Value m -> m (Maybe (Value m))
    , nativeObjectGetAttribute :: Value m -> m (Maybe (Value m))
    , nativeObjectStringified :: m Text
    , nativeObjectEncoded :: m Encoded
    , nativeObjectAsList :: m (Maybe [Value m])
    , nativeObjectCall :: Maybe (NativeObject m -> [(Maybe Identifier, Value m)] -> m (Either RuntimeError (Value m)))
    , nativeObjectEq :: NativeObject m -> NativeObject m -> m (Either RuntimeError Bool)
    }

(-->) :: obj -> (obj -> obj -> a) -> a
obj --> field = field obj obj

defNativeObject :: Monad m => NativeObject m
defNativeObject =
  NativeObject
    { nativeObjectGetFieldNames = pure []
    , nativeObjectGetField = \_ -> pure Nothing
    , nativeObjectGetAttribute = \_ -> pure Nothing
    , nativeObjectStringified = pure "<native object>"
    , nativeObjectEncoded = pure (Encoded "[[native object]]")
    , nativeObjectAsList = pure Nothing
    , nativeObjectCall = Nothing
    , nativeObjectEq = \_self _other -> pure . Right $ False
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

instance ToValue Double a where
  toValue = ScalarV . toScalar

instance ToValue Float a where
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
-- Procedure helpers
--------------------------------------------------------------------------------

resolveArgs :: Maybe Text
            -> [(Identifier, Maybe (Value m))]
            -> [(Maybe Identifier, Value m)]
            -> Either RuntimeError (Map Identifier (Value m))
resolveArgs context specs args =
  let kwargs0 = Map.fromList [(k, v) | (Just k, v) <- args]
      varargs0 = [v | (Nothing, v) <- args]
  in go specs kwargs0 varargs0
  where
    go ((argName, defVal):xs) kwargs varargs =
      case Map.lookup argName kwargs of
        Nothing ->
          -- positional argument
          case varargs of
            [] ->
              -- No more arguments passed, look for default value
              maybe
                -- No default, argument required
                (Left $
                  ArgumentError
                    context
                    (Just $ identifierName argName)
                    (Just "argument")
                    (Just "end of arguments")
                )
                -- Default exists, use it.
                (\val ->
                  Map.insert argName val <$> go xs kwargs varargs
                )
                defVal
            (v:varargs') ->
              -- Argument passed, use it.
              Map.insert argName v <$> go xs kwargs varargs'
        Just v ->
          -- Keyword argument found.
          Map.insert argName v <$> go xs (Map.delete argName kwargs) varargs

    go [] kwargs varargs =
        -- Map remaining arguments to @varargs@ and @kwargs@.
        Right $ Map.fromList
          [ ("varargs", ListV varargs)
          , ("kwargs", DictV (Map.mapKeys (toScalar . identifierName) kwargs))
          ]

leftNaN :: Double -> Either RuntimeError Double
leftNaN c | isNaN c = Left $ NumericError Nothing (Just "not a number")
leftNaN c | isInfinite c = Left $ NumericError Nothing (Just "infinity")
leftNaN c = Right c

numericFunc :: Monad m
             => (Integer -> Integer)
             -> (Double -> Double)
             -> Value m
             -> Either RuntimeError (Value m)
numericFunc f g =
  numericFuncCatch f' g'
  where
    f' x = Right (f x)
    g' x = Right (g x)

numericFuncCatch :: Monad m
                  => (Integer -> Either RuntimeError Integer)
                  -> (Double -> Either RuntimeError Double)
                  -> Value m
                  -> Either RuntimeError (Value m)
numericFuncCatch f _ (IntV a) = IntV <$> f a
numericFuncCatch _ f (FloatV a) = FloatV <$> (leftNaN =<< f a)
numericFuncCatch _ _ a = Left (TagError Nothing (Just "number") (Just . tagNameOf $ a))

asIntVal :: Value m -> Either RuntimeError Integer
asIntVal (IntV a) = Right a
asIntVal x = Left $ TagError Nothing (Just "int") (Just . tagNameOf $ x)

asFloatVal :: Value m -> Either RuntimeError Double
asFloatVal (FloatV a) = Right a
asFloatVal (IntV a) = Right (fromInteger a)
asFloatVal x = Left $ TagError Nothing (Just "float") (Just . tagNameOf $ x)

asBoolVal :: Value m -> Either RuntimeError Bool
asBoolVal (BoolV a) = Right a
asBoolVal NoneV = Right False
asBoolVal x = Left $ TagError Nothing (Just "bool") (Just . tagNameOf $ x)

asListVal :: Monad m => Value m -> m (Either RuntimeError [Value m])
asListVal (ListV a) = pure $ Right a
asListVal (NativeV n) =
  maybe
    (Left $ TagError Nothing (Just "list") (Just "non-list native object"))
    Right <$>
    nativeObjectAsList n
asListVal x = pure . Left $ TagError Nothing (Just "list") (Just . tagNameOf $ x)

asTextVal :: Value m -> Either RuntimeError Text
asTextVal (StringV a) = Right a
asTextVal (EncodedV (Encoded a)) = Right a
asTextVal (IntV a) = Right (Text.show a)
asTextVal (FloatV a) = Right (Text.show a)
asTextVal NoneV = Right ""
asTextVal x = Left $ TagError Nothing (Just "string") (Just . tagNameOf $ x)

intFunc :: Monad m
         => (Integer -> Either RuntimeError Integer)
         -> Value m
         -> Either RuntimeError (Value m)
intFunc f a = IntV <$> (asIntVal a >>= f)

floatFunc :: Monad m
         => (Double -> Either RuntimeError Double)
         -> Value m
         -> Either RuntimeError (Value m)
floatFunc f a = FloatV <$> (asFloatVal a >>= f)

boolFunc :: Monad m
         => (Bool -> Bool)
         -> Value m
         -> Either RuntimeError (Value m)
boolFunc f (BoolV a) = pure . BoolV $ f a
boolFunc _ a = Left (TagError Nothing (Just "bool") (Just . tagNameOf $ a))

textFunc :: Monad m
         => (Text -> Either RuntimeError Text)
         -> Value m
         -> Either RuntimeError (Value m)
textFunc f (StringV a) = StringV <$> f a
textFunc f (EncodedV (Encoded a)) = EncodedV . Encoded <$> f a
textFunc f (IntV a) = StringV <$> f (Text.show a)
textFunc f (FloatV a) = StringV <$> f (Text.show a)
textFunc f NoneV = StringV <$> f ""
textFunc _ a = Left (TagError Nothing (Just "int") (Just . tagNameOf $ a))

numericFunc2 :: Monad m
             => (Integer -> Integer -> Integer)
             -> (Double -> Double -> Double)
             -> Value m
             -> Value m
             -> Either RuntimeError (Value m)
numericFunc2 f g =
  numericFunc2Catch f' g'
  where
    f' x y = Right (f x y)
    g' x y = Right (g x y)

numericFunc2Catch :: Monad m
                  => (Integer -> Integer -> Either RuntimeError Integer)
                  -> (Double -> Double -> Either RuntimeError Double)
                  -> Value m
                  -> Value m
                  -> Either RuntimeError (Value m)
numericFunc2Catch f _ (IntV a) (IntV b) = IntV <$> (a `f` b)
numericFunc2Catch _ f (FloatV a) (FloatV b) = FloatV <$> (leftNaN =<< a `f` b)
numericFunc2Catch _ f (IntV a) (FloatV b) = FloatV <$> (leftNaN =<< fromInteger a `f` b)
numericFunc2Catch _ f (FloatV a) (IntV b) = FloatV <$> (leftNaN =<< a `f` fromInteger b)
numericFunc2Catch _ _ (FloatV _) b = Left (TagError Nothing (Just "number") (Just . tagNameOf $ b))
numericFunc2Catch _ _ (IntV _) b = Left (TagError Nothing (Just "number") (Just . tagNameOf $ b))
numericFunc2Catch _ _ b _ = Left (TagError Nothing (Just "number") (Just . tagNameOf $ b))

intFunc2 :: Monad m
         => (Integer -> Integer -> Either RuntimeError Integer)
         -> Value m
         -> Value m
         -> Either RuntimeError (Value m)
intFunc2 f a b = do
  x <- asIntVal a
  y <- asIntVal b
  IntV <$> f x y

floatFunc2 :: Monad m
         => (Double -> Double -> Either RuntimeError Double)
         -> Value m
         -> Value m
         -> Either RuntimeError (Value m)
floatFunc2 f (IntV a) b = floatFunc2 f (FloatV $ fromIntegral a) b
floatFunc2 f (FloatV a) (IntV b) = floatFunc2 f (FloatV a) (FloatV $ fromIntegral b)
floatFunc2 f (FloatV a) (FloatV b) = FloatV <$> (a `f` b)
floatFunc2 _ (FloatV _) b = Left (TagError Nothing (Just "float") (Just . tagNameOf $ b))
floatFunc2 _ b _ = Left (TagError Nothing (Just "float") (Just . tagNameOf $ b))

boolFunc2 :: Monad m
         => (Bool -> Bool -> Bool)
         -> Value m
         -> Value m
         -> Either RuntimeError (Value m)
boolFunc2 f a b = BoolV <$> (f <$> asBoolVal a <*> asBoolVal b)

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

instance Monad m => Arbitrary (Value m) where
  arbitrary =
    QC.oneof
      [ pure NoneV
      , ScalarV <$> arbitrary
      , ListV <$> fuelledList arbitrary
      , DictV . Map.fromList <$> fuelledList arbitrary
      , NativeV <$> arbitraryNative
      , ProcedureV <$> arbitraryNativeProcedure
      ]

arbitraryNativeProcedure :: Monad m => QC.Gen (Procedure m)
arbitraryNativeProcedure = do
  retval <- QC.scale (`div` 2) arbitrary
  pure $ NativeProcedure (\_ -> pure (Right retval))

arbitraryNative :: Monad m => QC.Gen (NativeObject m)
arbitraryNative = do
  objectID <- arbitrary
  pure defNativeObject
    { nativeObjectGetFieldNames = pure ["id"]
    , nativeObjectGetField = \case
        "id" -> pure . Just . toValue . identifierName $ objectID
        _ -> pure Nothing
    , nativeObjectStringified = pure $ identifierName objectID
    , nativeObjectEq = \self other -> do
        otherID <- nativeObjectGetField other "id"
        selfID <- nativeObjectGetField self "id"
        pure . Right $ otherID == selfID
    }
