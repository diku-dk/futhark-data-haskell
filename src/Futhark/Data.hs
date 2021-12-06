{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- | This module defines an efficient value representation of the
-- Futhark data format.
module Futhark.Data
  ( Value (..),
    Vector,
    valueText,

    -- * Types of values
    PrimType (..),
    primTypeText,
    primTypeBytes,
    ValueType (..),
    valueTypeTextNoDims,
    valueType,
    valueElemType,
    valueShape,
    valueTypeText,

    -- * Converting values
    GetValue (..),
    PutValue (..),
    PutValue1 (..),
    valueElems,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (chr, ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector.Storable as SVec
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Numeric.Half

-- | The value vector type.
type Vector = SVec.Vector

-- | An efficiently represented Futhark value, represented as a shape
-- vector and a value vector, which contains elements in row-major
-- order.  The size of the value vector must be equal to the product
-- of the shape vector.  This is not enforced by the representation,
-- but consuming functions may give unexpected results if this
-- invariant is broken.  Scalars are represented with an empty shape
-- vector.
--
-- Use 'valueText' to get a human-readable representation, and v'put'
-- to obtain binary a representation.
--
-- The 'Eq' instance is the naive one, meaning that no values
-- containing NaNs will be considered equal.  Use the functions from
-- "Futhark.Data.Compare" if this is not what you want.
data Value
  = I8Value (Vector Int) (Vector Int8)
  | I16Value (Vector Int) (Vector Int16)
  | I32Value (Vector Int) (Vector Int32)
  | I64Value (Vector Int) (Vector Int64)
  | U8Value (Vector Int) (Vector Word8)
  | U16Value (Vector Int) (Vector Word16)
  | U32Value (Vector Int) (Vector Word32)
  | U64Value (Vector Int) (Vector Word64)
  | F16Value (Vector Int) (Vector Half)
  | F32Value (Vector Int) (Vector Float)
  | F64Value (Vector Int) (Vector Double)
  | BoolValue (Vector Int) (Vector Bool)
  deriving (Eq, Show)

binaryFormatVersion :: Word8
binaryFormatVersion = 2

instance Binary Value where
  put (I8Value shape vs) = putBinaryValue "  i8" shape vs
  put (I16Value shape vs) = putBinaryValue " i16" shape vs
  put (I32Value shape vs) = putBinaryValue " i32" shape vs
  put (I64Value shape vs) = putBinaryValue " i64" shape vs
  put (U8Value shape vs) = putBinaryValue "  u8" shape vs
  put (U16Value shape vs) = putBinaryValue " u16" shape vs
  put (U32Value shape vs) = putBinaryValue " u32" shape vs
  put (U64Value shape vs) = putBinaryValue " u64" shape vs
  put (F16Value shape vs) = putBinaryValue " f16" shape vs
  put (F32Value shape vs) = putBinaryValue " f32" shape vs
  put (F64Value shape vs) = putBinaryValue " f64" shape vs
  -- Bool must be treated specially because the Storable instance
  -- uses four bytes.
  put (BoolValue shape vs) = putBinaryValue "bool" shape $ SVec.map boolToInt8 vs
    where
      boolToInt8 True = 1 :: Int8
      boolToInt8 False = 0

  get = do
    first <- getInt8
    version <- getWord8
    rank <- getInt8

    unless (chr (fromIntegral first) == 'b') $
      fail "Input does not begin with ASCII 'b'."
    unless (version == binaryFormatVersion) $
      fail $ "Expecting binary format version 1; found version: " ++ show version
    unless (rank >= 0) $
      fail $ "Rank must be non-negative, but is: " ++ show rank

    type_f <- getLazyByteString 4

    shape <- replicateM (fromIntegral rank) $ fromIntegral <$> getInt64le
    let num_elems = product shape
        shape' = SVec.fromList shape

    case LBS.unpack type_f of
      "  i8" -> get' (I8Value shape') num_elems 1
      " i16" -> get' (I16Value shape') num_elems 2
      " i32" -> get' (I32Value shape') num_elems 4
      " i64" -> get' (I64Value shape') num_elems 8
      "  u8" -> get' (U8Value shape') num_elems 1
      " u16" -> get' (U16Value shape') num_elems 2
      " u32" -> get' (U32Value shape') num_elems 4
      " u64" -> get' (U64Value shape') num_elems 8
      " f16" -> get' (F16Value shape') num_elems 2
      " f32" -> get' (F32Value shape') num_elems 4
      " f64" -> get' (F64Value shape') num_elems 8
      -- Bool must be treated specially because the Storable instance
      -- uses four bytes.
      "bool" -> BoolValue shape' . SVec.map int8ToBool . byteStringToVector . BS.copy <$> getByteString num_elems
      s -> fail $ "Cannot parse binary values of type " ++ show s
    where
      -- The copy is to ensure that the bytestring is properly
      -- aligned.
      get' mk num_elems elem_size =
        mk . byteStringToVector . BS.copy <$> getByteString (num_elems * elem_size)

      int8ToBool :: Int8 -> Bool
      int8ToBool = (/= 0)

putBinaryValue ::
  SVec.Storable a =>
  String ->
  Vector Int ->
  Vector a ->
  Put
putBinaryValue tstr shape vs = do
  putInt8 $ fromIntegral $ ord 'b'
  putWord8 binaryFormatVersion
  putWord8 $ fromIntegral $ SVec.length shape
  mapM_ (putInt8 . fromIntegral . ord) tstr
  putByteString $ vectorToByteString shape
  putByteString $ vectorToByteString vs

arrayText :: (SVec.Storable a) => (a -> TB.Builder) -> [Int] -> SVec.Vector a -> TB.Builder
arrayText p [] vs =
  p $ SVec.head vs
arrayText p (d : ds) vs =
  "[" <> mconcat (intersperse separator $ map (arrayText p ds . slice) [0 .. d -1]) <> "]"
  where
    slice_size = product ds
    slice i = SVec.slice (i * slice_size) slice_size vs
    separator
      | null ds = ", "
      | otherwise = ",\n"

-- | Construct a textual representation of the value as a strict text.
valueText :: Value -> T.Text
valueText v
  | product (valueShape v) == 0 =
    "empty(" <> dims <> primTypeText (valueElemType v) <> ")"
  where
    dims = mconcat $ map (brackets . T.pack . show) $ valueShape v
    brackets s = "[" <> s <> "]"
valueText v =
  case v of
    I8Value shape vs -> f pNum shape vs
    I16Value shape vs -> f pNum shape vs
    I32Value shape vs -> f pNum shape vs
    I64Value shape vs -> f pNum shape vs
    U8Value shape vs -> f pNum shape vs
    U16Value shape vs -> f pNum shape vs
    U32Value shape vs -> f pNum shape vs
    U64Value shape vs -> f pNum shape vs
    F16Value shape vs -> f pF16 shape vs
    F32Value shape vs -> f pF32 shape vs
    F64Value shape vs -> f pF64 shape vs
    BoolValue shape vs -> f pBool shape vs
  where
    suffix = primTypeText $ valueElemType v
    pNum x = TB.fromString (show x) <> TB.fromText suffix
    pF16 x
      | isInfinite x, x >= 0 = "f16.inf"
      | isInfinite x, x < 0 = "-f16.inf"
      | isNaN x = "f16.nan"
      | otherwise = pNum x
    pF32 x
      | isInfinite x, x >= 0 = "f32.inf"
      | isInfinite x, x < 0 = "-f32.inf"
      | isNaN x = "f32.nan"
      | otherwise = pNum x
    pF64 x
      | isInfinite x, x >= 0 = "f64.inf"
      | isInfinite x, x < 0 = "-f64.inf"
      | isNaN x = "f64.nan"
      | otherwise = pNum x

    pBool True = "true"
    pBool False = "false"

    f p shape vs = LT.toStrict $ TB.toLazyText $ arrayText p (SVec.toList shape) vs

-- | The scalar types supported by the value format.
data PrimType = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | F16 | F32 | F64 | Bool
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Textual primitive type as a strict text.
primTypeText :: PrimType -> T.Text
primTypeText I8 = "i8"
primTypeText I16 = "i16"
primTypeText I32 = "i32"
primTypeText I64 = "i64"
primTypeText U8 = "u8"
primTypeText U16 = "u16"
primTypeText U32 = "u32"
primTypeText U64 = "u64"
primTypeText F16 = "f16"
primTypeText F32 = "f32"
primTypeText F64 = "f64"
primTypeText Bool = "bool"

-- | The number of bytes taken up by a single element of this type.
primTypeBytes :: PrimType -> Int
primTypeBytes I8 = 1
primTypeBytes I16 = 2
primTypeBytes I32 = 4
primTypeBytes I64 = 8
primTypeBytes U8 = 1
primTypeBytes U16 = 2
primTypeBytes U32 = 4
primTypeBytes U64 = 8
primTypeBytes F16 = 2
primTypeBytes F32 = 4
primTypeBytes F64 = 8
primTypeBytes Bool = 1

-- | The type of a simple Futhark value, comprising a shape and an
-- element type.
data ValueType = ValueType [Int] PrimType
  deriving (Eq, Ord, Show)

-- | Prettyprint a value type as a strict text.
valueTypeText :: ValueType -> T.Text
valueTypeText (ValueType ds t) = mconcat (map pprDim ds) <> primTypeText t
  where
    pprDim d = "[" <> T.pack (show d) <> "]"

-- | Prettyprint a value type with empty dimensions as a strict text.
-- This is needed for Futhark server programs, whose types are
-- un-sized.
valueTypeTextNoDims :: ValueType -> T.Text
valueTypeTextNoDims (ValueType dims t) =
  mconcat (replicate (length dims) "[]") <> primTypeText t

-- | Get the type of a value.
valueType :: Value -> ValueType
valueType v = ValueType (valueShape v) $ valueElemType v

-- | Get the element type of a value.
valueElemType :: Value -> PrimType
valueElemType I8Value {} = I8
valueElemType I16Value {} = I16
valueElemType I32Value {} = I32
valueElemType I64Value {} = I64
valueElemType U8Value {} = U8
valueElemType U16Value {} = U16
valueElemType U32Value {} = U32
valueElemType U64Value {} = U64
valueElemType F16Value {} = F16
valueElemType F32Value {} = F32
valueElemType F64Value {} = F64
valueElemType BoolValue {} = Bool

-- | The shape of a value.  Empty list in case of a scalar.
valueShape :: Value -> [Int]
valueShape (I8Value shape _) = SVec.toList shape
valueShape (I16Value shape _) = SVec.toList shape
valueShape (I32Value shape _) = SVec.toList shape
valueShape (I64Value shape _) = SVec.toList shape
valueShape (U8Value shape _) = SVec.toList shape
valueShape (U16Value shape _) = SVec.toList shape
valueShape (U32Value shape _) = SVec.toList shape
valueShape (U64Value shape _) = SVec.toList shape
valueShape (F16Value shape _) = SVec.toList shape
valueShape (F32Value shape _) = SVec.toList shape
valueShape (F64Value shape _) = SVec.toList shape
valueShape (BoolValue shape _) = SVec.toList shape

-- Conversions

-- | Produce a list of the immediate elements of the value.  That is,
-- a 2D array will produce a list of 1D values.  A zero-dimensional
-- value will produce an empty list.  While lists are of course
-- inefficient, the actual values are just slices of the original
-- value, which makes them fairly space-efficient (but beware space
-- leaks).
valueElems :: Value -> [Value]
valueElems v
  | n : ns <- valueShape v =
    let k = product ns
        slices mk vs =
          [ mk (SVec.fromList ns) $
              SVec.slice (k * i) k vs
            | i <- [0 .. n -1]
          ]
     in case v of
          I8Value _ vs -> slices I8Value vs
          I16Value _ vs -> slices I16Value vs
          I32Value _ vs -> slices I32Value vs
          I64Value _ vs -> slices I64Value vs
          U8Value _ vs -> slices U8Value vs
          U16Value _ vs -> slices U16Value vs
          U32Value _ vs -> slices U32Value vs
          U64Value _ vs -> slices U64Value vs
          F16Value _ vs -> slices F16Value vs
          F32Value _ vs -> slices F32Value vs
          F64Value _ vs -> slices F64Value vs
          BoolValue _ vs -> slices BoolValue vs
  | otherwise =
    []

-- | A class for Haskell values that can be retrieved from 'Value'.
-- This is a convenience facility - don't expect it to be fast.
class GetValue t where
  getValue :: Value -> Maybe t

instance GetValue t => GetValue [t] where
  getValue v
    | null $ valueShape v = Nothing
    | otherwise = mapM getValue $ valueElems v

instance GetValue Bool where
  getValue (BoolValue shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Int8 where
  getValue (I8Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Int16 where
  getValue (I16Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Int32 where
  getValue (I32Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Int64 where
  getValue (I64Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Word8 where
  getValue (U8Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Word16 where
  getValue (U16Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Word32 where
  getValue (U32Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

instance GetValue Word64 where
  getValue (U64Value shape vs)
    | [] <- SVec.toList shape =
      Just $ vs SVec.! 0
  getValue _ = Nothing

-- | A class for Haskell values that can be converted to 'Value'.
-- This is a convenience facility - don't expect it to be fast.
class PutValue t where
  -- | This may fail for cases such as irregular arrays.
  putValue :: t -> Maybe Value

instance PutValue Int8 where
  putValue = Just . putValue1

instance PutValue Int16 where
  putValue = Just . putValue1

instance PutValue Int32 where
  putValue = Just . putValue1

instance PutValue Int64 where
  putValue = Just . putValue1

instance PutValue Word8 where
  putValue = Just . putValue1

instance PutValue Word16 where
  putValue = Just . putValue1

instance PutValue Word32 where
  putValue = Just . putValue1

instance PutValue Word64 where
  putValue = Just . putValue1

instance PutValue [Value] where
  putValue [] = Nothing
  putValue (x : xs) = do
    let res_shape = SVec.fromList $ length (x : xs) : valueShape x
    guard $ all ((== valueType x) . valueType) xs
    Just $ case x of
      I8Value {} -> I8Value res_shape $ foldMap getVec (x : xs)
      I16Value {} -> I16Value res_shape $ foldMap getVec (x : xs)
      I32Value {} -> I32Value res_shape $ foldMap getVec (x : xs)
      I64Value {} -> I64Value res_shape $ foldMap getVec (x : xs)
      U8Value {} -> U8Value res_shape $ foldMap getVec (x : xs)
      U16Value {} -> U16Value res_shape $ foldMap getVec (x : xs)
      U32Value {} -> U32Value res_shape $ foldMap getVec (x : xs)
      U64Value {} -> U64Value res_shape $ foldMap getVec (x : xs)
      F16Value {} -> F16Value res_shape $ foldMap getVec (x : xs)
      F32Value {} -> F32Value res_shape $ foldMap getVec (x : xs)
      F64Value {} -> F64Value res_shape $ foldMap getVec (x : xs)
      BoolValue {} -> BoolValue res_shape $ foldMap getVec (x : xs)
    where
      getVec (I8Value _ vec) = SVec.unsafeCast vec
      getVec (I16Value _ vec) = SVec.unsafeCast vec
      getVec (I32Value _ vec) = SVec.unsafeCast vec
      getVec (I64Value _ vec) = SVec.unsafeCast vec
      getVec (U8Value _ vec) = SVec.unsafeCast vec
      getVec (U16Value _ vec) = SVec.unsafeCast vec
      getVec (U32Value _ vec) = SVec.unsafeCast vec
      getVec (U64Value _ vec) = SVec.unsafeCast vec
      getVec (F16Value _ vec) = SVec.unsafeCast vec
      getVec (F32Value _ vec) = SVec.unsafeCast vec
      getVec (F64Value _ vec) = SVec.unsafeCast vec
      getVec (BoolValue _ vec) = SVec.unsafeCast vec

instance PutValue T.Text where
  putValue = putValue . T.encodeUtf8

instance PutValue BS.ByteString where
  putValue bs =
    Just $ U8Value size $ byteStringToVector bs
    where
      size = SVec.fromList [fromIntegral (BS.length bs)]

-- | Like 'PutValue', but only for scalars - which means it cannot
-- fail.
class PutValue1 t where
  putValue1 :: t -> Value

instance PutValue1 Int8 where
  putValue1 = I8Value mempty . SVec.singleton

instance PutValue1 Int16 where
  putValue1 = I16Value mempty . SVec.singleton

instance PutValue1 Int32 where
  putValue1 = I32Value mempty . SVec.singleton

instance PutValue1 Int64 where
  putValue1 = I64Value mempty . SVec.singleton

instance PutValue1 Word8 where
  putValue1 = U8Value mempty . SVec.singleton

instance PutValue1 Word16 where
  putValue1 = U16Value mempty . SVec.singleton

instance PutValue1 Word32 where
  putValue1 = U32Value mempty . SVec.singleton

instance PutValue1 Word64 where
  putValue1 = U64Value mempty . SVec.singleton
