{-# LANGUAGE OverloadedStrings #-}

-- | Facilities for comparing values for equality.  While 'Eq'
-- instances are defined, these are not useful when NaNs are involved,
-- and do not *explain* the differences.
module Futhark.Data.Compare
  ( compareValues,
    compareSeveralValues,
    Tolerance (..),
    Mismatch,
  )
where

import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Vector.Storable as SVec
import Futhark.Data

-- | Two values differ in some way.  The 'Show' instance produces a
-- human-readable explanation.
data Mismatch
  = -- | The position the value number and a flat index
    -- into the array.
    PrimValueMismatch Int [Int] T.Text T.Text
  | ArrayShapeMismatch Int [Int] [Int]
  | TypeMismatch Int T.Text T.Text
  | ValueCountMismatch Int Int

showText :: Show a => a -> T.Text
showText = T.pack . show

-- | A human-readable description of how two values are not the same.
explainMismatch :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
explainMismatch i what got expected =
  "Value #" <> i <> ": expected " <> what <> expected <> ", got " <> got

instance Show Mismatch where
  show (PrimValueMismatch vi [] got expected) =
    T.unpack $ explainMismatch (showText vi) "" got expected
  show (PrimValueMismatch vi js got expected) =
    T.unpack $ explainMismatch (showText vi <> " index [" <> mconcat (intersperse "," (map showText js)) <> "]") "" got expected
  show (ArrayShapeMismatch i got expected) =
    T.unpack $ explainMismatch (showText i) "array of shape " (showText got) (showText expected)
  show (TypeMismatch i got expected) =
    T.unpack $ explainMismatch (showText i) "value of type " got expected
  show (ValueCountMismatch got expected) =
    T.unpack $ "Expected " <> showText expected <> " values, got " <> showText got

-- | The maximum relative tolerance used for comparing floating-point
-- results.  0.002 (0.2%) is a fine default if you have no particular
-- opinion.
newtype Tolerance = Tolerance Double
  deriving (Eq, Ord, Show)

toleranceFloat :: RealFloat a => Tolerance -> a
toleranceFloat (Tolerance x) = fromRational $ toRational x

-- | Compare two Futhark values for equality.
compareValues :: Tolerance -> Value -> Value -> [Mismatch]
compareValues tol = compareValue tol 0

-- | As 'compareValues', but compares several values.  The two lists
-- must have the same length.
compareSeveralValues :: Tolerance -> [Value] -> [Value] -> [Mismatch]
compareSeveralValues tol got expected
  | n /= m = [ValueCountMismatch n m]
  | otherwise = concat $ zipWith3 (compareValue tol) [0 ..] got expected
  where
    n = length got
    m = length expected

unflattenIndex :: [Int] -> Int -> [Int]
unflattenIndex = unflattenIndexFromSlices . drop 1 . sliceSizes
  where
    sliceSizes [] = [1]
    sliceSizes (n : ns) = product (n : ns) : sliceSizes ns
    unflattenIndexFromSlices [] _ = []
    unflattenIndexFromSlices (size : slices) i =
      (i `quot` size) : unflattenIndexFromSlices slices (i - (i `quot` size) * size)

compareValue :: Tolerance -> Int -> Value -> Value -> [Mismatch]
compareValue tol i got_v expected_v
  | valueShape got_v == valueShape expected_v =
    case (got_v, expected_v) of
      (I8Value _ got_vs, I8Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (I16Value _ got_vs, I16Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (I32Value _ got_vs, I32Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (I64Value _ got_vs, I64Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (U8Value _ got_vs, U8Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (U16Value _ got_vs, U16Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (U32Value _ got_vs, U32Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (U64Value _ got_vs, U64Value _ expected_vs) ->
        compareNum got_vs expected_vs
      (F16Value _ got_vs, F16Value _ expected_vs) ->
        compareFloat (tolerance (toleranceFloat tol) expected_vs) got_vs expected_vs
      (F32Value _ got_vs, F32Value _ expected_vs) ->
        compareFloat (tolerance (toleranceFloat tol) expected_vs) got_vs expected_vs
      (F64Value _ got_vs, F64Value _ expected_vs) ->
        compareFloat (tolerance (toleranceFloat tol) expected_vs) got_vs expected_vs
      (BoolValue _ got_vs, BoolValue _ expected_vs) ->
        compareGen compareBool got_vs expected_vs
      _ ->
        [TypeMismatch i (primTypeText $ valueElemType got_v) (primTypeText $ valueElemType expected_v)]
  | otherwise =
    [ArrayShapeMismatch i (valueShape got_v) (valueShape expected_v)]
  where
    unflatten = unflattenIndex (valueShape got_v)
    value :: Show a => a -> T.Text
    value = T.pack . show
    {-# INLINE compareGen #-}
    {-# INLINE compareNum #-}
    {-# INLINE compareFloat #-}
    {-# INLINE compareFloatElement #-}
    {-# INLINE compareElement #-}
    compareNum :: (SVec.Storable a, Eq a, Show a) => SVec.Vector a -> SVec.Vector a -> [Mismatch]
    compareNum = compareGen compareElement
    compareFloat :: (SVec.Storable a, RealFloat a, Show a) => a -> SVec.Vector a -> SVec.Vector a -> [Mismatch]
    compareFloat = compareGen . compareFloatElement

    compareGen cmp got expected =
      let l = SVec.length got
          check acc j
            | j < l =
              case cmp j (got SVec.! j) (expected SVec.! j) of
                Just mismatch ->
                  check (mismatch : acc) (j + 1)
                Nothing ->
                  check acc (j + 1)
            | otherwise =
              acc
       in reverse $ check [] 0

    compareElement :: (Show a, Eq a) => Int -> a -> a -> Maybe Mismatch
    compareElement j got expected
      | got == expected = Nothing
      | otherwise = Just $ PrimValueMismatch i (unflatten j) (value got) (value expected)

    compareFloatElement :: (Show a, RealFloat a) => a -> Int -> a -> a -> Maybe Mismatch
    compareFloatElement abstol j got expected
      | isNaN got,
        isNaN expected =
        Nothing
      | isInfinite got,
        isInfinite expected,
        signum got == signum expected =
        Nothing
      | abs (got - expected) <= abstol = Nothing
      | otherwise = Just $ PrimValueMismatch i (unflatten j) (value got) (value expected)

    compareBool j got expected
      | got == expected = Nothing
      | otherwise = Just $ PrimValueMismatch i (unflatten j) (value got) (value expected)

tolerance :: (RealFloat a, SVec.Storable a) => a -> Vector a -> a
tolerance tol = SVec.foldl tolerance' tol . SVec.filter (not . nanOrInf)
  where
    tolerance' t v = max t $ tol * v
    nanOrInf x = isInfinite x || isNaN x
