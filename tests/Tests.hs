{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Binary (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int
import qualified Data.Text as T
import qualified Data.Vector.Storable as SVec
import Data.Word
import Futhark.Data
import Futhark.Data.Compare
import Futhark.Data.Parser
import Futhark.Data.Reader
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char (space)

-- A hack to get around the default Eq instance for values, which does
-- not handle NaNs the way we need.
newtype TestValue = TestValue {unTestValue :: Value}

instance Eq TestValue where
  TestValue x == TestValue y =
    null $ compareValues (Tolerance 0) x y

instance Show TestValue where
  show (TestValue x) = show x

instance Arbitrary TestValue where
  arbitrary =
    TestValue
      <$> oneof
        [ mk I8Value,
          mk I16Value,
          mk I32Value,
          mk I64Value,
          mk U8Value,
          mk U16Value,
          mk U32Value,
          mk U64Value,
          mk BoolValue
        ]
    where
      mk f = do
        -- Careful not to create enormous arrays.
        shape <- listOf $ choose (0, 3)
        f (SVec.fromList shape) . SVec.fromList <$> replicateM (product shape) arbitrary

scalar :: SVec.Storable a => (Vector Int -> Vector a -> Value) -> a -> Value
scalar f x = f mempty (SVec.singleton x)

readerTests :: TestTree
readerTests =
  testGroup
    "Reader"
    [ test "1" [scalar I32Value 1],
      test "2i32" [scalar I32Value 2],
      test "3i64" [scalar I64Value 3],
      test "[1, 2, 3]" [I32Value (SVec.fromList [3]) (SVec.fromList [1, 2, 3])],
      test
        "2i32 [1, 2, 3]"
        [ scalar I32Value 2,
          I32Value (SVec.fromList [3]) (SVec.fromList [1, 2, 3])
        ],
      test
        "[[1,-- comment\n 2], [3,4]]"
        [I32Value (SVec.fromList [2, 2]) (SVec.fromList [1, 2, 3, 4])],
      test
        "b\STX\SOH i32\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\154\238\179u"
        [I32Value (SVec.fromList [1]) (SVec.fromList [1974726298])],
      test
        "0.9597767951851629f64\nb\STX\SOH i32\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC1\ETX\NUL\NUL2"
        [ scalar F64Value 0.9597767951851629,
          I32Value (SVec.fromList [1]) (SVec.fromList [785]),
          scalar I32Value 2
        ],
      testProperty "read random binary data" $
        \v ->
          (map TestValue <$> readValues (encode $ unTestValue v))
            == Just [v],
      testProperty "read random text data" $
        \v ->
          (map TestValue <$> readValues (LBS.pack $ T.unpack $ valueText $ unTestValue v))
            == Just [v]
    ]
  where
    maxlen = 40

    shorten s
      | length s < maxlen = s
      | otherwise = take (maxlen - 3) s <> "..."

    test s x =
      testCase ("Reading " <> shorten (show s)) $
        readValues s @?= Just x

parserTests :: TestTree
parserTests =
  testGroup
    "Parser"
    [ test "1" $ scalar I32Value 1,
      negtest "_1",
      test "2i32" $ scalar I32Value 2,
      test "3i64" $ scalar I64Value 3,
      test "-2_3i32" $ scalar I32Value (-23),
      test "0b1_0_0_1" $ scalar I32Value 9,
      test "0x12_34" $ scalar I32Value 0x1234,
      test "3.1_4" $ scalar F64Value 3.14,
      test "1.0" $ scalar F64Value 1,
      negtest "_1.0",
      test "2f32" $ scalar F32Value 2,
      test "2f16" $ scalar F16Value 2,
      test "3.0f64" $ scalar F64Value 3.0,
      test "3.1f64" $ scalar F64Value 3.1,
      test "3.1_e-2f64" $ scalar F64Value 3.1e-2,
      test "f32.nan" $ scalar F32Value (0 / 0),
      test "f16.nan" $ scalar F16Value (0 / 0),
      test "f64.nan" $ scalar F64Value (0 / 0),
      test "f64.inf" $ scalar F64Value (1 / 0),
      test "-f64.inf" $ scalar F64Value (-1 / 0),
      test "true" $ scalar BoolValue True,
      test "false" $ scalar BoolValue False,
      negtest "tr_ue",
      testProperty "parse random data" $
        \v ->
          (TestValue <$> parseMaybe (parseValue space) (valueText $ unTestValue v))
            == Just v
    ]
  where
    test s x =
      testCase ("Parsing " <> show s) $
        (TestValue <$> runParser (parseValue space <* eof) "" s) @?= Right (TestValue x)
    negtest s =
      testCase ("Parsing " <> show s) $
        either
          (const Nothing)
          Just
          (TestValue <$> runParser (parseValue space <* eof) "" s)
          @?= Nothing

getValueTests :: TestTree
getValueTests =
  testGroup
    "GetValue"
    [ test (putValue1 (1 :: Int32)) (Nothing :: Maybe [Word8]),
      test (putValue1 (1 :: Int32)) (Just (1 :: Int32))
    ]
  where
    test v expected =
      testCase (unwords ["getValue", v', "==", show expected]) $
        getValue v @?= expected
      where
        v' = T.unpack (valueText v)

allTests :: TestTree
allTests =
  testGroup "" [readerTests, parserTests, getValueTests]

main :: IO ()
main = defaultMain allTests
