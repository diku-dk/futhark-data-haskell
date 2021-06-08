{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec-based parser for 'Value's in the textual value format.
-- The difference between this and the reader defined in
-- "Futhark.Data.Reader" is that we don't try to handle both the
-- textual and binary format - only the former.  On the other hand,
-- this parser has (much) better error messages and can be easily used
-- by other parsers (like the ones for FutharkScript or test blocks).
module Futhark.Data.Parser
  ( parsePrimType,
    parseType,
    parsePrimValue,
    parseValue,
  )
where

import Control.Monad.Except
import Data.Functor
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as SVec
import Data.Void
import Futhark.Data
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
  ( binary,
    decimal,
    float,
    hexadecimal,
    signed,
  )

-- | Parse the name of a primitive type.  Does *not* consume any
-- trailing whitespace, nor does it permit any internal whitespace.
parsePrimType :: Parsec Void T.Text PrimType
parsePrimType =
  choice
    [ "i8" $> I8,
      "i16" $> I16,
      "i32" $> I32,
      "i64" $> I64,
      "u8" $> U8,
      "u16" $> U16,
      "u32" $> U32,
      "u64" $> U64,
      "f32" $> F32,
      "f64" $> F64,
      "bool" $> Bool
    ]

parseInteger :: Parsec Void T.Text Integer
parseInteger =
  signed (pure ()) $
    choice
      [ "0b" *> binary,
        "0x" *> hexadecimal,
        decimal
      ]

scalar :: SVec.Storable a => (Vector Int -> Vector a -> Value) -> a -> Value
scalar f x = f mempty (SVec.singleton x)

parseIntConst :: Parsec Void T.Text Value
parseIntConst = do
  x <- parseInteger
  notFollowedBy $ "f32" <|> "f64" <|> "." <|> "e"
  choice
    [ intV I8Value x "i8",
      intV I16Value x "i16",
      intV I32Value x "i32",
      intV I64Value x "i64",
      intV U8Value x "u8",
      intV U16Value x "u16",
      intV U32Value x "u32",
      intV U64Value x "u64",
      intV I32Value x ""
    ]
  where
    intV mk x suffix =
      suffix $> scalar mk (fromInteger x)

parseFloatConst :: Parsec Void T.Text Value
parseFloatConst =
  choice
    [ "f32.nan" $> scalar F32Value (0 / 0),
      "f64.nan" $> scalar F64Value (0 / 0),
      "f32.inf" $> scalar F32Value (1 / 0),
      "f64.inf" $> scalar F64Value (1 / 0),
      "-f32.inf" $> scalar F32Value (-1 / 0),
      "-f64.inf" $> scalar F64Value (-1 / 0),
      numeric
    ]
  where
    numeric = do
      x <-
        signed (pure ()) $ choice [try float, fromInteger <$> decimal]
      choice
        [ floatV F32Value x "f32",
          floatV F64Value x "f64",
          floatV F64Value x ""
        ]

    floatV mk x suffix =
      suffix $> scalar mk (realToFrac (x :: Double))

-- | Parse a primitive value.  Does *not* consume any trailing
-- whitespace, nor does it permit any internal whitespace.
parsePrimValue :: Parsec Void T.Text Value
parsePrimValue =
  choice
    [ try parseIntConst,
      parseFloatConst,
      "true" $> BoolValue mempty (SVec.singleton True),
      "false" $> BoolValue mempty (SVec.singleton False)
    ]

lexeme :: Parsec Void T.Text () -> Parsec Void T.Text a -> Parsec Void T.Text a
lexeme sep p = p <* sep

inBrackets :: Parsec Void T.Text () -> Parsec Void T.Text a -> Parsec Void T.Text a
inBrackets sep = between (lexeme sep "[") (lexeme sep "]")

-- | Parse a type.  Does *not* consume any trailing whitespace, nor
-- does it permit any internal whitespace.
parseType :: Parsec Void T.Text ValueType
parseType = ValueType <$> many parseDim <*> parsePrimType
  where
    parseDim = fromInteger <$> ("[" *> parseInteger <* "]")

parseEmpty :: Parsec Void T.Text Value
parseEmpty = do
  ValueType dims t <- parseType
  unless (product dims == 0) $ fail "Expected at least one empty dimension"
  pure $ case t of
    I8 -> I8Value (SVec.fromList dims) mempty
    I16 -> I16Value (SVec.fromList dims) mempty
    I32 -> I32Value (SVec.fromList dims) mempty
    I64 -> I64Value (SVec.fromList dims) mempty
    U8 -> U8Value (SVec.fromList dims) mempty
    U16 -> U16Value (SVec.fromList dims) mempty
    U32 -> U32Value (SVec.fromList dims) mempty
    U64 -> U64Value (SVec.fromList dims) mempty
    F32 -> F32Value (SVec.fromList dims) mempty
    F64 -> F64Value (SVec.fromList dims) mempty
    Bool -> BoolValue (SVec.fromList dims) mempty

-- | Parse a value, given a post-lexeme parser for whitespace.
parseValue :: Parsec Void T.Text () -> Parsec Void T.Text Value
parseValue sep =
  choice
    [ lexeme sep parsePrimValue,
      putValue' $ inBrackets sep (parseValue sep `sepBy` lexeme sep ","),
      lexeme sep $ "empty(" *> parseEmpty <* ")"
    ]
  where
    putValue' :: PutValue v => Parsec Void T.Text v -> Parsec Void T.Text Value
    putValue' p = do
      o <- getOffset
      x <- p
      case putValue x of
        Nothing ->
          parseError . FancyError o . S.singleton $
            ErrorFail "array is irregular or has elements of multiple types."
        Just v ->
          pure v
