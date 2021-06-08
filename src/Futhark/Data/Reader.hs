{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Trustworthy #-}

-- | The value reader can handle a delightful mix of binary and
-- textual input.  It is the most general way of reading values, but
-- it is less efficient than using the 'Get' instance if you know that
-- the data will be in the binary format.
module Futhark.Data.Reader
  ( readValues,
  )
where

import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isPrint, isSpace)
import qualified Data.Text as T
import Futhark.Data
import Futhark.Data.Parser
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

dropRestOfLine, dropSpaces :: LBS.ByteString -> LBS.ByteString
dropRestOfLine = LBS.drop 1 . LBS.dropWhile (/= '\n')
dropSpaces t = case LBS.dropWhile isSpace t of
  t'
    | "--" `LBS.isPrefixOf` t' -> dropSpaces $ dropRestOfLine t'
    | otherwise -> t'

readValue :: LBS.ByteString -> Maybe (Value, LBS.ByteString)
readValue full_t
  | Right (t', _, v) <- decodeOrFail full_t =
    Just (v, dropSpaces t')
  -- Some nasty hackery where we take the ASCII prefix of the
  -- bytestring, turn it into a Text, run the value parser, and
  -- prepend the remnant back.
  | otherwise = do
    let (a, b) = LBS.span (\c -> isSpace c || isPrint c) full_t
    case MP.parse
      ((,) <$> parseValue space <*> (MP.stateInput <$> MP.getParserState))
      ""
      (T.pack (LBS.unpack a)) of
      Right (v, a') -> Just (v, LBS.pack (T.unpack a') <> b)
      _ -> Nothing
  where
    space = MP.space *> MP.choice ["--" *> restOfLine *> space, pure ()]
    restOfLine = MP.takeWhileP Nothing (/= '\n') <* MP.choice [void MP.eol, MP.eof]

-- | Parse Futhark values from the given bytestring.
readValues :: LBS.ByteString -> Maybe [Value]
readValues = readValues' . dropSpaces
  where
    readValues' t
      | LBS.null t = Just []
      | otherwise = do
        (a, t') <- readValue t
        (a :) <$> readValues' t'
