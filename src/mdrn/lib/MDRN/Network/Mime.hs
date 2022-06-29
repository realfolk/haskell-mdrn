{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Mime
    ( Encoding (..)
    , binaryMimeType
    , decodeMimeType
    , encodeMimeType
    , textMimeType
    ) where

import qualified Data.Text   as T
import qualified Text.Parsec as P

-- * MIME Types
-- | A type representation of available encoding formats.
data Encoding = TextEncoding | BinaryEncoding deriving (Eq, Show)

-- | The MIME type for a text-encoded MDRN data.
textMimeType = "application/mdrn?encoding=text"

-- | The MIME type for a binary-encoded MDRN data.
binaryMimeType = "application/mdrn?encoding=binary"

-- | Encode a 'T.Text' MIME type from an 'Encoding'.
encodeMimeType :: Encoding -> T.Text
encodeMimeType encoding =
  case encoding of
    TextEncoding   -> textMimeType
    BinaryEncoding -> binaryMimeType

-- | Decode an 'Encoding' from 'T.Text'.
decodeMimeType :: T.Text -> Maybe Encoding
decodeMimeType t = either (const Nothing) Just encoding
  where
    encoding = P.parse parser "MDRN Encoding MIME Type" t
    parser = P.string "application/mdrn" *> textOrBinary
    textOrBinary = P.option TextEncoding $ P.try binary
    binary = BinaryEncoding <$ P.string "?encoding=binary"
