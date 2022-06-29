{-# LANGUAGE OverloadedStrings #-}

module MDRN.Lib.Text
    ( escapeText
    ) where

import qualified Data.Text as T

escapeText :: T.Text -> T.Text
escapeText = T.concatMap escape

escape :: Char -> T.Text
escape c =
  case c of
    '\n' -> "\\n"
    '\t' -> "\\t"
    '\\' -> "\\\\"
    '\"' -> "\\\""
    _    -> T.singleton c
