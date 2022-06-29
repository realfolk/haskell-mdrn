{-# LANGUAGE OverloadedStrings #-}

module MDRN.Lib.ByteString
    ( toHex
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Data.Word       (Word8)
import           Numeric         (showHex)

toHex :: BS.ByteString -> T.Text
toHex bs = T.pack $ '#' : 'x' : bytes bs
  where
    bytes bs
      | bs == BS.empty = ":00"
      | otherwise = BS.foldr (\w s -> showChar ':' . byteToHex w . s) id bs ""

byteToHex :: Word8 -> ShowS
byteToHex w
  | w <= 15   = showChar '0' . showHex w
  | otherwise = showHex w
