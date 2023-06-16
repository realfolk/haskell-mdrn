{-# LANGUAGE OverloadedStrings #-}

module Test.MDRN.Lib.ByteStringSpec
    ( spec
    ) where

import qualified Data.ByteString     as BS
import           MDRN.Lib.ByteString (toHex)
import           Test.Hspec

spec :: Spec
spec =
  toHexSpec

toHexSpec :: Spec
toHexSpec =
  describe "toHex" $ do
    describe "empty bytes" $ do
      it "returns #x:00" $ do
        toHex BS.empty `shouldBe` "#x:00"

    describe "bytes 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" $ do
      it "returns #x:00:01:02:03:04:05:06:07:08:09:0a:0b:0c:0d:0e:0f" $ do
        toHex (BS.pack [0..15]) `shouldBe` "#x:00:01:02:03:04:05:06:07:08:09:0a:0b:0c:0d:0e:0f"

    describe "bytes 16 32 64 128 255" $ do
      it "returns #x:10:20:40:80:ff" $ do
        toHex (BS.pack [16, 32, 64, 128, 255]) `shouldBe` "#x:10:20:40:80:ff"

    describe "bytes 0" $ do
      it "returns #x:00" $ do
        toHex (BS.singleton 0) `shouldBe` "#x:00"

    describe "bytes 33" $ do
      it "returns #x:21" $ do
        toHex (BS.singleton 33) `shouldBe` "#x:21"
