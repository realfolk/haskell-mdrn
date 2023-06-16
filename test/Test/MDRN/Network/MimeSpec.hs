{-# LANGUAGE OverloadedStrings #-}

module Test.MDRN.Network.MimeSpec
    ( spec
    ) where

import           MDRN.Network.Mime
import           Test.Hspec

spec :: Spec
spec = decodeMimeTypeSpec

decodeMimeTypeSpec :: Spec
decodeMimeTypeSpec =
  describe "decodeMimeType" $ do
    describe "acceptable strings" $ do
      describe "application/mdrn" $ do
        it "returns Just TextEncoding" $ do
          decodeMimeType "application/mdrn" `shouldBe` Just TextEncoding

      describe "application/mdrn?encoding=text" $ do
        it "returns Just TextEncoding" $ do
          decodeMimeType "application/mdrn?encoding=text" `shouldBe` Just TextEncoding

      describe "application/mdrn?encoding=binary" $ do
        it "returns Just BinaryEncoding" $ do
          decodeMimeType "application/mdrn?encoding=binary" `shouldBe` Just BinaryEncoding

      describe "application/mdrn?encoding=binary256" $ do
        it "returns Just BinaryEncoding" $ do
          decodeMimeType "application/mdrn?encoding=binary256" `shouldBe` Just BinaryEncoding

    describe "unacceptable strings" $ do
      describe "empty string" $ do
        it "returns Nothing" $ do
          decodeMimeType "" `shouldBe` Nothing

      describe "application/html" $ do
        it "returns Nothing" $ do
          decodeMimeType "application/html" `shouldBe` Nothing
