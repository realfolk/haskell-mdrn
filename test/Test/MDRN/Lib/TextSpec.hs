{-# LANGUAGE OverloadedStrings #-}

module Test.MDRN.Lib.TextSpec
    ( spec
    ) where

import           MDRN.Lib.Text (escapeText)
import           Test.Hspec

spec :: Spec
spec =
  escapeTextSpec

escapeTextSpec :: Spec
escapeTextSpec =
  describe "escapeText" $ do
    it "escapes any combination of \\n, \\t, \\\\, or \\\"" $ do
      escapeText "a\nbc\tde \\ \" \r" `shouldBe` "a\\nbc\\tde \\\\ \\\" \r"
