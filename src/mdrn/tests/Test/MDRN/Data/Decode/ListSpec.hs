{-# LANGUAGE OverloadedStrings #-}

module Test.MDRN.Data.Decode.ListSpec
    ( spec
    ) where

import qualified Data.Text             as T
import           MDRN.Data.Decode
import qualified MDRN.Data.Decode.List as DL
import           Test.Hspec

spec :: Spec
spec =
  describe "advanced list decoders" $ do
    describe "list" $ do
      describe "(add 1 2)" $ do
        it "works" $ do
          let decoder = DL.list $ (+) <$ DL.symbolEq "add" <*> DL.integer <*> DL.integer
          decodeTextCustom decoder "(add 1 2)" `shouldBe` Right 3
    describe "applicative examples" $ do
      it "works" $ do
        let decoder = DL.list $ (+5) <$> DL.integer
        decodeTextCustom decoder "(1)" `shouldBe` Right 6
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 6
        decodeTextCustom decoder "(1 2 3)" `shouldBe` Right 6

        decodeTextCustom decoder "()" `shouldBe` Left (Failure "List is too short")
        decodeTextCustom decoder "1" `shouldBe` Left TypeMismatch

        let decoder = DL.list $ 10 <$ DL.integer
        decodeTextCustom decoder "(1)" `shouldBe` Right 10
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 10
        decodeTextCustom decoder "(1 2 3)" `shouldBe` Right 10

        decodeTextCustom decoder "()" `shouldBe` Left (Failure "List is too short")
        decodeTextCustom decoder "1" `shouldBe` Left TypeMismatch

        let decoder = DL.list $ (+) <$> DL.integer <*> DL.integer
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 3
        decodeTextCustom decoder "(1 2 3)" `shouldBe` Right 3

        decodeTextCustom decoder "(1)" `shouldBe` Left (Failure "List is too short")
        decodeTextCustom decoder "()" `shouldBe` Left (Failure "List is too short")
        decodeTextCustom decoder "1" `shouldBe` Left TypeMismatch

        let decoder = DL.list $ DL.integer *> DL.integer
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 2

        let decoder = DL.list $ DL.integer *> DL.integer *> DL.integer
        decodeTextCustom decoder "(1 2 3)" `shouldBe` Right 3

        let decoder = DL.list $ (+5) <$> DL.integer *> DL.integer
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 2

        let decoder = DL.list $ (+5) <$> (DL.integer *> DL.integer)
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 7

        let decoder = DL.list $ (+5) <$> DL.integer <* DL.integer
        decodeTextCustom decoder "(1 2)" `shouldBe` Right 6

        let sum4 a b c d = a + b + c + d
        let decoder = DL.list $ sum4 <$> DL.integer <*> DL.integer <*> DL.integer <*> DL.integer <* DL.end
        decodeTextCustom decoder "(1 2 3 4)" `shouldBe` Right 10

        decodeTextCustom decoder "(1 2 3 4 5)" `shouldBe` Left (Failure "List is too long")
        decodeTextCustom decoder "(1 2 3)" `shouldBe` Left (Failure "List is too short")

        -- You can't match past the end of the list
        let decoder = DL.list $ (+) <$> DL.integer <* DL.end <*> DL.integer
        decodeTextCustom decoder "(1 2)" `shouldBe` Left (Failure "List is too long")
    describe "monad examples" $ do
      it "works" $ do
        decodeTextCustom fnCallDecoder "(add 3 1 5 4 2)" `shouldBe` Right 15
        decodeTextCustom fnCallDecoder "(min 3 1 5 4 2)" `shouldBe` Right 1
        decodeTextCustom fnCallDecoder "(max 3 1 5 4 2)" `shouldBe` Right 5
        decodeTextCustom fnCallDecoder "(abc 3 1 5 4 2)" `shouldBe` Right 0
    describe "PostMDRN example" $ do
      it "works" $ do
        let paragraph = "(paragraph (text \"a\") (link \"b\" \"c\"))"
        let result = Paragraph [ Text "a", Link "b" "c" ]

        decodeTextCustom paragraphDecoder paragraph `shouldBe` Right result


-- HELPERS

fnCallDecoder :: Decoder Integer
fnCallDecoder = DL.list $ do
  f <- DL.item symbol
  rest <- DL.rest'
  return $
    case f of
      "add" -> sum rest
      "min" -> minimum rest
      "max" -> maximum rest
      _     -> 0

-- For the POSTMDRN example

newtype Paragraph
  = Paragraph [Item]
  deriving (Eq, Show)

data Item
  = Text T.Text
  | Link T.Text T.Text
  deriving (Eq, Show)

instance FromData Item where
  decoder = oneOf $ map DL.list
    [ Text <$ DL.symbolEq "text" <*> DL.item' <* DL.end
    , Link <$ DL.symbolEq "link" <*> DL.item' <*> DL.item' <* DL.end
    ]

paragraphDecoder :: Decoder Paragraph
paragraphDecoder =
  DL.list $ Paragraph <$> (DL.symbolEq "paragraph" >> DL.rest')
