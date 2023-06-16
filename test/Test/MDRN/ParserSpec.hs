{-# LANGUAGE OverloadedStrings #-}

module Test.MDRN.ParserSpec
    ( spec
    ) where

import qualified Data.ByteString     as BS
import           Data.Either         (isLeft)
import           MDRN.Data
import           MDRN.Data.Parser
import           Pouch.Math.Rational ((%))
import           Test.Hspec

spec :: Spec
spec = do
  primitiveSpec
  listSpec
  dotNotationSpec
  quotedSpec
  quotedShorthandSpec
  createListShorthandSpec

primitiveSpec :: Spec
primitiveSpec = do
  unitSpec
  booleanSpec
  integerSpec
  symbolSpec
  textSpec
  byteStringSpec
  rationalSpec

unitSpec :: Spec
unitSpec =
  describe "unit" $ do
    describe "#u" $ do
      it "returns unit" $ do
        "#u" ==> Node Unit


booleanSpec :: Spec
booleanSpec =
  describe "booleans" $ do
    describe "#f" $ do
      it "returns the boolean false" $ do
        "#f" ==> Node (Bool False)

    describe "#t" $ do
      it "returns the boolean true" $ do
        "#t" ==> Node (Bool True)

integerSpec :: Spec
integerSpec =
  describe "integers" $ do
    describe "-1" $ do
      it "returns the integer negative one" $ do
        "-1" ==> Node (Integer (-1))

    describe "0" $ do
      it "returns the integer zero" $ do
        "0" ==> Node (Integer 0)

    describe "1" $ do
      it "returns the integer one" $ do
        "1" ==> Node (Integer 1)

symbolSpec :: Spec
symbolSpec =
  describe "symbols" $ do
    describe "a" $ do
      it "returns the symbol a" $ do
        "a" ==> nodeSymbol "a"

    describe "AbC-dEf-123" $ do
      it "returns the symbol AbC-dEf-123" $ do
        "AbC-dEf-123" ==> nodeSymbol "AbC-dEf-123"


textSpec :: Spec
textSpec =
  describe "text" $ do
    describe "empty" $ do
      it "returns empty text" $ do
        "\"\"" ==> Node (Text "")

    describe "non-empty" $ do
      it "returns non-empty text" $ do
        "\"abc\"" ==> Node (Text "abc")

byteStringSpec :: Spec
byteStringSpec =
  describe "byte strings" $ do
    describe "empty bytes" $ do
      describe "#x" $ do
        it "returns empty byte string" $ do
          "#x" ==> Node (ByteString BS.empty)

    describe "non-empty bytes" $ do
      describe "#x:ff" $ do
        it "returns bytes FF" $ do
          "#x:ff" ==> Node (ByteString (BS.pack [0xFF]))

      describe "#x:FF" $ do
        it "returns bytes FF" $ do
          "#x:FF" ==> Node (ByteString (BS.pack [0xFF]))

      describe "#x:12:34:56:78:9a:bc:de:f0" $ do
        it "returns bytes 12 34 56 78 9A BC DE F0" $ do
          "#x:12:34:56:78:9a:bc:de:f0" ==> Node (ByteString (BS.pack [0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0]))

    describe "bad byte string syntax" $ do
      xit "does't support the empty byte string" $ do
        hasParseError "#x"

      xit "doesn't support uppercase hexadecimal symbols" $ do
        hasParseError "#x:FF"

      it "doesn't allow a missing colon" $ do
        hasParseError "#xff"

      it "requires pairs of hexadecimal symbols" $ do
        hasParseError "#x:f"

rationalSpec :: Spec
rationalSpec =
  describe "rationals" $ do
    describe "rational fractions" $ do
      describe "1/2" $ do
        it "returns the rational 1/2" $ do
          let half = 1%2 :: Rational
          "1/2" ==> Node (Rational half)

      describe "-1/2" $ do
        it "returns the rational -1/2" $ do
          let negativeHalf = -1%2 :: Rational
          "-1/2" ==> Node (Rational negativeHalf)

      describe "1.5" $ do
        it "returns the rational 3/2" $ do
          let threeOverTwo = 3%2 :: Rational
          "1.5" ==> Node (Rational threeOverTwo)

      describe "-3.125" $ do
        it "returns the rational -25/8" $ do
          let negativeTwentyFiveOverEight = -25%8 :: Rational
          "-3.125" ==> Node (Rational negativeTwentyFiveOverEight)

listSpec :: Spec
listSpec =
  describe "lists" $ do
    describe "empty" $ do
      it "returns the empty list" $ do
        "()" ==> List []

    describe "singleton" $ do
      it "returns a list containing the symbol a" $ do
        "(a)" ==> List [ nodeSymbol "a" ]

    describe "more than 1" $ do
      it "returns a list containing the symbols a b c" $ do
        "(a b c)" ==> List [ nodeSymbol "a", nodeSymbol "b", nodeSymbol "c" ]

    describe "nested" $ do
      it "returns a nested list" $ do
        "((a) b (c (d e)))" ==>
          List
            [ List [ nodeSymbol "a" ]
            , nodeSymbol "b"
            , List
                [ nodeSymbol "c"
                , List
                    [ nodeSymbol "d"
                    , nodeSymbol "e"
                    ]
                ]
            ]

    describe "spaces after data before closing parenthesis" $ do
      it "ignores trailing spaces" $ do
        "(a )" ==> List [ nodeSymbol "a" ]

    describe "liberal use of spaces" $ do
      it "ignores irrelevant leading and trailing spaces" $ do
        " (  a   b    (     c      )       )        " ==>
          List
            [ nodeSymbol "a"
            , nodeSymbol "b"
            , List [ nodeSymbol "c" ]
            ]

dotNotationSpec :: Spec
dotNotationSpec =
  describe "dot notation" $ do
    describe "foo.bar" $ do
      it "returns (get foo (quote bar))" $ do
        "foo.bar" ==>
          List
            [ nodeSymbol "get"
            , nodeSymbol "foo"
            , List [ nodeSymbol "quote", nodeSymbol "bar" ]
            ]

    describe "foo.bar.baz" $ do
      it "returns (get (get foo (quote bar)) (quote baz))" $ do
        "foo.bar.baz" ==>
          List
            [ nodeSymbol "get"
            , List
                [ nodeSymbol "get"
                , nodeSymbol "foo"
                , List [ nodeSymbol "quote", nodeSymbol "bar" ]
                ]
            , List
                [ nodeSymbol "quote"
                , nodeSymbol "baz"
                ]
            ]

    describe "foo.bar.((fn [] ''baz)).bang" $ do
      it "returns (get (get (get foo (quote bar)) ((fn (quote ()) (quote (quote baz))))) (quote bang))" $ do
        "foo.bar.((fn [] ''baz)).bang" ==>
          List
            [ nodeSymbol "get"
            , List
                [ nodeSymbol "get"
                , List
                    [ nodeSymbol "get"
                    , nodeSymbol "foo"
                    , List [ nodeSymbol "quote", nodeSymbol "bar" ]
                    ]
                , List
                    [ List
                        [ nodeSymbol "fn"
                        , List [ nodeSymbol "quote", List [] ]
                        , List
                            [ nodeSymbol "quote"
                            , List [ nodeSymbol "quote", nodeSymbol "baz" ]
                            ]
                        ]
                    ]
                ]
            , List [ nodeSymbol "quote", nodeSymbol "bang" ]
            ]

    describe "().()" $ do
      it "returns (get () ())" $ do
        "().()" ==> List [ nodeSymbol "get", List [], List [] ]

quotedSpec :: Spec
quotedSpec =
  describe "quoted data" $ do
    describe "primitives" $ do
      describe "'#u" $ do
        it "returns the quoted unit" $ do
          "'#u" ==> List [ nodeSymbol "quote", Node Unit ]

      describe "'#t" $ do
        it "returns the quoted boolean true" $ do
          "'#t" ==> List [ nodeSymbol "quote", Node (Bool True) ]

      describe "'#f" $ do
        it "returns the quoted boolean false" $ do
          "'#f" ==> List [ nodeSymbol "quote", Node (Bool False) ]

      describe "'-5" $ do
        it "returns the quoted integer -5" $ do
          "'-5" ==> List [ nodeSymbol "quote", Node (Integer (-5)) ]

      describe "'abc" $ do
        it "returns the quoted symbol abc" $ do
          "'abc" ==> List [ nodeSymbol "quote", nodeSymbol "abc" ]

      describe "'\"abc\"" $ do
        it "returns the quoted text abc" $ do
          "'\"abc\"" ==> List [ nodeSymbol "quote", Node (Text "abc") ]

      describe "'1/2" $ do
        it "returns the quoted rational 1/2" $ do
          let half = 1%2 :: Rational
          "'1/2" ==> List [ nodeSymbol "quote", Node (Rational half) ]

      describe "'#x:ff" $ do
        it "returns the quoted byte string FF" $ do
          "'#x:ff" ==> List [ nodeSymbol "quote", Node (ByteString (BS.pack [ 0xFF ])) ]

    describe "lists" $ do
      describe "'()" $ do
        it "returns the quoted empty list" $ do
          "'()" ==> List [ nodeSymbol "quote", List [] ]

      describe "'(a b c)" $ do
        it "returns the quoted non-empty list (a b c)" $ do
          "'(a b c)" ==>
              List
                [ nodeSymbol "quote"
                , List [ nodeSymbol "a", nodeSymbol "b", nodeSymbol "c" ]
                ]

    describe "dot notation" $ do
      describe "'foo.bar" $ do
        it "returns (quote (get foo (quote bar)))" $ do
          "'foo.bar" ==>
            List
              [ nodeSymbol "quote"
              , List
                  [ nodeSymbol "get"
                  , nodeSymbol "foo"
                  , List [ nodeSymbol "quote", nodeSymbol "bar" ]
                  ]
              ]

quotedShorthandSpec :: Spec
quotedShorthandSpec =
  describe "quoted shorthand" $ do
    describe "[]" $ do
      it "returns (quote ())" $ do
        "[]" ==> List [ nodeSymbol "quote", List [] ]

    describe "[a 2]" $ do
      it "returns (quote (a 2))" $ do
        "[a 2]" ==>
          List
            [ nodeSymbol "quote"
            , List [ nodeSymbol "a", Node (Integer 2) ]
            ]

createListShorthandSpec :: Spec
createListShorthandSpec =
  describe "create list shorthand" $ do
    describe "{}" $ do
      it "returns ((get list (quote create)) (quote ()))" $ do
        "{}" ==>
          List
            [ List
                [ nodeSymbol "get"
                , nodeSymbol "list"
                , List [ nodeSymbol "quote", nodeSymbol "create" ]
                ]
            , List [ nodeSymbol "quote", List [] ]
            ]

    describe "{a 2}" $ do
      it "returns ((get list (quote create)) (quote (a 2)))" $ do
        "{a 2}" ==>
          List
            [ List
                [ nodeSymbol "get"
                , nodeSymbol "list"
                , List [ nodeSymbol "quote", nodeSymbol "create" ]
                ]
            , List
                [ nodeSymbol "quote"
                , List [ nodeSymbol "a", Node (Integer 2) ]
                ]
            ]

-- HELPERS

(==>) input output =
  parseData input `shouldBe` Right output

hasParseError =
  isLeft . parseData

nodeSymbol =
  Node . Symbol
