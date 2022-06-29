{-# LANGUAGE OverloadedStrings #-}

module MDRN.Data
    ( Data (..)
    , Prim (..)
    , bool
    , byteString
    , float
    , int
    , integer
    , list
    , mapGet
    , mapGetSymbol
    , node
    , quote
    , rational
    , symbol
    , text
    , toByteString
    , toEscapedText
    , toText
    , toUTF8ByteString
    , unit
    ) where

import qualified Data.Binary          as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Logger
import qualified MDRN.Lib.Text        as T
import           MDRN.Prim            (Prim (..))
import qualified MDRN.Prim            as Prim

data Data
  = Node Prim
  | List ![Data]
  deriving (Eq, Show)

-- * Constructors

node :: Prim -> Data
node = Node

unit :: Data
unit = node Unit

bool :: Bool -> Data
bool = node . Bool

integer :: Integer -> Data
integer = node . Integer

int :: Int -> Data
int = integer . toInteger

rational :: Rational -> Data
rational = node . Rational

float :: Float -> Data
float = rational . toRational

symbol :: T.Text -> Data
symbol = node . Symbol

text :: T.Text -> Data
text = node . Text

byteString :: BS.ByteString -> Data
byteString = node . ByteString

list :: [Data] -> Data
list = List

-- ** Special constructors

quote :: Data -> Data
quote data' =
  list [ symbol "quote", data' ]

mapGet :: Data -> Data -> Data
mapGet mapData keyData =
  list [ symbol "get", mapData, keyData ]

mapGetSymbol :: T.Text -> T.Text -> Data
mapGetSymbol mapSymbol keySymbol =
  mapGet (symbol mapSymbol) (quote (symbol keySymbol))

-- * Instances

instance B.Binary Data where
  put (Node p) = B.putWord8 0 <> B.put p
  put (List l) = B.putWord8 1 <> B.put l

  get = do
    tag <- B.getWord8
    case tag of
      0 -> Node <$> B.get
      1 -> List <$> B.get
      _ -> fail $ "Unsupported tag: " ++ show tag

instance Logger.ToRecord Data where
  toRecord = Logger.Plain . toText

-- * Converters

toEscapedText :: Data -> T.Text
toEscapedText = T.escapeText . toText

toText :: Data -> T.Text
toText (Node p) = Prim.toText p
toText (List l) = "(" <> T.unwords (map toText l) <> ")"

toUTF8ByteString :: Data -> LBS.ByteString
toUTF8ByteString = LBS.fromStrict . TE.encodeUtf8 . toText

toByteString :: Data -> LBS.ByteString
toByteString = B.encode
