{-# LANGUAGE OverloadedStrings #-}

module MDRN.Prim
    ( Prim (..)
    , toText
    ) where

import qualified Data.Binary         as B
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Lib.Rational        as Rational
import           Lib.Text            (tshow)
import qualified Logger
import qualified MDRN.Lib.ByteString as BS
import qualified MDRN.Lib.Text       as T

data Prim
  = Unit
  | Bool !Bool
  | Integer !Integer
  | Rational !Rational
  | Symbol !T.Text
  | Text !T.Text
  | ByteString !BS.ByteString
  deriving (Eq, Ord, Show)

instance B.Binary Prim where
  put a =
    case a of
      Unit          -> B.putWord8 0
      Bool b        -> B.putWord8 1 <> B.put b
      Integer i     -> B.putWord8 2 <> B.put i
      Rational r    -> B.putWord8 3 <> B.put r
      Symbol s      -> B.putWord8 4 <> B.put s
      Text t        -> B.putWord8 5 <> B.put t
      ByteString bs -> B.putWord8 6 <> B.put bs

  get = do
    tag <- B.getWord8
    case tag of
      0 -> return Unit
      1 -> Bool <$> B.get
      2 -> Integer <$> B.get
      3 -> Rational <$> B.get
      4 -> Symbol <$> B.get
      5 -> Text <$> B.get
      6 -> ByteString <$> B.get
      _ -> fail $ "Unsupported tag: " ++ show tag

instance Logger.ToRecord Prim where
  toRecord = Logger.Plain . toText

toText :: Prim -> T.Text
toText p =
  case p of
    Unit          -> "#u"
    Bool True     -> "#t"
    Bool False    -> "#f"
    Integer i     -> tshow i
    Rational r    -> Rational.toText r
    Symbol s      -> s
    Text t        -> "\"" <> T.escapeText t <> "\""
    ByteString bs -> BS.toHex bs
