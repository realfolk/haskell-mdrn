{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module MDRN.Data.Decode.Internal
    ( Decoder (..)
    , Error (..)
    , FromData (..)
    , bool
    , byteString
    , byteStringEq
    , decodeData
    , integer
    , list
    , oneOf
    , prim
    , rational
    , symbol
    , symbolEq
    , text
    , textEq
    , unit
    , when
    ) where

import           Data.Bifunctor  (first)
import qualified Data.ByteString as BS
import qualified Data.List       as List
import qualified Data.Text       as T
import           Lib.Text        (tshow)
import           MDRN.Data       (Data (..), Prim (..))
import           Prelude         hiding (drop, head, length, tail, take)

-- * Decoder

newtype Decoder a
  = Decoder { runDecoder :: Data -> Either Error a }

data Error
  = TypeMismatch
  | ParseError T.Text
  | MultipleErrors [Error]
  | Failure T.Text
  deriving (Eq, Show)

instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap f . d)

instance Applicative Decoder where
  pure = Decoder . const . pure
  Decoder f <*> Decoder d = Decoder ((<*>) <$> f <*> d)

instance Monad Decoder where
  Decoder g >>= f = Decoder (\d -> g d >>= (flip runDecoder d . f))

instance MonadFail Decoder where
  fail = Decoder . const . Left . Failure . T.pack

-- * FromData

class FromData a where
  decoder :: Decoder a


-- * Runners

decodeData :: Decoder a -> Data -> Either Error a
decodeData = runDecoder

-- * Basic decoders

prim :: Decoder Prim
prim = withPrim id

unit :: Decoder ()
unit = withPrim (\Unit -> ())

bool :: Decoder Bool
bool = withPrim (\(Bool b) -> b)

integer :: Decoder Integer
integer = withPrim (\(Integer i) -> i)

rational :: Decoder Rational
rational = withPrim (\(Rational r) -> r)

symbol :: Decoder T.Text
symbol = withPrim (\(Symbol s) -> s)

text :: Decoder T.Text
text = withPrim (\(Text t) -> t)

byteString :: Decoder BS.ByteString
byteString = withPrim (\(ByteString b) -> b)

withPrim :: (Prim -> a) -> Decoder a
withPrim f = Decoder $ \case
  Node prim -> Right $ f prim
  _         -> Left TypeMismatch

-- * List decoders

list :: Decoder a -> Decoder [a]
list (Decoder d) = withList (mapM d)

withList :: ([Data] -> Either Error a) -> Decoder a
withList f = Decoder $ \case
  List items -> f items
  _          -> Left TypeMismatch

-- * Equality decoders

symbolEq :: T.Text -> Decoder T.Text
symbolEq = flip when symbol . (==)

textEq :: T.Text -> Decoder T.Text
textEq = flip when text . (==)

byteStringEq :: BS.ByteString -> Decoder BS.ByteString
byteStringEq = flip when byteString . (==)

-- * Helpers

oneOf :: [Decoder a] -> Decoder a
oneOf decoders = Decoder (first flattenErrors . oneOfHelper decoders [])

oneOfHelper :: [Decoder a] -> [Error] -> Data -> Either [Error] a
oneOfHelper [] errors _ = Left (reverse errors)
oneOfHelper ((Decoder d) : ds) errors data' =
  case d data' of
    Right a -> Right a
    Left e  -> oneOfHelper ds (e : errors) data'

flattenErrors :: [Error] -> Error
flattenErrors = foldr combineErrors (MultipleErrors [])

combineErrors :: Error -> Error -> Error
combineErrors (MultipleErrors es1) (MultipleErrors es2) = MultipleErrors (es1 ++ es2)
combineErrors (MultipleErrors es) e = MultipleErrors (es ++ [e])
combineErrors e (MultipleErrors es) = MultipleErrors (e : es)
combineErrors e1 e2 = MultipleErrors [e1, e2]

when :: (a -> Bool) -> Decoder a -> Decoder a
when f decoder = decoder >>= (\a -> if f a then return a else fail "when: condition not satisfied")
