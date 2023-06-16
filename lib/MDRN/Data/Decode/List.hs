{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module MDRN.Data.Decode.List
    ( Decoder
    , end
    , integer
    , item
    , item'
    , list
    , rest
    , rest'
    , symbol
    , symbolEq
    ) where

import qualified Data.Text                 as T
import           MDRN.Data                 (Data (..))
import           MDRN.Data.Decode.Internal (Error (..), FromData (..))
import qualified MDRN.Data.Decode.Internal as Decode
import           Prelude                   hiding (any)

newtype Decoder a
  = Decoder { runDecoder :: [Data] -> (Either Error a, [Data]) }

instance Functor Decoder where
  fmap f (Decoder d) = Decoder $ \items ->
    case d items of
      (Left e, items')  -> (Left e, items')
      (Right a, items') -> (Right (f a), items')

instance Applicative Decoder where
  pure a = Decoder (Right a, )
  (Decoder dab) <*> (Decoder d) = Decoder $ \items ->
    case dab items of
      (Left e, items') -> (Left e, items')
      (Right f, items') ->
        case d items' of
          (Left e, items'')  -> (Left e, items'')
          (Right a, items'') -> (Right (f a), items'')

instance Monad Decoder where
  (Decoder da) >>= f = Decoder $ \items ->
    case da items of
      (Left e, items')  -> (Left e, items')
      (Right a, items') -> runDecoder (f a) items'

list :: Decoder a -> Decode.Decoder a
list (Decoder ld) = Decode.Decoder $ \case
  List items -> fst $ ld items
  _          -> Left TypeMismatch

-- ABBREVIATIONS

integer :: Decoder Integer
integer =
  item Decode.integer

symbol :: Decoder T.Text
symbol =
  item Decode.symbol

symbolEq :: T.Text -> Decoder T.Text
symbolEq =
  item . Decode.symbolEq

-- LIST

item :: Decode.Decoder a -> Decoder a
item (Decode.Decoder d) = Decoder $ \case
  (x:rest) -> (d x, rest)
  xs       -> (Left $ Failure "List is too short", xs)

item' :: FromData a => Decoder a
item' =
  item decoder

rest :: Decode.Decoder a -> Decoder [a]
rest (Decode.Decoder d) = Decoder $ (, []) . mapM d

rest' :: FromData a => Decoder [a]
rest' =
  rest decoder

end :: Decoder ()
end = Decoder $ \case
  [] -> (Right (), [])
  _  -> (Left $ Failure "List is too long", [])
