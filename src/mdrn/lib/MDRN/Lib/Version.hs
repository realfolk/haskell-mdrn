{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module MDRN.Lib.Version
    ( Adapt (..)
    , Version
    , decoder
    , downgrade
    , encode
    , mapGet
    , mapGetSymbol
    , toText
    , upgrade
    , version1
    , version2
    ) where

import           Data.Kind             (Type)
import qualified Data.Text             as T
import           MDRN.Data             (Data)
import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (fromData)
import qualified MDRN.Data.Decode      as Decode
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))

-- * Adapt

class Adapt a b where
  adapt :: a -> b

upgrade :: (Adapt v1 v2) => v1 -> v2
upgrade = adapt

downgrade :: (Adapt v2 v1) => v2 -> v1
downgrade = adapt

-- * VERSION

newtype Version
  = Version T.Text

version1 = Version "v1"

version2 = Version "v2"

toText :: Version -> T.Text
toText (Version v) = v

encode :: Version -> Data -> Data
encode v a =
  Data.list [ Data.symbol (toText v), a ]


decoder :: Version -> Decode.Decoder a -> Decode.Decoder a
decoder v a =
  DL.list $ DL.symbolEq (toText v) *> DL.item a


mapGet :: Version -> Data -> Data -> Data
mapGet v mapData keyData =
  Data.mapGet
    (Data.mapGet mapData keyData)
    (Data.quote (Data.symbol (toText v)))


mapGetSymbol :: Version -> T.Text -> T.Text -> Data
mapGetSymbol v mapSymbol keySymbol =
  Data.mapGet
    (Data.mapGetSymbol mapSymbol keySymbol)
    (Data.quote (Data.symbol (toText v)))
