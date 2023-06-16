{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Client.Request
    ( Request (..)
    ) where

import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (FromData (..))
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))

data Request payload
  = Request
      { getId      :: !Integer
      , getPayload :: !payload
      }
  deriving (Show)

-- Request format: (request 123 (add 1 2))

instance ToData a => ToData (Request a) where
  toData (Request id payload) =
    Data.list
      [ Data.symbol "request"
      , Data.integer id
      , toData payload
      ]

instance FromData a => FromData (Request a) where
  decoder = DL.list $ Request <$ DL.symbolEq "request" <*> DL.item' <*> DL.item'
