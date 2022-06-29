{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Client.Response
    ( Response (..)
    ) where

import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (FromData (..))
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))

data Response payload
  = Response
      { getId      :: !Integer
      , getPayload :: !payload
      }
  deriving (Show)

-- Response format: (response id payload)

instance ToData a => ToData (Response a) where
  toData (Response id payload) =
    Data.list
      [ Data.symbol "response"
      , Data.integer id
      , toData payload
      ]

instance FromData a => FromData (Response a) where
  decoder = DL.list $ Response <$ DL.symbolEq "response" <*> DL.item' <*> DL.item'
