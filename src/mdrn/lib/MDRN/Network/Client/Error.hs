{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Client.Error
    ( Error (..)
    ) where

import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (FromData (..))
import qualified MDRN.Data.Decode      as Decode
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))

data Error = ResponseParseFailure | UnableToConnect deriving (Eq, Show)

instance ToData Error where
  toData ResponseParseFailure = Data.symbol "response-parse-failure"
  toData UnableToConnect      = Data.symbol "unable-to-connect"

instance FromData Error where
  decoder = Decode.oneOf $ map DL.list
    [ ResponseParseFailure <$ DL.item (DL.list $ DL.symbolEq "response-parse-failure")
    , UnableToConnect <$ DL.item (DL.list $ DL.symbolEq "unable-to-connect")
    ]
