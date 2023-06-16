{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Server.Error
    ( Error (..)
    ) where

import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (FromData (..))
import qualified MDRN.Data.Decode      as Decode
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))
import           MDRN.Language.Expr    (RuntimeError)

data Error
  = ParseFailure
  | RuntimeError RuntimeError
  deriving (Eq, Show)

instance ToData Error where
  toData ParseFailure     = Data.symbol "parse-failure"
  toData (RuntimeError e) = Data.list [Data.symbol "runtime-error", toData e]

instance FromData Error where
  decoder = Decode.oneOf $ map DL.list
    [ ParseFailure <$ DL.item (DL.list $ DL.symbolEq "parse-failure")
    , RuntimeError <$ DL.item (DL.list $ DL.symbolEq "runtime-error") <*> DL.item'
    ]
