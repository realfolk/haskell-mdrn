module MDRN.Network.Server.HTTP.Response
    ( Body
    , Headers
    , Response
    , Status
    , toWaiResponse
    ) where

import           Data.Binary.Builder             (fromLazyByteString)
import           MDRN.Network.Server.HTTP.Common (Body, Headers)
import qualified Network.HTTP.Types              as H
import qualified Network.Wai                     as Wai

type Response = (Status, Headers, Body)

type Status = H.Status

toWaiResponse :: Response -> Wai.Response
toWaiResponse (status, headers, body) =
  Wai.responseBuilder status headers (fromLazyByteString body)
