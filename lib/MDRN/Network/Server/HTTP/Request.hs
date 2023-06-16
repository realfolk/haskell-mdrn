{-# LANGUAGE TupleSections #-}

module MDRN.Network.Server.HTTP.Request
    ( Body
    , Headers
    , Method
    , Path
    , Query
    , QueryItem
    , Request
    , fromWaiRequest
    ) where

import           Pouch.URL.Component.Path          (Path)
import qualified Pouch.URL.Component.Path          as Path
import           Pouch.URL.Component.Query         (Query)
import qualified Pouch.URL.Component.Query         as Query
import qualified Pouch.URL.Component.Query.Item    as QueryItem
import           MDRN.Network.Server.HTTP.Common (Body, Headers)
import qualified Network.HTTP.Types              as H
import qualified Network.Wai                     as Wai

type Request = (Method, Path, Query, Headers, Body)

type QueryItem = QueryItem.Item

type Method = H.Method

fromWaiRequest :: Wai.Request -> IO Request
fromWaiRequest req =
  (method, path, query, headers, ) <$> body
  where
    method = Wai.requestMethod req
    path = Path.fromWaiRequest req
    query = Query.fromWaiRequest req
    headers = Wai.requestHeaders req
    body = Wai.lazyRequestBody req
