{-# LANGUAGE OverloadedStrings #-}

{-|
Module: MDRN.Network.Client.HTTP
Description: A client for sending remote procedure calls to a server.
Copyright: (c) Real Folk Inc. 2022
Maintainer: admin@realfolk.io
Stability: experimental
Portability: POSIX

A client for sending remote procedure calls to a server.
-}

module MDRN.Network.Client.HTTP
    ( Client
    , fromAddr
    , fromURI
    , http
    ) where

import           Control.Exception            (tryJust)
import           Data.Bifunctor               (first)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified MDRN.Data                    as Data
import           MDRN.Data.Decode             (FromData (..))
import qualified MDRN.Data.Decode             as Decode
import           MDRN.Data.Encode             (ToData (..))
import           MDRN.Language.Expr
import qualified MDRN.Network.Client.Error    as CE
import           MDRN.Network.Client.Request  (Request (..))
import qualified MDRN.Network.Client.Response as Response
import           MDRN.Network.Mime
import           Network.Connection           (TLSSettings (TLSSettingsSimple))
import qualified Network.HTTP.Client          as HC
import qualified Network.HTTP.Client.TLS      as HCTLS
import qualified Network.HTTP.Types           as H
import qualified Network.URI                  as URI

type Client = Data -> IO (Either Data Data)

-- * Constructors

http :: Encoding -> HC.Manager -> URI.URI -> Client
http encoding manager uri msg =
  runRequestSafely encoding manager uri (makeRPCRequest msg) >>= (return . handleResult) . first toData
  where
    makeRPCRequest msg = toData $ Request 1 msg
    handleResult result = do
      response <- result
      Response.getPayload <$> first (const $ toData CE.ResponseParseFailure) (decode encoding (HC.responseBody response))

decode :: FromData a => Encoding -> LBS.ByteString -> Either Decode.Error a
decode TextEncoding   = Decode.decodeText . TE.decodeUtf8 . LBS.toStrict
decode BinaryEncoding = Decode.decodeByteString

-- | Given a URI, it uses the scheme (http/https) to construct and return the appropriate client.
fromURI :: URI.URI -> IO (Maybe Client)
fromURI uri =
  case URI.uriScheme uri of
    "http:" -> do
      manager <- makeManagerUnsafe
      return $ Just $ http BinaryEncoding manager uri
    "https:" -> do
      manager <- makeManager
      return $ Just $ http BinaryEncoding manager uri
    _ -> return Nothing

-- | Given an address, it tries to convert it to a URI and return the appropriate client.
fromAddr :: T.Text -> IO (Maybe Client)
fromAddr addr =
  case URI.parseURI $ T.unpack addr of
    Just uri ->
      fromURI uri

    Nothing ->
      return Nothing


-- Create a new connection manager.
makeManager :: IO HC.Manager
makeManager = HCTLS.newTlsManager

-- Create a new connection manager with custom settings (unsafe).
-- Only use this for development to avoid verification of HTTPS certs
makeManagerUnsafe :: IO HC.Manager
makeManagerUnsafe = HCTLS.newTlsManagerWith settings
  where
    settings =
      HCTLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

runRequestSafely :: Encoding -> HC.Manager -> URI.URI -> Data -> IO (Either CE.Error (HC.Response LBS.ByteString))
runRequestSafely encoding manager uri msg =
  case makeRequest encoding uri msg of
    Just request -> runHTTPIOSafely (HC.httpLbs request manager)
    Nothing      -> return $ Left CE.UnableToConnect

makeRequest :: Encoding -> URI.URI -> Data -> Maybe HC.Request
makeRequest encoding uri msg = do
  request <- HC.requestFromURI uri
  return $
    request
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS (encode encoding msg)
      , HC.requestHeaders = [(H.hContentType, mimeType), (H.hAccept, mimeType)]
      }
  where
    mimeType = TE.encodeUtf8 $ encodeMimeType encoding

encode :: Encoding -> Data -> LBS.ByteString
encode TextEncoding   = Data.toUTF8ByteString
encode BinaryEncoding = Data.toByteString

runHTTPIOSafely :: IO a -> IO (Either CE.Error a)
runHTTPIOSafely = tryJust handleHTTPException

handleHTTPException :: HC.HttpException -> Maybe CE.Error
handleHTTPException _ = Just CE.UnableToConnect
