{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MDRN.Network.Server.HTTP
    ( Config (..)
    , run
    ) where

import           Data.Bifunctor                    (bimap)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           GHC.Exception.Type                (SomeException,
                                                    fromException)
import qualified Lib.Time                          as Time
import qualified Lib.URL.Component.Path            as Path
import qualified Lib.UUID                          as UUID
import           Logger                            (Logger)
import qualified Logger                            as L
import           MDRN.Data.Decode                  (fromData)
import           MDRN.Data.Encode                  (ToData (..))
import           MDRN.Language.Expr                (Map)
import qualified MDRN.Network.Client.Request       as ClientRequest
import qualified MDRN.Network.Client.Response      as ClientResponse
import qualified MDRN.Network.Server.Error         as SE
import qualified MDRN.Network.Server.Handler       as Handler
import qualified MDRN.Network.Server.HTTP.Request  as HTTPRequest
import qualified MDRN.Network.Server.HTTP.Response as HTTPResponse
import           MDRN.Network.Server.HTTP.Route    (Action (..), Route)
import qualified MDRN.Network.Server.HTTP.Route    as Route
import qualified Network.HTTP.Types                as H
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import           System.TimeManager                (TimeoutThread (TimeoutThread))

data Config
  = Config
      { cRoutes       :: [Route]
        -- ^ The list of routes.
      , cNotFound     :: HTTPRequest.Request -> Action
        -- ^ The response to use when a requested route is not found.
      , cTLSSettings  :: Maybe WarpTLS.TLSSettings
        -- ^ The TLS settings to use when running securely.
      , cSettings     :: Maybe Warp.Settings
        -- ^ The Warp settings to use, if any.
      , cMiddleware   :: Maybe Wai.Middleware
        -- ^ The WAI middleware settings to use, if any.
      , cLogger       :: Logger
        -- ^ The logger to use.
      , cDomainModule :: Map
        -- ^ The domain module to use.
      }

run :: Config -> IO ()
run Config {..} =
  runSettings settings
    $ middleware
    $ \waiRequest sendWaiResponse -> do
        -- Track request start time
        start <- Time.now
        -- Transform request
        request <- HTTPRequest.fromWaiRequest waiRequest
        -- Determine log request ID
        requestLogId <- nextLogId
        -- Log incoming request
        logRequest cLogger requestLogId request
        -- Compute response based on server configuration
        response <- case Route.find request cRoutes of
          Just action -> processAction action
          Nothing     -> processAction $ cNotFound request
        -- Send response to client
        responseSent <- sendWaiResponse $ HTTPResponse.toWaiResponse response
        -- Track request end time
        end <- Time.now
        let elapsed = end - start
        logResponse cLogger requestLogId response elapsed
        -- Return correct value for WAI
        return responseSent
  where
    runSettings = maybe Warp.runSettings WarpTLS.runTLS cTLSSettings
    settings = Warp.setOnException (logException cLogger) $ fromMaybe Warp.defaultSettings cSettings
    middleware = fromMaybe id cMiddleware

    processAction :: Action -> IO HTTPResponse.Response
    processAction action = do
      case action of
        RunProgram program handler -> do
          case fromData program of
            Right (ClientRequest.Request id payload) -> do
              result <- Handler.handleRequest cDomainModule payload
              processAction $ handler $ bimap (prepareResponse id . toData) (prepareResponse id) result
            _ -> processAction $ handler $ Left $ prepareResponse 0 (toData SE.ParseFailure)
        Impure io -> io >>= processAction
        Respond response -> return response
    prepareResponse = (toData .) . ClientResponse.Response

-- Logging

instance L.ToRecord SomeException where
  toRecord = L.bold (L.Level L.Error) . L.text . T.pack . show

instance L.ToRecord TimeoutThread where
  toRecord = L.regular (L.Level L.Warn) . L.text . T.pack . show

instance L.ToRecord Warp.InvalidRequest where
  toRecord = L.regular (L.Level L.Warn) . L.text . T.pack . show

type LogId = UUID.UUID

nextLogId :: IO LogId
nextLogId = UUID.nextRandom

logIdToText :: LogId -> T.Text
logIdToText = UUID.encodeBase64TextStrict

logRequest :: L.Logger -> LogId -> HTTPRequest.Request -> IO ()
logRequest logger logId (method, path, _, _, _) =
  writeRecord (logger L.Debug) logId record
    where
      record = L.delimitSpace
        [ L.leftArrow
        , L.boldText (L.Level L.Info) $ TE.decodeUtf8 method
        , L.text $ Path.toText path
        ]

logResponse :: L.Logger -> LogId -> HTTPResponse.Response -> Time.Time -> IO ()
logResponse logger logId (H.Status code _, _, _) elapsed =
  writeRecord (logger L.Debug) logId record
    where
      record = L.delimitSpace
        [ L.rightArrow
        , L.bold (colorCode code) $ L.toRecord code
        , L.regular L.Muted $ L.toRecord $ Time.toMilliseconds elapsed
        ]

      colorCode code
        | code >= 400 = L.Level L.Error
        | code >= 300 = L.Level L.Warn
        | code >= 200 = L.Success
        | otherwise = L.Muted

writeRecord :: (L.Record -> IO ()) -> LogId -> L.Record -> IO ()
writeRecord log logId record =
  log
    $ L.delimitSpace
        [ L.surroundBrackets $ L.mutedRegularText $ logIdToText logId
        , record
        ]

logException :: L.Logger -> Maybe Wai.Request -> SomeException -> IO ()
logException logger maybeRequest exception =
  case fromException exception of
    Just e@TimeoutThread -> logWarn $ record e
    _                    -> case fromException exception of
                              Just e@Warp.ConnectionClosedByPeer -> logWarn $ record e
                              _ -> logError $ record exception
    where
      record :: L.ToRecord r => r -> L.Record
      record r = L.delimitSpace $ waiRequestRecord <> [L.toRecord r]
      waiRequestRecord =
        case maybeRequest of
          Nothing -> []
          Just request ->
            [ L.delimitSpace
                $ map (L.text . T.pack)
                    [ show $ Wai.requestMethod request
                    , show $ Wai.httpVersion request
                    , show $ Wai.pathInfo request
                    ]
            ]
      logWarn = logger L.Warn
      logError = logger L.Error
