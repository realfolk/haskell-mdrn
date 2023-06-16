{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Import
    ( module'
    ) where

import           Control.Monad            (join)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bifunctor           (bimap, first)
import qualified Data.Text                as T
import qualified MDRN.Data                as Data
import qualified MDRN.Data.Decode         as Decode
import           MDRN.Data.Encode         (ToData (..))
import qualified MDRN.Data.Parser         as Parser
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr       as Expr
import qualified MDRN.Language.Map        as Map
import           MDRN.Metadata
import qualified MDRN.Network.Client.HTTP as Client
import           MDRN.Network.Mime

module' :: [(T.Text, Map)] -> Map
module' allowedImports =
  Map.fromList
    [ ("remote", F remote)
    , ("local", F local)
    , ("builtin", F $ builtin allowedImports)
    ]

-- | Function for importing a remote function module at the given address.
remote :: Function
remote scope evalExpr args =
  case args of
    [P (Text remoteAddr)] -> remoteSend remoteAddr
    _                     -> throwEvaluationError EEInvalidArguments
  where
    remoteSend :: T.Text -> Result Expr
    remoteSend addr =
      join $ liftIO $ either throwEvaluationError return <$> remoteSendWithAddr addr

    remoteSendWithAddr :: T.Text -> IO (Either EvaluationError Expr)
    remoteSendWithAddr addr =
      Client.fromAddr addr >>= maybe (return $ Left EEInvalidArguments) remoteSendWithClient

    remoteSendWithClient :: Client.Client -> IO (Either EvaluationError Expr)
    remoteSendWithClient client = do
      handleResponse client <$> client metadataClientMsg

    handleResponse :: Client.Client -> Either Data Data -> Either EvaluationError Expr
    handleResponse client response =
      first EEDomainError response >>= bimap (EEDomainError . toData) (handleMetadata client) . Decode.fromData

local :: Function
local scope evalExpr args = undefined

-- | Imports a built-in module not directly defined in the MDRN base module.
--
-- @
-- (import.builtin "file")
-- @
builtin :: [(T.Text, Map)] -> Function
builtin allowed scope evalExpr args =
  case args of
    [P (Text moduleName)] -> do
      let maybeMod = Map.lookup moduleName $ Map.fromList allowed
      maybe (throwEvaluationError $ EENotDefined moduleName) (return . M) maybeMod
    _ -> throwEvaluationError EEInvalidArguments

metadataClientMsg :: Data
metadataClientMsg = Data.list [Data.symbol "metadata"]

-- | Handle the metadata response from the server.
handleMetadata :: Client.Client -> Metadata -> Expr
handleMetadata adapter metadata =
  case metadata of
    MapMetadata name children  -> createRemoteMap adapter children name -- create a map containing remote functions, primitives, or lists
    FnMetadata name            -> createRemoteFunction adapter name -- create a remote function that can be called with args
    ListMetadata name children -> L (map (handleMetadata adapter) children)
    PrimMetadata _ prim        -> P prim

-- | Creates a map whose values are remote functions, or sub-maps containing remote functions.
createRemoteMap :: Client.Client -> [Metadata] -> T.Text -> Expr
createRemoteMap adapter children accMapName = M $ loop children Map.empty
  where
    loop xs m =
      case xs of
        ((FnMetadata n):rest) ->
          loop rest $
          Map.insert n (createRemoteFunction adapter (buildAccName n)) m
        ((MapMetadata n cs):rest) ->
          loop rest $
          Map.insert n (createRemoteMap adapter cs (buildAccName n)) m
        ((ListMetadata n c):rest) ->
          loop rest $ Map.insert n (L (map (handleMetadata adapter) c)) m
        ((PrimMetadata n p):rest) -> loop rest $ Map.insert n (P p) m
        [] -> m
    buildAccName n =
      if T.null accMapName
        then n
        else accMapName <> "." <> n

-- | Creates a remote function that can be invoked by a client application.
createRemoteFunction :: Client.Client -> Name -> Expr
createRemoteFunction client name =
  F (\scope evalExpr args -> do
       msg <- createMsg name args
       result <- liftIO $ client msg
       case result of
         Right v  -> return $ Expr.optimize v
         Left err -> throwEvaluationError $ EEDomainError err)
  where
    createMsg :: T.Text -> [Expr] -> Result Data
    createMsg n a = do
      -- We have to pass the function name through the MDRN parser in order to parse any shorthand syntax
      parsedName <- either (const $ throwEvaluationError EEInvalidArguments) return $ Parser.parseData n
      let dataArgs = mapM Expr.tryConvertToData a
      case dataArgs of
        Left _   -> throwEvaluationError EEInvalidArguments
        Right da -> return payload
          where
            payload = Data.list [Data.symbol "apply", parsedName, Data.quote (Data.list da)]
