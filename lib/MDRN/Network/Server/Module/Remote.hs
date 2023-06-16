{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Server.Module.Remote
    ( module'
    ) where

import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified MDRN.Data.Encode   as Encode
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr as Expr
import qualified MDRN.Language.Map  as Map
import           MDRN.Metadata      (exprToMetadata)

module' :: Map -> Map
module' serverModule =
  Map.wrap $
  M.unionWith f (Map.unwrap serverModule) $
  M.fromList [("metadata", F $ metadata serverModule)]
  where
    -- If the target module already has a "metadata" function, then preserve that.
    f v1 v2 = v1

-- Used for responding to "metadata" requests from clients.
-- Returns a list of available functions in the given module.
metadata :: Map -> Function
metadata module' scope evalExpr args =
  -- Empty text is passed as binding name since the 'Map' will be bound on the client.
  return $ Expr.optimize $ Encode.toData $ exprToMetadata T.empty $ M module'
