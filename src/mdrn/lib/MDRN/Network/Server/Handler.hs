{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Server.Handler
    ( handleRequest
    ) where

import           Data.Bifunctor                    (first)
import           MDRN.Language.Evaluator           (evalExpr)
import           MDRN.Language.Expr                hiding (Result)
import qualified MDRN.Language.Expr                as Expr
import qualified MDRN.Language.Map                 as Map
import qualified MDRN.Language.Scope               as Scope
import qualified MDRN.Module.Base                  as BaseModule
import qualified MDRN.Module.Map                   as MapModule
import qualified MDRN.Network.Server.Error         as SE
import qualified MDRN.Network.Server.Module.Remote as RemoteModule

handleRequest :: Map -> Data -> IO (Either SE.Error Data)
handleRequest m data' = do
  let domainModule = RemoteModule.module' m
  let scope = Scope.push domainModule baseServerScope
  result <- runResult $ evalExpr scope $ Expr.optimize data'
  return $
    case result of
      Right (_, expr) -> prepareResult expr
      Left err        -> Left $ SE.RuntimeError err
  where
    prepareResult = first (const SE.ParseFailure) . Expr.tryConvertToData

-- Needs the @apply@ (so @import.remote@ can work)
-- and @get@ (so dot notation works) functions.
baseServerScope :: Scope
baseServerScope =
  Scope.fromMap $
    Map.fromList
      [ ("apply", F BaseModule.apply)
      , ("get", F MapModule.get)
      ]
