{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Server.Module.Debug
    ( module'
    ) where

import           MDRN.Language.Expr
import qualified MDRN.Language.Map                      as Map
import qualified MDRN.Network.Server.Module.Debug.Error as Error

module' :: Map
module' = Map.fromList [ ("errors", M Error.module') ]
