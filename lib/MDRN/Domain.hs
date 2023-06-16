{-# LANGUAGE OverloadedStrings #-}

module MDRN.Domain
    ( FromArgs (..)
    , ToResult (..)
    , domainFunction
    ) where

import           MDRN.Data          (symbol)
import           MDRN.Data.Encode   (ToData (..))
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr as Expr

class ToResult m where
  toResult :: m a -> Result a

instance ToResult Maybe where
  toResult m =
    case m of
      Just a  -> return a
      Nothing -> throwEvaluationError $ EEDomainError $ symbol "nothing"

class FromArgs options where
  fromArgs :: Args -> Maybe options

domainFunction ::
     (ToResult m, FromArgs options, ToData result)
  => (options -> m result)
  -> Function
domainFunction f scope evalExpr args = do
  opts <- maybe (throwEvaluationError EEInvalidArguments) return (fromArgs args)
  mdrnR <- toResult $ f opts
  return $ Expr.optimize $ toData mdrnR
