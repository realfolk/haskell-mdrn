{-# LANGUAGE OverloadedStrings #-}

module MDRN.REPL.Module
    ( module'
    ) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Foldable           (foldlM)
import qualified Data.Text.IO            as TIO
import qualified MDRN.Language.Evaluator as Evaluator
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr      as Expr
import qualified MDRN.Language.Map       as Map
import qualified MDRN.Language.Scope     as Scope
import           MDRN.REPL.Lib           (exprToText)
import           System.IO               (stdout)

module' :: Map
module' =
  Map.fromList
    [ ("bind", F bind)
    , ("unbind", F unbind)
    ]

-- | Given a list of bindings, it evaluates them and returns the extended scope.
bind :: Function
bind scope _ args = toBindings args >>= buildScope scope >>= updateScope

-- | Given a symbol, it unbinds that symbol from the current scope and returns the modified scope.
unbind :: Function
unbind scope _ args =
  case args of
    [P (Symbol name)] -> Evaluator.unbind name scope >>= updateScope
    _                 -> throwEvaluationError EEInvalidArguments

-- * Helpers

toBindings :: Args -> Result [Data]
toBindings = either (const $ throwEvaluationError EEInvalidArguments) pure . mapM Expr.tryConvertToData

buildScope :: Scope -> [Data] -> Result Scope
buildScope = foldlM addBinding

addBinding :: Scope -> Data -> Result Scope
addBinding scope binding =
  case binding of
    List [Node (Symbol name), value] -> do
      (_, expr) <- Evaluator.evalExpr scope $ Expr.optimize value
      liftIO $ TIO.hPutStrLn stdout (name <> " :: " <> exprToText expr)
      return $ Scope.push (Map.fromList [(name, expr)]) scope
    _ -> throwEvaluationError EEInvalidArguments

updateScope :: Scope -> Result Expr
updateScope scope =
  return $ L $ P (Symbol "scope") : map M (Scope.toMaps scope)
