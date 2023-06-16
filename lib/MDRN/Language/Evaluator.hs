{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module MDRN.Language.Evaluator
    ( bind
    , evalExpr
    , evalNestedExpr
    , unbind
    ) where

import qualified Control.Monad.State as S
import qualified Data.Text           as T
import qualified MDRN.Data           as Data
import           MDRN.Language.Expr
import qualified MDRN.Language.Scope as Scope
import           Prelude             hiding (lookup)

-- * Evaluators

evalExpr :: EvalExpr
evalExpr scope expr =
  case expr of
    -- Variable reference
    P (Symbol name) -> (scope, ) <$> lookup name scope

    -- Function application
    L (funExpr:argsExprs) ->
      case funExpr of
        P (Symbol "quote") -> (scope, ) <$> quote argsExprs

        _ -> do
          -- Is it a function?
          (_, possibleF) <- evalExpr scope funExpr

          case possibleF of
            F f -> do
              -- 1. Push the valid function call onto the call stack
              S.modify (expr :)

              -- 2. Evaluate the arguments to the function
              args <- mapM (fmap snd . evalExpr scope) argsExprs

              -- 3. Apply the function to the arguments
              (scope, ) <$> apply f scope args

            -- Not a function
            nonF -> throwEvaluationError (EENonFunctionApply $ callStackExprToText nonF)

    -- Pass through
    _ -> return (scope, expr)

apply :: Function -> Scope -> Args -> Result Expr
apply f scope = f scope evalExpr

evalNestedExpr :: Scope -> EvalExpr -> Expr -> Result Expr
evalNestedExpr scope evalExpr expr =
  snd <$> evalExpr scope expr

-- * Built-in Functions

-- | Quote an 'Expr' verbatim.
--
-- @
-- (quote (foo bar baz))
-- (quote 5)
-- (quote xyz)
-- @
quote :: [Expr] -> Result Expr
quote args =
  case args of
    [expr] -> return expr
    _      -> throwEvaluationError EEInvalidArguments

-- * Scope Helpers

lookup :: Name -> Scope -> Result Expr
lookup name = maybe (throwEvaluationError (EENotDefined name)) return . Scope.lookup name

bind :: Name -> Expr -> Scope -> Result Scope
bind name expr = maybe (throwEvaluationError (EEExprAlreadyBound name)) return . Scope.tryBind name expr

unbind :: Name -> Scope -> Result Scope
unbind name = maybe (throwEvaluationError (EENotDefined name)) return . Scope.tryUnbind name
