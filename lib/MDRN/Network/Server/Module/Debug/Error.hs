{-# LANGUAGE OverloadedStrings #-}

module MDRN.Network.Server.Module.Debug.Error
    ( module'
    ) where

import           MDRN.Data.Encode          (toData)
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr        as Expr
import qualified MDRN.Language.Map         as Map
import qualified MDRN.Network.Server.Error as SE

module' :: Map
module' =
  Map.fromList
    [ ("parse-failure", F throwParseFailure)
    , ("invalid-arguments", F throwServerInvalidArguments)
    , ("non-function-apply", F throwServerNonFunctionApply)
    , ("not-defined", F throwServerNotDefined)
    , ("domain-error", F throwServerDomainError)
    , ("expr-already-bound", F throwServerExprAlreadyBound)
    , ("not-serializable", F throwServerNotSerializable)
    , ("io-error", F throwServerIOError)
    ]

throwParseFailure :: Function
throwParseFailure scope evalExpr args =
  case args of
    [] -> return $ Expr.optimize $ toData SE.ParseFailure
    _  -> throwEvaluationError EEInvalidArguments

throwServerInvalidArguments :: Function
throwServerInvalidArguments scope evalExpr args =
  case args of
    [] -> do
      let err = SE.RuntimeError $ RuntimeError EEInvalidArguments []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerNonFunctionApply :: Function
throwServerNonFunctionApply scope evalExpr args =
  case args of
    [P (Text sym)] -> do
      let err = SE.RuntimeError $ RuntimeError (EENonFunctionApply sym) []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerNotDefined :: Function
throwServerNotDefined scope evalExpr args =
  case args of
    [P (Text sym)] -> do
      let err = SE.RuntimeError $ RuntimeError (EENotDefined sym) []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerDomainError :: Function
throwServerDomainError scope evalExpr args =
  case args of
    [expr] -> do
      data' <- either (const $ throwEvaluationError EEInvalidArguments) return $ Expr.tryConvertToData expr
      let err = SE.RuntimeError $ RuntimeError (EEDomainError data') []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerExprAlreadyBound :: Function
throwServerExprAlreadyBound scope evalExpr args =
  case args of
    [P (Text sym)] -> do
      let err = SE.RuntimeError $ RuntimeError (EEExprAlreadyBound sym) []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerNotSerializable :: Function
throwServerNotSerializable scope evalExpr args =
  case args of
    [P (Text sym)] -> do
      let err = SE.RuntimeError $ RuntimeError (EENotSerializable sym) []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments

throwServerIOError :: Function
throwServerIOError scope evalExpr args =
  case args of
    [] -> do
      let err = SE.RuntimeError $ RuntimeError EEIOError []
      return $ Expr.optimize $ toData err
    _ -> throwEvaluationError EEInvalidArguments
