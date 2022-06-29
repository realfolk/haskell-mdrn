{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Either
    ( module'
    ) where

import           MDRN.Language.Expr
import qualified MDRN.Language.Map  as Map

module' :: Map
module' =
  Map.fromList
    [ ("either", F either')
    , ("is-left", F isLeft)
    , ("is-right", F isRight)
    , ("from-left", F fromLeft)
    , ("from-right", F fromRight)
    , ("map-left", F mapLeft)
    , ("map-right", F mapRight)
    ]

-- | Case analysis for the MDRN 'either' type. If the value is a 'left', apply the first 'Function'. If the value is a 'right', apply the second 'Function'.
either' :: Function
either' scope evalExpr args =
  case args of
    [F left, F right, L [P (Symbol "left"), value]] ->
      left scope evalExpr [value]
    [F left, F right, L [P (Symbol "right"), value]] ->
      right scope evalExpr [value]
    _ -> throwEvaluationError EEInvalidArguments

-- | Indicates whether the given expression is a 'left' value 'either'.
isLeft :: Function
isLeft _ _ args =
  case args of
    [L [P (Symbol "left"), _]]  -> return $ P $ Bool True
    [L [P (Symbol "right"), _]] -> return $ P $ Bool False
    _                           -> throwEvaluationError EEInvalidArguments

-- | Indicates whether the given expression is a 'right' value 'either'.
isRight :: Function
isRight _ _ args =
  case args of
    [L [P (Symbol "right"), _]] -> return $ P $ Bool True
    [L [P (Symbol "left"), _]]  -> return $ P $ Bool False
    _                           -> throwEvaluationError EEInvalidArguments

-- | Return the value if the 'either' is a 'left', otherwise return the provided default value.
fromLeft :: Function
fromLeft _ _ args =
  case args of
    [_, L [P (Symbol "left"), expr]]      -> return expr
    [default', L [P (Symbol "right"), _]] -> return default'
    _                                     -> throwEvaluationError EEInvalidArguments

-- | Return the value if the 'either' is a 'right', otherwise return the provided default value.
fromRight :: Function
fromRight _ _ args =
  case args of
    [_, L [P (Symbol "right"), expr]] -> return expr
    [default', L [P (Symbol "left"), expr]] -> return default'
    _ -> throwEvaluationError EEInvalidArguments

-- | Map a 'Function' f over the 'either'. Returns the application of f to a 'right' value, and passes through a 'left' value unchanged.
mapRight :: Function
mapRight scope evalExpr args =
  case args of
    [F f, L [P (Symbol "right"), value]] -> do
      r <- f scope evalExpr [value]
      return $ L [P (Symbol "right"), r]
    [_, expr@(L [P (Symbol "left"), _])] -> return expr
    _ -> throwEvaluationError EEInvalidArguments

-- | Map a 'Function' f over the 'either'. Returns the application of f to a 'left' value, and passes through a 'right' value unchanged.
mapLeft :: Function
mapLeft scope evalExpr args =
  case args of
    [F f, L [P (Symbol "left"), value]] -> do
      r <- f scope evalExpr [value]
      return $ L [P (Symbol "left"), r]
    [_, expr@(L [P (Symbol "right"), _])] -> return expr
    _ -> throwEvaluationError EEInvalidArguments
