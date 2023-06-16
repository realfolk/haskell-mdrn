{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Maybe
    ( module'
    ) where

import           MDRN.Language.Expr
import qualified MDRN.Language.Map  as Map

module' :: Map
module' =
  Map.fromList
    [ ("maybe", F maybe')
    , ("map", F map')
    , ("is-just", F isJust)
    , ("is-nothing", F isNothing)
    , ("from-just", F fromJust)
    , ("from-maybe", F fromMaybe)
    ]

-- | Case analysis for a MDRN 'maybe' value. Takes a default value, a 'Function' v, and a 'maybe'. If the 'maybe' is a 'just', it returns the application of f to the value in the 'just'. Otherwise it returns the default value.
maybe' :: Function
maybe' scope evalExpr args =
  case args of
    [default', F f, L [P (Symbol "just"), value]] -> f scope evalExpr [value]
    [default', F f, P (Symbol "nothing")] -> return default'
    _ -> throwEvaluationError EEInvalidArguments

-- | Indicates whether the given 'maybe' is a 'just' or otherwise.
isJust :: Function
isJust _ _ args =
  case args of
    [L [P (Symbol "just"), _]] -> return $ P $ Bool True
    [P (Symbol "nothing")]     -> return $ P $ Bool False
    _                          -> throwEvaluationError EEInvalidArguments

-- | Indicates whether the given 'maybe' is a 'nothing' or otherwise.
isNothing :: Function
isNothing _ _ args =
  case args of
    [L [P (Symbol "just"), _]] -> return $ P $ Bool False
    [P (Symbol "nothing")]     -> return $ P $ Bool True
    _                          -> throwEvaluationError EEInvalidArguments

-- | Returns the element from a 'maybe' if it's a 'just', otherwise it throws an error.
fromJust :: Function
fromJust _ _ args =
  case args of
    [L [P (Symbol "just"), value]] -> return value
    _                              -> throwEvaluationError EEInvalidArguments

-- | Returns the element from a 'maybe' if it's a 'just', otherwise returns the given default value.
fromMaybe :: Function
fromMaybe _ _ args =
  case args of
    [_, L [P (Symbol "just"), value]] -> return value
    [default', P (Symbol "nothing")]  -> return default'
    _                                 -> throwEvaluationError EEInvalidArguments

-- | Map a 'Function' f over the 'maybe'. Returns the application of f to a 'just' value, and passes through a 'nothing' if present.
map' :: Function
map' scope evalExpr args =
  case args of
    [F f, L [P (Symbol "just"), value]] -> do
      r <- f scope evalExpr [value]
      return $ L [P (Symbol "just"), r]
    [_, expr@(P (Symbol "nothing"))] -> return expr
    _ -> throwEvaluationError EEInvalidArguments
