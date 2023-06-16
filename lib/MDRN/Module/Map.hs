{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Map
    ( get
    , module'
    ) where

import qualified Data.Map           as M
import qualified MDRN.Data          as Data
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr as Expr
import qualified MDRN.Language.Map  as Map

-- | Standard Map data structure for the MDRN language.
module' :: Map
module' =
  Map.fromList
    [ ("from-list", F fromList')
    , ("to-list", F toList')
    , ("get", F get)
    , ("set", F set)
    , ("delete", F delete)
    , ("values", F values)
    , ("keys", F keys)
    , ("empty", F empty)
    , ("map-keys", F mapKeys)
    , ("map-values", F mapExprs)
    , ("map-values-with-key", F mapExprsWithKey)
    ]

-- * Map Operations
-- | Given two symbols, a map name and a key, look up the map, and then get the value from the map for that key.
--
-- @
-- (get '(mapName) '(fnName))
-- @
get :: Function
get scope evalData args =
  case args of
    [M m, P (Symbol key)] -> do
      maybe (throwEvaluationError (EENotDefined key)) return $ Map.lookup key m
    _ -> throwEvaluationError EEInvalidArguments

-- | Set the value 'v' for key 'k' in the specified Map 'm', and returns the updated Map.
--
-- @
-- map.set m k v
-- @
set :: Function
set scope evalData args =
  case args of
    [M m, P (Symbol k), v] -> do
      maybe (throwEvaluationError (EEExprAlreadyBound k)) (return . M) $ Map.tryInsert k v m
    _ -> throwEvaluationError EEInvalidArguments

-- | Delete the key-value pair with the specified key 'k' from the given Map 'm', and returns the updated Map.
--
-- If 'k' is not a key in 'm', then the original Map 'm' is returned.
--
-- @
-- map.delete m k
-- @
delete :: Function
delete scope evalData args =
  case args of
    [M m , P (Symbol k)] -> maybe (throwEvaluationError (EENotDefined k)) (return . M) $ Map.tryDelete k m
    _                    -> throwEvaluationError EEInvalidArguments

-- | Create a new Map from the given list of key-value pairs.
--
-- @
-- map.from-list (["foo", 10], ["bar", 20])
-- @
fromList' :: Function
fromList' scope evalData args = loop args Map.empty
  where
    loop exprs m =
      case exprs of
        (L [P (Symbol k), v]:xs) -> do
          (_, value) <- evalData scope v
          loop xs $ Map.insert k value m
        [] -> return $ M m
        _ -> throwEvaluationError EEInvalidArguments

-- | Produce a 'List' of key-value pairs from the specified Map 'm'.
--
-- Function values will simply be represented as the string "Function".
-- Map values will be fully expanded into their own list representations.
--
-- @
-- (let [(m (from-list [foo 10] [bar 20]))] [map.to-list m])
-- ... produces ...
--  List
--    [ List [Node (Symbol "bar"), Node (Integer 20)]
--    , List [Node (Symbol "foo"), Node (Integer 10)]
--    ]
-- @
toList' :: Function
toList' scope evalData args =
  case args of
    [M m] -> do
      Expr.optimize . List <$> mapToData m
    _ -> throwEvaluationError EEInvalidArguments
  where
    mapToData m = mapM kvToData $ Map.toList m
    kvToData (k, v) = do
      let eitherExpr = Expr.tryConvertToData v
      case eitherExpr of
        Right value -> return $ List [Node (Symbol k), value]
        Left err    -> throwEvaluationError $ EENotSerializable err

-- | Returns the values of the specified Map 'm' as a List.
--
-- @
-- (let [(m (from-list [foo 10] [bar 20]))] [map.values m])
-- ... produces ...
-- List [Node (Integer 20), Node (Integer 10)])
-- @
values :: Function
values scope evalData args =
  case args of
    [M m] -> return $ L $ M.elems $ Map.unwrap m
    _     -> throwEvaluationError EEInvalidArguments

-- | Returns the keys of the specified Map 'm' as a List.
--
-- @
-- (let [(m (from-list [foo 10] [bar 20]))] [map.keys m])
-- ... produces ...
-- List [Node (Symbol "bar"), Node (Symbol "foo")])
-- @
keys :: Function
keys scope evalSExpr args =
  case args of
    [M m] -> return $ L $ map (Expr.optimize . Data.symbol) (M.keys $ Map.unwrap m)
    _     -> throwEvaluationError EEInvalidArguments

-- | Returns an empty Map.
--
-- @
-- (let [(mymap (map.empty))] [map.to-list mymap])"
-- ... produces ...
-- List []
-- @
empty :: Function
empty scope evalSExpr args =
  case args of
    [] -> return $ M Map.empty
    _  -> throwEvaluationError EEInvalidArguments

-- | Maps a given function onto the keys of the specified Map 'm'.
--
-- @
-- (let [(double (fn [a] [append a a])) (mymap (map.from-list [foo 10] [bar 20]))] [map.map-keys double mymap])
-- ... produces ...
-- M
--  (Map.fromList
--    [("barbar", P (Integer 20)), ("foofoo", P (Integer 10))])
-- @
mapKeys :: Function
mapKeys scope evalSExpr args =
  case args of
    [F f, M m] ->
      M <$> M.foldrWithKey (accumulateNewMap f) (return Map.empty) (Map.unwrap m)
    _ -> throwEvaluationError EEInvalidArguments
  where
    accumulateNewMap f k v accResult = do
      newMap <- accResult
      newKeyResult <- f scope evalSExpr [P (Symbol k)]
      case newKeyResult of
        P (Symbol newKey) -> return $ Map.insert newKey v newMap
        _                 -> throwEvaluationError EEInvalidArguments

-- | Maps a given function onto the values of the specified Map 'm'.
--
-- @
-- (let [(double (fn [a] [add a a])) (mymap (map.from-list [foo 10] [bar 20]))] [map.map-values double mymap])
-- ... produces ...
-- M
--  (Map.fromList
--    [("bar", P (Integer 40)), ("foo", P (Integer 20))])
-- @
mapExprs :: Function
mapExprs scope evalSExpr args =
  case args of
    [F f, M m] ->
      M <$> M.foldrWithKey (accumulateNewMap f) (return Map.empty) (Map.unwrap m)
    _ -> throwEvaluationError EEInvalidArguments
  where
    accumulateNewMap f k v accResult = do
      newMap <- accResult
      newExpr <- f scope evalSExpr [v]
      return $ Map.insert k newExpr newMap

-- | Maps a given function onto the keys and values of the specified Map 'm'.
--
-- @
-- (let
--    [double (fn [a k] [add a a])]
--    [mymap (map.from-list [foo 10] [bar 20])]
--    [map.map-values-with-key double mymap])
-- ... produces ...
-- M
--  (Map.fromList
--    [("bar", P (Integer 40)), ("foo", P (Integer 20))])
-- @
mapExprsWithKey :: Function
mapExprsWithKey scope evalSExpr args =
  case args of
    [F f, M m] ->
      M <$> M.foldrWithKey (accumulateNewMap f) (return Map.empty) (Map.unwrap m)
    _ -> throwEvaluationError EEInvalidArguments
  where
    accumulateNewMap f k v accResult = do
      newMap <- accResult
      newExpr <- f scope evalSExpr [P (Symbol k), v]
      return $ Map.insert k newExpr newMap
