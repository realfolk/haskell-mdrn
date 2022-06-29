{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Text
    ( module'
    ) where

import qualified Data.Text          as T
import           MDRN.Language.Expr
import qualified MDRN.Language.Map  as Map

module' :: Map
module' =
  Map.fromList
    [ ("append", F append)
    , ("length", F length')
    , ("intercalate", F intercalate)
    , ("reverse", F reverse')
    , ("transpose", F transpose)
    , ("replace", F replace)
    , ("to-upper", F toUpper)
    , ("to-lower", F toLower)
    , ("to-title", F toTitle)
    , ("concat", F concat')
    , ("replicate", F replicate')
    , ("take", F take')
    , ("take-end", F takeEnd)
    , ("drop", F drop')
    , ("drop-end", F dropEnd)
    , ("trim", F trim)
    , ("null", F null')
    ]

append :: Function
append _ _ args =
  case args of
    [P (Text t1), P (Text t2)]     -> (return . P . Text) $ T.append t1 t2
    [P (Symbol t1), P (Symbol t2)] -> (return . P . Symbol) $ T.append t1 t2
    _                              -> throwEvaluationError EEInvalidArguments

length' :: Function
length' _ _ args =
  case args of
    [P (Text t)] -> (return . P . Integer) $ fromIntegral $ T.length t
    _            -> throwEvaluationError EEInvalidArguments

intercalate :: Function
intercalate _ _ args =
  case args of
    [P (Text joiner), L (P (Text first):rest)] ->
      fmap ((P . Text) . T.append first) (foldr (join joiner) (return "") rest)
    _ -> throwEvaluationError EEInvalidArguments
  where
    join :: T.Text -> Expr -> Result T.Text -> Result T.Text
    join joiner xs acc =
      case xs of
        P (Text t) ->
          let joined = T.append joiner t
           in T.append joined <$> acc
        _ -> throwEvaluationError EEInvalidArguments

reverse' :: Function
reverse' _ _ args =
  case args of
    [P (Text t)] -> (return . P . Text) $ T.reverse t
    _            -> throwEvaluationError EEInvalidArguments

transpose :: Function
transpose _ _ args =
  case args of
    [L items] -> L <$> foldl transpose' (return []) items
    _         -> throwEvaluationError EEInvalidArguments
  where
    transpose' :: Result [Expr] -> Expr -> Result [Expr]
    transpose' acc x =
      case x of
        P (Text t) -> fmap (x :) acc
        _          -> throwEvaluationError EEInvalidArguments

replace :: Function
replace _ _ args =
  case args of
    [P (Text needle), P (Text replacement), P (Text hayStack)] ->
      (return . P . Text) $ T.replace needle replacement hayStack
    _ -> throwEvaluationError EEInvalidArguments

toLower :: Function
toLower _ _ args =
  case args of
    [P (Text t)] -> (return . P . Text) $ T.toLower t
    _            -> throwEvaluationError EEInvalidArguments

toUpper :: Function
toUpper _ _ args =
  case args of
    [P (Text t)] -> (return . P . Text) $ T.toUpper t
    _            -> throwEvaluationError EEInvalidArguments

toTitle :: Function
toTitle _ _ args =
  case args of
    [P (Text t)] -> (return . P . Text) $ T.toTitle t
    _            -> throwEvaluationError EEInvalidArguments

concat' :: Function
concat' _ _ args =
  case args of
    [L items] -> P . Text <$> foldr join (return "") items
    _         -> throwEvaluationError EEInvalidArguments
  where
    join :: Expr -> Result T.Text -> Result T.Text
    join xs acc =
      case xs of
        P (Text t) -> T.append t <$> acc
        _          -> throwEvaluationError EEInvalidArguments

replicate' :: Function
replicate' _ _ args =
  case args of
    [P (Integer n), P (Text t)] ->
      (return . P . Text) $ T.replicate (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

take' :: Function
take' _ _ args =
  case args of
    [P (Integer n), P (Text t)] ->
      (return . P . Text) $ T.take (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

takeEnd :: Function
takeEnd _ _ args =
  case args of
    [P (Integer n), P (Text t)] ->
      (return . P . Text) $ T.takeEnd (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

drop' :: Function
drop' _ _ args =
  case args of
    [P (Integer n), P (Text t)] ->
      (return . P . Text) $ T.drop (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

dropEnd :: Function
dropEnd _ _ args =
  case args of
    [P (Integer n), P (Text t)] ->
      (return . P . Text) $ T.dropEnd (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

trim :: Function
trim _ _ args =
  case args of
    [P (Text t)] -> (return . P . Text) $ T.strip t
    _            -> throwEvaluationError EEInvalidArguments

null' :: Function
null' _ _ args =
  case args of
    [P (Text b)] -> (return . P . Bool) $ T.null b
    _            -> throwEvaluationError EEInvalidArguments
