{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.ByteString
    ( module'
    ) where

import qualified Data.ByteString    as B
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
    , ("concat", F concat')
    , ("take", F take')
    , ("drop", F drop')
    , ("null", F null')
    ]

-- | Append to 'ByteString's together into a single 'ByteString'
append :: Function
append _ _ args =
  case args of
    [P (ByteString b1), P (ByteString b2)] ->
      (return . P . ByteString) $ B.append b1 b2
    _ -> throwEvaluationError EEInvalidArguments

-- | Calculate the length of the given 'ByteString'
length' :: Function
length' _ _ args =
  case args of
    [P (ByteString b)] -> (return . P . Integer) $ fromIntegral $ B.length b
    _                  -> throwEvaluationError EEInvalidArguments

-- | Join a list of 'ByteString's with another 'ByteString' joining each element.
intercalate :: Function
intercalate _ _ args =
  case args of
    [P (ByteString joiner), L (P (ByteString first):rest)] ->
      fmap ((P . ByteString) . B.append first) (foldr (join joiner) (return "") rest)
    _ -> throwEvaluationError EEInvalidArguments
  where
    join :: B.ByteString -> Expr -> Result B.ByteString -> Result B.ByteString
    join joiner xs acc =
      case xs of
        P (ByteString b) ->
          let joined = B.append joiner b
           in B.append joined <$> acc
        _ -> throwEvaluationError EEInvalidArguments

-- | Reverse the given ByteString
--
-- @
-- (reverse #x:25:26:27), results in #x:27:26:25
-- @
reverse' :: Function
reverse' _ _ args =
  case args of
    [P (ByteString t)] -> (return . P . ByteString) $ B.reverse t
    _                  -> throwEvaluationError EEInvalidArguments

-- | Tranpose the given list of 'ByteString's
transpose :: Function
transpose _ _ args =
  case args of
    [L items] -> L <$> foldl transpose' (return []) items
    _         -> throwEvaluationError EEInvalidArguments
  where
    transpose' :: Result [Expr] -> Expr -> Result [Expr]
    transpose' acc x =
      case x of
        P (ByteString t) -> fmap (x :) acc
        _                -> throwEvaluationError EEInvalidArguments

-- | Concatenate the given list of 'ByteString's into a single 'ByteString'
concat' :: Function
concat' _ _ args =
  case args of
    [L items] -> P . ByteString <$> foldr join (return "") items
    _         -> throwEvaluationError EEInvalidArguments
  where
    join :: Expr -> Result B.ByteString -> Result B.ByteString
    join xs acc =
      case xs of
        P (ByteString t) -> B.append t <$> acc
        _                -> throwEvaluationError EEInvalidArguments

-- | Take given number of 'Word8's from the beginning of the given 'ByteString' to form a new 'ByteString'
take' :: Function
take' _ _ args =
  case args of
    [P (Integer n), P (ByteString t)] ->
      (return . P . ByteString) $ B.take (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

-- | Drop the given number of 'Word8's from the beginning of the given 'ByteString' to form a new 'ByteString' from the remainder.
drop' :: Function
drop' _ _ args =
  case args of
    [P (Integer n), P (ByteString t)] ->
      (return . P . ByteString) $ B.drop (fromIntegral n) t
    _ -> throwEvaluationError EEInvalidArguments

-- | Test whether the given 'ByteString' is empty.
null' :: Function
null' _ _ args =
  case args of
    [P (ByteString b)] -> (return . P . Bool) $ B.null b
    _                  -> throwEvaluationError EEInvalidArguments
