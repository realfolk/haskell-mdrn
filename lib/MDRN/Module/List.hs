{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.List
    ( module'
    ) where

import           MDRN.Language.Expr
import qualified MDRN.Language.Map  as Map

module' :: Map
module' =
  Map.fromList
    [ ("create", F create)
    , ("concat", F concat')
    , ("append", F append)
    , ("map", F map')
    , ("filter", F filter')
    , ("length", F length')
    , ("null", F null')
    , ("head", F head')
    , ("last", F last')
    , ("tail", F tail')
    , ("init", F init')
    , ("get", F get')
    , ("reverse", F reverse')
    , ("and", F and')
    , ("or", F or')
    , ("all", F all')
    , ("any", F any')
    ]

-- * List Operations
-- | Construct a list by first evaluating each item in the argument passed to this function.
--
-- @
-- (let [(a 1) (b 2)] [list.create [a b])]
-- ; ... produces the same result as ...
-- [1 2]
-- @
--
-- You can also use the short-hand notation:
--
-- @
-- {1 (add 2 3)}
-- ; ... expands to ...
-- (list.create (quote (1 (add 2 3))))
-- ; ... and produces the same result as ...
-- [1 5]
-- @
create :: Function
create scope evalExpr args =
  case args of
    [L exprs] -> do
      items <- mapM (fmap snd . evalExpr scope) exprs
      return $ L items
    _         -> throwEvaluationError EEInvalidArguments

-- | Flatten multiple lists into a single list.
--
-- @
-- (concat [1 2] [2 3] [3 4]), results in (1 2 2 3 3 4)
-- @
concat' :: Function
concat' _ _ args = L <$> foldr join (return []) args
  where
    join :: Expr -> Result [Expr] -> Result [Expr]
    join x acc =
      case x of
        L l -> fmap (l ++) acc
        _   -> throwEvaluationError EEInvalidArguments

-- | Construct a single list by appending two lists together.
--
-- @
-- (append ["foo"] ["bar"]), results in ("foo" "bar")
-- @
append :: Function
append _ _ args =
  case args of
    [L l1, L l2] -> (return . L) $ l1 ++ l2
    _            -> throwEvaluationError EEInvalidArguments

-- | Map a function f over each item in a list.
--
-- @
-- (map (partial add 1) [1 2 3]), results in (2 3 4)
-- @
map' :: Function
map' scope evalExpr args =
  case args of
    [F f, L l] -> L <$> mapM (\l' -> f scope evalExpr [l']) l
    _          -> throwEvaluationError EEInvalidArguments

-- | Filter a list by the given predicate.
-- Will return an error if the predicate cannot be applied to any expression in the list or if the predicate returns anything other than a 'Bool'.
--
-- @
-- (filter (partial lt 5) [2 4 6 8]), results in (6 8)
-- @
filter' :: Function
filter' scope evalExpr args =
  case args of
    [F f, L l] -> L <$> foldr (evalAndCompare f) (return []) l
    _          -> throwEvaluationError EEInvalidArguments
  where
    evalAndCompare f x acc = do
      r <- f scope evalExpr [x]
      case r of
        P (Bool True)  -> (x :) <$> acc
        P (Bool False) -> acc
        _              -> throwEvaluationError EEInvalidArguments

-- | Returns the length of the given list.
--
-- @
-- (length [1 2 3]), results in 3
-- @
length' :: Function
length' _ _ args =
  case args of
    [L l] -> (return . P . Integer) $ fromIntegral $ length l
    _     -> throwEvaluationError EEInvalidArguments

-- | Indicates whether the specified list is empty or not.
--
-- @
-- (null []), results in True
-- @
--
-- @
-- (null [1 2 3]), results in False
-- @
null' :: Function
null' _ _ args =
  case args of
    [L []] -> return $ P $ Bool True
    [L _]  -> return $ P $ Bool False
    _      -> throwEvaluationError EEInvalidArguments

-- | Returns the first item in the given list.
-- Will throw an error if an empty list is provided or anything other than a list is provided.
--
-- @
-- (head ["foo" "bar" 1 2 3]), results in "foo"
-- @
--
-- @
-- (head []), throws an EEInvalidArguments exception.
-- @
head' :: Function
head' _ _ args =
  case args of
    [L []] -> throwEvaluationError EEInvalidArguments
    [L l]  -> return $ head l
    _      -> throwEvaluationError EEInvalidArguments

-- | Returns the last item in the given list.
-- Will throw an error if an empty list is provided or anything other than a list is provided.
--
-- @
-- (last ["foo" "bar" 1 2 3]), results in 3
-- @
--
-- @
-- (last []), throws an EEInvalidArguments exception.
-- @
last' :: Function
last' _ _ args =
  case args of
    [L []] -> throwEvaluationError EEInvalidArguments
    [L l]  -> return $ last l
    _      -> throwEvaluationError EEInvalidArguments

-- | Returns the elements after the first item in the given list.
-- Will throw an error if an empty list is provided or anything other than a list is provided.
--
-- @
-- (tail ["foo" "bar" 1 2 3]), results in ("bar" 1 2 3)
-- @
--
-- @
-- (tail []), throws an EEInvalidArguments exception.
-- @
tail' :: Function
tail' _ _ args =
  case args of
    [L []] -> throwEvaluationError EEInvalidArguments
    [L l]  -> (return . L) $ tail l
    _      -> throwEvaluationError EEInvalidArguments

-- | Returns the elements of the given list except for the last item.
-- Will throw an error if an empty list is provided or anything other than a list is provided.
--
-- @
-- (init ["foo" "bar" 1 2 3]), results in ("foo" "bar" 1 2)
-- @
--
-- @
-- (init []), throws an EEInvalidArguments exception.
-- @
init' :: Function
init' _ _ args =
  case args of
    [L []] -> throwEvaluationError EEInvalidArguments
    [L l]  -> (return . L) $ init l
    _      -> throwEvaluationError EEInvalidArguments

-- | Returns the element at the given index in the given list.
-- Will throw an error if the given index is greater than or equal to the length of the list.
--
-- @
-- (list.get ["hello" "world" 1 2 3] 4), results in 3
-- @
--
-- @
-- (list.get ["hello" "world" 1 2 3] 5, throws an exception since 5 is greater than the largest index for the given list.
-- @
get' :: Function
get' _ _ args =
  case args of
    [L l, P (Integer i)] ->
      if fromIntegral i >= length l
        then throwEvaluationError EEInvalidArguments
        else return $ l !! fromIntegral i
    _ -> throwEvaluationError EEInvalidArguments

-- | Reverses the elements of the given list.
--
-- @
-- (reverse [1 2 3]), results in (3 2 1)
-- @
reverse' :: Function
reverse' _ _ args =
  case args of
    [L l] -> (return . L) $ reverse l
    _     -> throwEvaluationError EEInvalidArguments

-- | Returns the conjuction of a list consisting of 'Bool' values.
-- Will throw an error if the given list contains values other than 'Bool'.
--
-- @
-- (and [#t #f #t]), results in #f
-- @
--
-- @
-- (and ["foo" #t #t]), will throw an error due to non-boolean expression in the list.
-- @
and' :: Function
and' _ _ args =
  case args of
    [L l] -> P . Bool <$> foldr process (return True) l
    _     -> throwEvaluationError EEInvalidArguments
  where
    process x acc =
      case x of
        P (Bool True)  -> acc
        P (Bool False) -> return False
        _              -> throwEvaluationError EEInvalidArguments

-- | Returns the disjunction of a list consisting of 'Bool' values.
-- Will throw an error if the given list contains values other than 'Bool'.
--
-- @
-- (or [#t #f #t]), results in #t
-- @
--
-- @
-- (or ["foo" #t #t]), will throw an error due to non-boolean expression in the list.
-- @
or' :: Function
or' _ _ args =
  case args of
    [L l] -> P . Bool <$> foldr process (return False) l
    _     -> throwEvaluationError EEInvalidArguments
  where
    process x acc =
      case x of
        P (Bool True)  -> return True
        P (Bool False) -> acc
        _              -> throwEvaluationError EEInvalidArguments

-- | Determines whether all of the elements in the given list satisfy the given predicate.
-- Will throw an error if the predicate can not be applied to any element of the list, or if the predicate a non-boolean result.
--
-- @
-- (all (partial eq 5) [5 5 5]), results in #t
-- @
--
-- @
-- (all (partial eq 5) [5 1 5]), results in #f
-- @
--
-- @
-- (all (partial eq 5) ["foo" 1 5]), throws an error since 'eq' cannot be applied to "foo"
-- @
all' :: Function
all' scope evalExpr args =
  case args of
    [F f, L l] -> P . Bool <$> foldr (evalAndCompare f) (return True) l
    _          -> throwEvaluationError EEInvalidArguments
  where
    evalAndCompare f x acc = do
      r <- f scope evalExpr [x]
      case r of
        P (Bool True)  -> acc
        P (Bool False) -> return False
        _              -> throwEvaluationError EEInvalidArguments

-- | Determines whether nay of the elements in the given list satisfy the given predicate.
-- Will throw an error if the predicate can not be applied to any element of the list, or if the predicate a non-boolean result.
--
-- @
-- (any (partial eq 5) [4 4 4]), results in #f
-- @
--
-- @
-- (any (partial eq 5) [5 1 2]), results in #t
-- @
--
-- @
-- (any (partial eq 5) ["foo" 1 5]), throws an error since 'eq' cannot be applied to "foo"
-- @
any' :: Function
any' scope evalExpr args =
  case args of
    [F f, L l] -> P . Bool <$> foldr (evalAndCompare f) (return False) l
    _          -> throwEvaluationError EEInvalidArguments
  where
    evalAndCompare f x acc = do
      r <- f scope evalExpr [x]
      case r of
        P (Bool True)  -> return True
        P (Bool False) -> acc
        _              -> throwEvaluationError EEInvalidArguments
