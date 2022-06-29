module MDRN.Language.Map
    ( Map
    , empty
    , fromList
    , insert
    , lookup
    , toList
    , tryDelete
    , tryInsert
    , unwrap
    , wrap
    ) where

import qualified Data.Map as M
import           Prelude  hiding (lookup)

newtype Map k v
  = Map (M.Map k v)
  deriving (Eq, Show)

-- * Constructors

empty :: Map k v
empty = Map M.empty

fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . M.fromList

wrap :: M.Map k v -> Map k v
wrap = Map

-- * Operations

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m) = M.lookup k m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m) = Map $ M.insert k v m

tryInsert :: Ord k => k -> v -> Map k v -> Maybe (Map k v)
tryInsert k v (Map m) =
  if M.member k m
     then Nothing
     else Just $ Map $ M.insert k v m

tryDelete :: Ord k => k -> Map k v -> Maybe (Map k v)
tryDelete k (Map m) =
  if M.member k m
     then Just $ Map $ M.delete k m
     else Nothing

-- * Converters

toList :: Map k v -> [(k, v)]
toList (Map m) = M.toList m

unwrap :: Map k v -> M.Map k v
unwrap (Map m) = m
