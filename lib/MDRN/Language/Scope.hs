module MDRN.Language.Scope
    ( Scope
    , empty
    , fromMap
    , fromMaps
    , lookup
    , push
    , toMaps
    , tryBind
    , tryUnbind
    ) where

import           Control.Applicative ((<|>))
import           MDRN.Language.Map   (Map)
import qualified MDRN.Language.Map   as M
import           Prelude             hiding (lookup)

newtype Scope n v
  = Scope [Map n v]

-- * Constructors

empty :: Scope n v
empty = Scope []

fromMap :: Map n v -> Scope n v
fromMap m = Scope [m]

fromMaps :: [Map n v] -> Scope n v
fromMaps = Scope

-- * Operations

tryBind :: Ord n => n -> v -> Scope n v -> Maybe (Scope n v)
tryBind n v (Scope [])        = Scope . (: []) <$> M.tryInsert n v M.empty
tryBind n v (Scope (m : ms) ) = Scope . (: ms) <$> M.tryInsert n v m

push :: Map n v -> Scope n v -> Scope n v
push m (Scope ms) = Scope (m : ms)

lookup :: Ord n => n -> Scope n v -> Maybe v
lookup n (Scope ms) = lookupHelper n ms

lookupHelper :: Ord n => n -> [Map n v] -> Maybe v
lookupHelper  _ []      = Nothing
lookupHelper n (m : ms) = M.lookup n m <|> lookupHelper n ms

tryUnbind :: Ord n => n -> Scope n v -> Maybe (Scope n v)
tryUnbind n (Scope ms) = Scope <$> tryUnbindHelper n ms

tryUnbindHelper :: Ord n => n -> [Map n v] -> Maybe [Map n v]
tryUnbindHelper _ [] = Nothing
tryUnbindHelper n (m : ms) =
  case M.tryDelete n m of
    Nothing -> (m :) <$> tryUnbindHelper n ms
    Just m' -> Just $ m' : ms

-- * Converters

toMaps :: Scope n v -> [Map n v]
toMaps (Scope ms) = ms
