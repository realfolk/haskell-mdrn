{-# LANGUAGE OverloadedStrings #-}

module MDRN.Metadata
    ( Metadata (..)
    , exprToMetadata
    ) where

import qualified Data.Text             as T
import qualified MDRN.Data             as Data
import           MDRN.Data.Decode      (FromData (..))
import qualified MDRN.Data.Decode      as Decode
import qualified MDRN.Data.Decode.List as DL
import           MDRN.Data.Encode      (ToData (..))
import           MDRN.Language.Expr
import qualified MDRN.Language.Map     as Map

-- * METADATA

data Metadata
  = FnMetadata Name
  | MapMetadata Name [Metadata]
  | ListMetadata Name [Metadata]
  | PrimMetadata Name Prim
  deriving (Show)

instance ToData Metadata where
  toData = encodeMetadata

encodeMetadata :: Metadata -> Data
encodeMetadata m =
  case m of
    FnMetadata name -> Data.list [Data.symbol "fn", Data.text name]
    MapMetadata name children -> Data.list [Data.symbol "map", Data.text name, Data.list $ map encodeMetadata children]
    ListMetadata name children -> Data.list [Data.symbol "list", Data.text name, Data.list $ map encodeMetadata children]
    PrimMetadata name p -> Data.list [Data.symbol "prim", Data.text name, Data.node p]

instance FromData Metadata where
  decoder = decodeMetadata

decodeMetadata :: Decode.Decoder Metadata
decodeMetadata =
  Decode.oneOf $ map DL.list
    [ decodeFn
    , decodeMap
    , decodeList
    , decodePrim
    ]
  where
    decodeFn = FnMetadata <$ DL.symbolEq "fn" <*> DL.item'
    decodeMap = MapMetadata <$ DL.symbolEq "map" <*> DL.item' <*> DL.item (Decode.list decodeMetadata)
    decodeList = ListMetadata <$ DL.symbolEq "list" <*> DL.item' <*> DL.item (Decode.list decodeMetadata)
    decodePrim = PrimMetadata <$ DL.symbolEq "prim" <*> DL.item' <*> DL.item Decode.prim

exprToMetadata :: Name -> Expr -> Metadata
exprToMetadata name expr =
  case expr of
    F _ -> FnMetadata name
    M m -> MapMetadata name $ mapContentsToMetadata (Map.toList m) []
    L l -> ListMetadata name $ map (exprToMetadata T.empty) l
    P p -> PrimMetadata name p
  where
    mapContentsToMetadata pairs acc =
      case pairs of
        ((k, v) : rest) -> mapContentsToMetadata rest $ exprToMetadata k v : acc
        []              -> reverse acc
