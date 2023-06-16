{-# LANGUAGE OverloadedStrings #-}

module MDRN.REPL.Lib
    ( exprToText
    ) where

import qualified Data.Text          as T
import           MDRN.Language.Expr
import qualified MDRN.Prim          as Prim

exprToText :: Expr -> T.Text
exprToText expr =
  case expr of
    P p -> Prim.toText p
    L l -> "(" <> T.intercalate " " (map exprToText l) <> ")"
    M _ -> "Map"
    F _ -> "Function"
