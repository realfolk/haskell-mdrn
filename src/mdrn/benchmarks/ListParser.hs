{-# LANGUAGE OverloadedStrings #-}

module Main
    where

import           Criterion.Main
import           Data.Either      (isRight)
import           MDRN.Data.Parser

main =
  defaultMain
    [ bgroup "parseData"
        [ bench "10" $ nf parse "(((((((((())))))))))"
        , bench "15" $ nf parse "((((((((((((((()))))))))))))))"
        , bench "20" $ nf parse "(((((((((((((((((((())))))))))))))))))))"
        , bench "30" $ nf parse "(((((((((((((((((((((((((((((())))))))))))))))))))))))))))))"
        , bench "100" $ nf parse "(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"
        ]
    ]

parse =
  isRight . parseData

