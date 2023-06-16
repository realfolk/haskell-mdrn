{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.Mustache
    ( module'
    ) where

import           Data.Aeson         ((.=))
import qualified Data.Aeson         as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Text.Lazy     as TZ
import qualified MDRN.Data          as Data
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr as Expr
import qualified MDRN.Language.Map  as Map
import           Text.Microstache   as Mustache

module' :: Map
module' =
  Map.fromList
    [ ("compile", F compile)
    , ("render", F render)
    ]

-- | Creates a mustache template function that accepts parameters.
compile :: Function
compile _ _ args =
  case args of
    [P (Text templateString)] -> do
      let compilationResult =
            compileMustacheText "MDRN Template" $ TZ.fromStrict templateString
      case compilationResult of
        Left _         -> throwEvaluationError EEInvalidArguments
        Right template -> return $ F $ render' template
    _ -> throwEvaluationError EEInvalidArguments

-- | Renders the given text to text using a mustache template and supplied parameters.
render :: Function
render scope evalExpr args =
  case args of
    [params@(M paramsMap), P (Text templateString)] -> do
      result <- compile scope evalExpr [P (Text templateString)]
      case result of
        F renderFn -> renderFn scope evalExpr [params]
        _          -> throwEvaluationError EEInvalidArguments
    _ -> throwEvaluationError EEInvalidArguments

-- | Helper function for rendering text given a mustache template and MDRN arguments.
render' :: Template -> Function
render' template scope evalExpr args =
  case args of
    [M paramsMap] -> do
      value <- mapToAesonValue paramsMap
      return $ Expr.optimize $ Data.text $ TZ.toStrict $ renderMustache template value
    _ -> throwEvaluationError EEInvalidArguments
  where
    mapToAesonValue m = Aeson.object <$> mapM tupleToPair (Map.toList m)
    tupleToPair t =
      case t of
        (name, P (Text value)) -> return $ Aeson.fromText name .= value
        _                      -> throwEvaluationError EEInvalidArguments
