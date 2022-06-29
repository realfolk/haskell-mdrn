{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.File.Text
    ( module'
    ) where

import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import qualified MDRN.Data.Parser       as Parser
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr     as Expr
import qualified MDRN.Language.Map      as Map

module' :: Map
module' =
  Map.fromList
    [ ("read", F read')
    , ("write", F write')
    , ("append", F append)
    , ("read-mdrn", F readMDRN)
    ]

-- | Read from the specified filepath and returns the contents as 'Text'.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
read' :: Function
read' _ _ args =
  case args of
    [P (Text fileName)] -> do
      result <-
        liftIO
        (try (readFile (T.unpack fileName)) :: IO (Either IOException String))
      either (const $ throwEvaluationError EEIOError ) (return . P . Text . T.pack) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Write the given 'Text' to the file at the specified path.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
write' :: Function
write' _ _ args =
  case args of
    [P (Text fileName), P (Text contents)] -> do
      result <-
        liftIO
        (try (writeFile (T.unpack fileName) (T.unpack contents)) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError ) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Append the given 'Text' to the file at the specified path.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
append :: Function
append _ _ args =
  case args of
    [P (Text fileName), P (Text contents)] -> do
      result <-
        liftIO
        (try (appendFile (T.unpack fileName) (T.unpack contents)) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError ) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Read the given file as MDRN `Data`.
readMDRN :: Function
readMDRN _ _ args =
  case args of
    [P (Text path)] -> do
      raw <-
        liftIO
        (try (readFile (T.unpack path)) :: IO (Either IOException String))
      either (const $ throwEvaluationError EEIOError ) parse raw
    _ -> throwEvaluationError EEInvalidArguments
  where
    parse raw =
      either
        (\e ->
           throwEvaluationError $
           EENotSerializable $
             "File does not contain valid MDRN: " <> T.pack (show e))
        (return . Expr.optimize)
        (Parser.parseData $ T.pack raw)
