{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.File.ByteString
    ( module'
    ) where

import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified MDRN.Data.Parser       as Parser
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr     as Expr
import qualified MDRN.Language.Map      as Map
import           System.IO

module' :: Map
module' =
  Map.fromList
    [ ("read", F read')
    , ("read-chunked", F readChunked)
    , ("write", F write')
    , ("append", F append)
    , ("read-mdrn", F readMDRN)
    ]

-- | Read from the specified filepath and returns the contents as a 'ByteString'.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
read' :: Function
read' _ _ args =
  case args of
    [P (Text fileName)] -> do
      result <-
        liftIO
        (try (B.readFile (T.unpack fileName)) :: IO (Either IOException B.ByteString))
      either (const $ throwEvaluationError EEIOError) (return . P . ByteString) result
    _ -> throwEvaluationError EEInvalidArguments

chunkSize = 4096

-- | Read a file in chunks and apply the appropriate handler function.
-- Takes handler functions for (1) a chunk being read (2) the read being complete (no more chunks) (3) and the read failing.
-- If an IO error occurs or the read handler fails, the read fail handler will be called.
readChunked :: Function
readChunked scope evalExpr args =
  case args of
    [P (Text filePath), F chunkRead, F readComplete, F readFail] -> do
      handle <- liftIO $ getHandle (T.unpack filePath)
      readLoop handle chunkRead readComplete readFail
    _ -> throwEvaluationError EEInvalidArguments
  where
    getHandle path = openFile path ReadMode
    readLoop :: Handle -> Function -> Function -> Function -> Result Expr
    readLoop handle handleChunk handleSuccess handleError = do
      done <- liftIO $ hIsEOF handle
      if done
        then do
          liftIO $ hClose handle
          handleSuccess scope evalExpr []
        else do
          chunkEither <-
            liftIO
            (try $ B.hGet handle chunkSize :: IO (Either IOException B.ByteString))
          either
            (const $ handleError scope evalExpr [])
            (\chunk -> do
               handleChunk scope evalExpr [P (ByteString chunk)]
               readLoop handle handleChunk handleSuccess handleError)
            chunkEither

writeChunked :: Function
writeChunked scope evalExpr args = undefined

-- | Write the given 'ByteString' to the file at the specified path.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
write' :: Function
write' _ _ args =
  case args of
    [P (Text fileName), P (ByteString contents)] -> do
      result <-
        liftIO
        (try (B.writeFile (T.unpack fileName) contents) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Append the given 'ByteString' to the file at the specified path.
-- If an IO error occurs, it will be caught and converted to a MDRN IO error.
append :: Function
append _ _ args =
  case args of
    [P (Text fileName), P (ByteString contents)] -> do
      result <-
        liftIO
        (try (B.appendFile (T.unpack fileName) contents) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Read the given file as MDRN `Data`.
readMDRN :: Function
readMDRN _ _ args =
  case args of
    [P (Text path)] -> do
      raw <-
        liftIO
        (try (B.readFile (T.unpack path)) :: IO (Either IOException B.ByteString))
      either
        (const $ throwEvaluationError EEIOError )
        (parse . T.unpack . TE.decodeUtf8)
        raw
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
