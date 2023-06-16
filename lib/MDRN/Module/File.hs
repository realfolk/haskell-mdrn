{-# LANGUAGE OverloadedStrings #-}

module MDRN.Module.File
    ( module'
    ) where

import           Control.Exception
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import           MDRN.Language.Expr
import qualified MDRN.Language.Map           as Map
import qualified MDRN.Module.File.ByteString as ByteStringSubModule
import qualified MDRN.Module.File.Text       as TextSubModule
import           System.Directory

module' :: Map
module' =
  Map.fromList
    [ ("delete", F delete')
    , ("rename", F rename)
    , ("text", M TextSubModule.module')
    , ("bytestring", M ByteStringSubModule.module')
    ]

-- | Delete the file with the given filepath.
-- Any IO errors are caught and converted to the MDRN equivalent.
--
-- @
-- (delete "files/temp1.txt")
-- @
delete' :: Function
delete' _ _ args =
  case args of
    [P (Text fileName)] -> do
      result <-
        liftIO
        (try (removeFile (T.unpack fileName)) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments

-- | Rename an existing file with the new provided filepath.
-- Any IO errors are caught and converted to the MDRN equivalent.
--
-- @
-- (rename "files/temp1.txt" "files/temp2.txt")
-- @
rename :: Function
rename _ _ args =
  case args of
    [P (Text old), P (Text new)] -> do
      result <- liftIO (try (renameFile (T.unpack old) (T.unpack new)) :: IO (Either IOException ()))
      either (const $ throwEvaluationError EEIOError) (const $ return $ P Unit) result
    _ -> throwEvaluationError EEInvalidArguments
