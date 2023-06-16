{-# LANGUAGE OverloadedStrings #-}

module MDRN.REPL.Builder
    ( repl
    , simpleREPL
    ) where

import           Control.Monad            (void, when)
import           Control.Monad.Trans      (lift)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified MDRN.Data                as Data
import qualified MDRN.Data.Decode         as Decode
import qualified MDRN.Data.Encode         as Encode
import qualified MDRN.Language.Evaluator  as Evaluator
import           MDRN.Language.Expr
import qualified MDRN.Language.Expr       as Expr
import qualified MDRN.Language.Map        as Map
import qualified MDRN.Language.Scope      as Scope
import           MDRN.REPL.Lib            (exprToText)
import qualified MDRN.REPL.Module         as REPLModule
import           MDRN.REPL.Options        (Mode (..), Options, Parser,
                                           ParserConfig)
import qualified MDRN.REPL.Options        as Options
import qualified System.Console.Haskeline as H
import           System.Exit              (exitFailure, exitSuccess)
import           System.IO                (Handle, IOMode (ReadMode), stderr,
                                           stdin, stdout, withFile)

-- * REPLs

repl :: Parser (Options -> a) -> (a -> Options) -> ParserConfig -> (a -> Scope) -> IO ()
repl parseAdditionalOptions getOptions config makeBaseScope = do
  allOptions <- Options.parse parseAdditionalOptions config
  start (getOptions allOptions) (makeREPLScope $ makeBaseScope allOptions)

simpleREPL :: ParserConfig -> Scope -> IO ()
simpleREPL config scope = repl (pure id) id config (const scope)

-- * Helpers

makeREPLScope :: Scope -> Scope
makeREPLScope =
  Scope.push (Map.fromList [("repl", M REPLModule.module')])

start :: Options -> Scope -> IO ()
start options scope = do
  case Options.mode options of
    Interactive -> H.runInputTBehavior H.preferTerm H.defaultSettings $ loop scope True
    Program rawData -> void $ exec True scope rawData
    File filePath -> withFile filePath ReadMode $ consumeHandleAndExec scope
    Stdin -> consumeStdinAndExec scope

loop :: Scope -> Bool -> H.InputT IO ()
loop scope showPrompt = do
  line <- H.getInputLine (if showPrompt then "> " else "")
  case line of
    Nothing ->
      -- Exit on Ctrl-D
      return ()
    Just input -> do
      -- Handle user input
      scope' <- lift $ exec False scope $ T.pack input
      loop scope' showPrompt

consumeStdinAndExec :: Scope -> IO ()
consumeStdinAndExec scope = consumeHandleAndExec scope stdin

consumeHandleAndExec :: Scope -> Handle -> IO ()
consumeHandleAndExec scope handle = void $ TIO.hGetContents handle >>= exec True scope

exec :: Bool -> Scope -> T.Text -> IO Scope
exec shouldExit scope input = do
  parsedExpr <- runResult $ parseData input
  case parsedExpr of
    Right v -> do
      result <- runResult $ Evaluator.evalExpr scope $ Expr.optimize v
      case result of
        Right (_, L (P (Symbol "scope"):scopeModules)) ->
          return $ Scope.fromMaps $ map (\(M module') -> module') scopeModules
        Right (_, expr) -> do
          TIO.hPutStrLn stdout $ exprToText expr
          when shouldExit exitSuccess
          return scope
        Left err -> do
          TIO.hPutStrLn stderr $ encodeError err
          when shouldExit exitFailure
          return scope
    Left e -> do
      TIO.hPutStrLn stderr $ encodeError e
      when shouldExit exitFailure
      return scope
  where
    encodeError = ("ERROR: " <>) . Data.toText . Encode.toData

parseData :: T.Text -> Result Data
parseData = either (const $ throwEvaluationError EEInvalidArguments) return . Decode.decodeText
