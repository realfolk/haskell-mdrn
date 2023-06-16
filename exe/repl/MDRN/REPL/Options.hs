{-# LANGUAGE RecordWildCards #-}

module MDRN.REPL.Options
    ( Mode (..)
    , Opt.Parser
    , Options
    , ParserConfig
    , defaultParserConfig
    , mode
    , parse
    ) where

import           Control.Applicative (optional)
import qualified Data.Text           as T
import qualified Options.Applicative as Opt
import qualified Pouch.Time          as Time

newtype Options
  = Options { mode :: Mode }

data Mode
  = Interactive
  | Program !T.Text
  | File !FilePath
  | Stdin

data ParserConfig
  = ParserConfig
      { header :: !String
      , footer :: !String
      }

defaultParserConfig :: Time.Years -> ParserConfig
defaultParserConfig year = ParserConfig "Evaluate MDRN with Prelude." $ "© " ++ show (fromEnum year) ++ " Real Folk Inc."

parse :: Opt.Parser (Options -> a) -> ParserConfig -> IO a
parse parseAdditionalOptions config = Opt.execParser parser
  where
    parser =
      Opt.info
        (Opt.helper <*> (parseAdditionalOptions <*> optionsParser))
        (help config)

help :: ParserConfig -> Opt.InfoMod a
help ParserConfig {..} =
  Opt.fullDesc <>
  Opt.header header <>
  Opt.progDesc description <>
  Opt.footer footer

description :: String
description =
  "Evaluate a MDRN expression. Specify an expression as a command-line argument (--expression EXPR), from stdin (--stdin), from a file (FILE), or interactively in a REPL (the default). MDRN is developed and maintained by Real Folk (https://realfolk.com)."

optionsParser :: Opt.Parser Options
optionsParser = Options <$> modeParser

modeParser :: Opt.Parser Mode
modeParser = getMode <$> optional exprParser <*> optional filePathParser <*> stdinParser
  where
    getMode (Just expr) Nothing False = Program expr
    getMode Nothing (Just path) False = File path
    getMode Nothing Nothing True      = Stdin
    getMode _ _ _                     = Interactive

exprParser :: Opt.Parser T.Text
exprParser =
  Opt.strOption $
    Opt.long "expression" <>
    Opt.short 'e' <>
    Opt.metavar "EXPR" <>
    Opt.help "Run the specified MDRN expression and exit the program."

filePathParser :: Opt.Parser FilePath
filePathParser =
  Opt.argument Opt.str $
    Opt.metavar "FILE" <>
    Opt.help "Optionally specify a file containing a MDRN expression, and exit the program upon its evaluation. This option is ignored if the \"--expression\" option is present."

stdinParser :: Opt.Parser Bool
stdinParser =
  Opt.flag False True $
    Opt.long "stdin" <>
    Opt.help "Read and evaluate a MDRN expression from stdin and exit the program."
