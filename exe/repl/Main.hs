module Main
    ( main
    ) where

import qualified MDRN.Language.Scope as Scope
import           MDRN.Module.Base    (clientModule)
import           MDRN.REPL.Builder   (simpleREPL)
import           MDRN.REPL.Options   (defaultParserConfig)
import qualified Pouch.Time.Date     as Date

main :: IO ()
main = do
  year <- Date.currentLocalYear
  simpleREPL (defaultParserConfig year) $ Scope.fromMap clientModule
