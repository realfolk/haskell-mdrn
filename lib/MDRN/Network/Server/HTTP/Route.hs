module MDRN.Network.Server.HTTP.Route
    ( Action (..)
    , Route
    , find
    ) where

import           Control.Monad                     (join, msum)
import           MDRN.Data                         (Data)
import           MDRN.Network.Server.HTTP.Request  (Request)
import           MDRN.Network.Server.HTTP.Response (Response)

type Route = Request -> Maybe (Maybe Action)

data Action
  = RunProgram Data (Either Data Data -> Action)
  -- ^ Run a MDRN program. The function is used decide how to continue if the program runs successfully or fails.
  | Impure (IO Action)
  -- ^ Run arbitrary IO.
  | Respond Response
  -- ^ Terminate with a response.

find :: Request -> [Route] -> Maybe Action
find request = msum . map (join . ($ request))
