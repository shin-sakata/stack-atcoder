module Cli.ClearSession (clearSession) where

import qualified AtCoder
import Data.Convertible.Utf8 (convert)
import Cli.Result

clearSession :: IO ()
clearSession = do
  AtCoder.clearSession
