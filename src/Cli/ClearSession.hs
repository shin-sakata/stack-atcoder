module Cli.ClearSession (clearSession) where

import qualified AtCoder
import           Cli.Exceptions
import           Data.Convertible.Utf8 (convert)

clearSession :: IO ()
clearSession = do
  AtCoder.clearSession
