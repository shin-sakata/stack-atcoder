module Cli.ClearSession (clearSession) where

import qualified AtCoder
import Control.Monad.Except (runExceptT)
import Data.Convertible.Utf8 (convert)

clearSession :: IO ()
clearSession = do
  result <- runExceptT AtCoder.clearSession
  case result of
    Right _ -> pure ()
    Left e -> putStrLn ("Error: Clear session failed: " <> convert e)
