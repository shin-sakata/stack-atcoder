module Settings where

import System.Directory (getHomeDirectory) 
import System.FilePath ((</>))

getSettingsDir :: IO FilePath
getSettingsDir = do
  home <- getHomeDirectory
  pure (home </> ".stack-atcoder")
