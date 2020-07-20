{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Data.Text (Text)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import  qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO)

data Config = Config
  { dependencies :: [Text],
    template :: Text
  }
  deriving (Show, Generic)

instance Yaml.ToJSON Config
instance Yaml.FromJSON Config

configFile :: FilePath
configFile = "stack-atcoder.yaml"

getSettingsDir :: IO FilePath
getSettingsDir = do
  home <- getHomeDirectory
  pure (home </> ".stack-atcoder")

getConfig :: IO (Either Yaml.ParseException Config)
getConfig = do
  settingsDir <- getSettingsDir
  Yaml.decodeFileEither (settingsDir </> configFile)
