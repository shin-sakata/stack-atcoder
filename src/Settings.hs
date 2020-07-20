{-# LANGUAGE DeriveGeneric #-}

module Settings where

import           Cli.Result
import qualified Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import qualified Data.Yaml              as Yaml
import           GHC.Generics           (Generic)
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))

data Config = Config
  { dependencies :: [Text],
    template     :: Text
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

getConfig :: Result Config
getConfig = do
  settingsDir <- liftIO getSettingsDir
  Yaml.decodeFileThrow (settingsDir </> configFile)
