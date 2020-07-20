module Cli.Result where

import qualified Control.Monad.Except    as E
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Either.Combinators (maybeToRight)
import           Data.Text               as T

type Result a = E.ExceptT T.Text IO a

runResult :: Result a -> IO (Either Text a)
runResult = E.runExceptT

throwError :: Text -> Result a
throwError = E.throwError

maybeToResult :: Maybe a -> Text -> Result a
maybeToResult maybe text = E.ExceptT $ pure $ maybeToRight text maybe

eitherToResult :: Either Text a -> Result a
eitherToResult = E.ExceptT . pure

liftIO :: MonadIO m => IO a -> m a
liftIO = E.liftIO