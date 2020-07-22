module Cli.Result
  ( Result,
    runResult,
    throwError,
    maybeToResult,
    eitherToResult,
    liftIO,
    maybeToMonadThrow,
    eitherToMonadThrow,
    module Control.Exception.Safe
  )
where

import Control.Arrow
import Control.Exception.Safe
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (MonadIO)
import Data.Either.Combinators (maybeToRight)
import Data.Text as T

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

maybeToMonadThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeToMonadThrow e = throw e `maybe` return

eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow = throw ||| return
