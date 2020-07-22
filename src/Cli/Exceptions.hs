module Cli.Exceptions
  (
    module Control.Exception.Safe,
    maybeToMonadThrow,
    eitherToMonadThrow,
    MonadIO,
    liftIO
  )
where

import           Control.Arrow
import           Control.Exception.Safe
import           Control.Monad.IO.Class (MonadIO, liftIO)

maybeToMonadThrow :: MonadThrow m => String -> Maybe a -> m a
maybeToMonadThrow errMsg = throwString errMsg `maybe` return

eitherToMonadThrow :: MonadThrow m => Either String a -> m a
eitherToMonadThrow = throwString ||| return
