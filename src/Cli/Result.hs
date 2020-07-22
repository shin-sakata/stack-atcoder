module Cli.Result
  (
    module Control.Exception.Safe,
    maybeToMonadThrow,
    eitherToMonadThrow
  )
where

import           Control.Arrow
import           Control.Exception.Safe
import           Control.Monad.IO.Class (MonadIO)

maybeToMonadThrow :: MonadThrow m => String -> Maybe a -> m a
maybeToMonadThrow errMsg = throwString errMsg `maybe` return

eitherToMonadThrow :: MonadThrow m => Either String a -> m a
eitherToMonadThrow = throwString ||| return
