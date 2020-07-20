module Cli.Result where

import qualified Control.Monad.Except as E
import Data.Text as T
import Data.Either.Combinators (maybeToRight)

type Result a = E.ExceptT T.Text IO a

runResult :: Result a -> IO (Either Text a)
runResult = E.runExceptT

throwError :: Text -> Result a
throwError = E.throwError

maybeToResult :: Maybe a -> Text -> Result a
maybeToResult maybe text = E.ExceptT $ pure $ maybeToRight text maybe

eitherToResult :: Either Text a -> Result a
eitherToResult = E.ExceptT . pure
