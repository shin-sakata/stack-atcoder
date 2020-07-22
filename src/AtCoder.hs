{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AtCoder
  ( login,
    submit,
    clearSession,
  )
where

import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified AtCoder.HttpClient as HttpClient
import qualified AtCoder.Scrape as Scrape
import Cli.Result
import Data.Convertible.Utf8 (convert)
import Data.Convertible.Utf8.Internal (Text)
import Data.Either.Combinators (maybeToRight)
import Network.HTTP.Req
  ( ReqBodyUrlEnc (..),
    Scheme (Https),
    Url,
    https,
    (/:),
    (=:),
  )

-- AtCoder endpoint
endpoint :: Url 'Https
endpoint = https "atcoder.jp"

type UserName = Text

type Password = Text

-- Login
login :: (MonadThrow m, MonadIO m) => UserName -> Password -> m ()
login userName password = do
  liftIO clearSession
  document <-
    liftIO $
      HttpClient.get
        loginEndpoint

  csrfToken <- Scrape.getCsrfToken document

  let form =
        ReqBodyUrlEnc $
          ("username" =: userName)
            <> ("password" =: password)
            <> ("csrf_token" =: csrfToken)

  res <- liftIO $ HttpClient.postForm loginEndpoint form

  -- TODO サクセス以外は全てusernameとpasswordが違うというエラーを吐いているので修正すべき
  if Scrape.hasSuccess res
    then liftIO $ putStrLn "Login Success!!"
    else throwString "Username or Password is incorrect."
  where
    loginEndpoint :: Url 'Https
    loginEndpoint = endpoint /: "login"

-- Logout
clearSession :: IO ()
clearSession = HttpClient.writeCookie mempty

-- Submit
type ContestId = Text

type TaskId = Text

type SourceCode = Text

haskellLanguageId :: Text
haskellLanguageId = "4027"

submit :: (MonadThrow m, MonadIO m) => ContestId -> TaskId -> SourceCode -> m ()
submit contestId taskId sourceCode = do
  let taskScreenName = contestId <> "_" <> taskId
  let contestUrl = endpoint /: "contests" /: contestId /: "tasks" /: taskScreenName
  document <-
    liftIO $
      HttpClient.get
        contestUrl

  csrfToken <- Scrape.getCsrfToken document

  let form =
        ReqBodyUrlEnc $
          ("data.TaskScreenName" =: taskScreenName)
            <> ("data.LanguageId" =: haskellLanguageId)
            <> ("sourceCode" =: sourceCode)
            <> ("csrf_token" =: csrfToken)

  let submitUrl = endpoint /: "contests" /: contestId /: "submit"
  liftIO $ HttpClient.postForm submitUrl form
  pure ()
