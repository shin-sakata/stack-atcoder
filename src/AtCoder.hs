{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AtCoder
  ( login,
    Result,
    submit,
    clearSession,
  )
where

import qualified AtCoder.HttpClient as HttpClient
import qualified AtCoder.Scrape as Scrape
import Control.Monad.Except
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

type Result a = ExceptT Text IO a

-- AtCoder endpoint
endpoint :: Url 'Https
endpoint = https "atcoder.jp"

type UserName = Text

type Password = Text

-- Login
login :: UserName -> Password -> Result ()
login userName password = do
  clearSession
  document <-
    liftIO $
      HttpClient.get
        loginEndpoint

  csrfToken <-
    maybeToResult
      (Scrape.getCsrfToken document)
      ("cannot find csrf_token. URL: " <> convert (show loginEndpoint))

  let form =
        ReqBodyUrlEnc $
          ("username" =: userName)
            <> ("password" =: password)
            <> ("csrf_token" =: csrfToken)

  res <- liftIO $ HttpClient.postForm loginEndpoint form

  -- TODO サクセス以外は全てusernameとpasswordが違うというエラーを吐いているので修正すべき
  if Scrape.hasSuccess res
    then return ()
    else throwError "Username or Password is incorrect."
  where
    loginEndpoint :: Url 'Https
    loginEndpoint = endpoint /: "login"

-- Logout
clearSession :: Result ()
clearSession = liftIO $ HttpClient.writeCookie mempty

-- Submit
type ContestId = Text
type TaskId = Text
type SourceCode = Text

haskellLanguageId :: Text
haskellLanguageId = "4027"

submit :: ContestId -> TaskId -> SourceCode -> Result ()
submit contestId taskId sourceCode = do
  let taskScreenName = contestId <> "_" <> taskId
  let contestUrl = endpoint /: "contests" /: contestId /: "tasks" /: taskScreenName
  document <-
    liftIO $
      HttpClient.get
        contestUrl
  
  csrfToken <-
    maybeToResult
      (Scrape.getCsrfToken document)
      ("cannot find csrf_token. URL: " <> convert (show contestUrl))
  
  let form =
        ReqBodyUrlEnc $
          ("data.TaskScreenName" =: taskScreenName)
            <> ("data.LanguageId" =: haskellLanguageId)
            <> ("sourceCode" =: sourceCode)
            <> ("csrf_token" =: csrfToken)
  
  let submitUrl = endpoint /: "contests" /: contestId /: "submit"
  void $ liftIO $ HttpClient.postForm submitUrl form

maybeToResult :: Maybe a -> Text -> Result a
maybeToResult maybe text = ExceptT $ pure $ maybeToRight text maybe

eitherToResult :: Either Text a -> Result a
eitherToResult = ExceptT . pure
