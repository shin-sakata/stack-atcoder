{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AtCoder where

import Control.Monad.Except
import Data.Convertible.Utf8 (convert)
import Data.Convertible.Utf8.Internal (Text)
import Data.Either.Combinators (maybeToRight)
import qualified AtCoder.HttpClient as HttpClient
import Network.HTTP.Req
  ( ReqBodyUrlEnc (..),
    Scheme (Https),
    Url,
    https,
    (/:),
    (=:),
  )
import qualified AtCoder.Scrape as Scrape

-- AtCoder endpoint
endpoint :: Url 'Https
endpoint = https "atcoder.jp"

type UserName = Text

type Password = Text

login :: UserName -> Password -> Result ()
login userName password = do
  logout
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

  if Scrape.hasSuccess res
    then return ()
    else throwError "Login failed"
  where
    loginEndpoint :: Url 'Https
    loginEndpoint = endpoint /: "login"

logout :: Result ()
logout = liftIO $ HttpClient.writeCookie mempty

type Result a = ExceptT Text IO a

maybeToResult :: Maybe a -> Text -> Result a
maybeToResult maybe text = ExceptT $ pure $ maybeToRight text maybe

eitherToResult :: Either Text a -> Result a
eitherToResult = ExceptT . pure
