{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module AtCoder where

import           Data.Convertible.Utf8.Internal (Text)
import           HttpClient                     (Form)
import qualified HttpClient
import           Network.HTTP.Req               (Scheme (Https), Url, https,
                                                 (/:))
import qualified Scrape

-- AtCoder endpoint
endpoint :: Url 'Https
endpoint = https "atcoder.jp"

type UserName = Text

type Password = Text

login :: UserName -> Password -> IO (Either Text Text)
login userName password = do
  document <- HttpClient.get Nothing loginEndpoint
  let csrfToken = Scrape.getCsrfToken document
  -- TODO
  print csrfToken
  return $ Right ""
    where
      loginEndpoint :: Url 'Https
      loginEndpoint = endpoint /: "login"
