{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpClient (get, postForm, writeCookie) where

import Control.Monad.IO.Class (liftIO)
import Data.Convertible.Utf8 (convert)
import Data.Convertible.Utf8.Internal (LazyByteString, Text)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Req
  ( GET (..),
    HttpBody,
    NoReqBody (..),
    Option,
    POST (..),
    ReqBodyUrlEnc (..),
    Url,
    (=:),
  )
import qualified Network.HTTP.Req as Req
import Text.Read (readMaybe)

type Form = [(Text, Text)]

-- UrlとFormを引数にレスポンスをTextとして返す
postForm :: Url schema -> ReqBodyUrlEnc -> IO Text
postForm url form = Req.runReq Req.defaultHttpConfig $ do
  r <-
    reqWithSession
      POST
      url
      form
      Req.lbsResponse
      mempty
  return $ convert (Req.responseBody r)

-- Cookie(optional)とUrlを引数にレスポンスをTextとして返す
get :: Url schema -> IO Text
get url = Req.runReq Req.defaultHttpConfig $ do
  r <-
    reqWithSession
      GET
      url
      NoReqBody
      Req.lbsResponse
      mempty
  return $ convert (Req.responseBody r)

-- cookieをFile管理するReq
reqWithSession ::
  ( Req.MonadHttp m,
    Req.HttpMethod method,
    Req.HttpBody body,
    Req.HttpResponse response,
    Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body)
  ) =>
  method ->
  Url scheme ->
  body ->
  Proxy response ->
  Option scheme ->
  m response
reqWithSession method url body responseType option = do
  cookie <- liftIO readCookie

  r <-
    Req.req
      method
      url
      body
      responseType
      (option <> Req.cookieJar cookie)

  liftIO $ writeCookie (Req.responseCookieJar r)
  return r

cookiePath :: FilePath
cookiePath = "./cookie"

-- cookieをcookiePathから読み込む
readCookie :: IO CookieJar
readCookie = readCookieWithPath cookiePath

-- cookieをcookiePathに書き込む
writeCookie :: CookieJar -> IO ()
writeCookie = writeCookieWithPath cookiePath

-- cookieを読み込む
-- readに失敗したらcookieのmemptyを返す
readCookieWithPath :: FilePath -> IO CookieJar
readCookieWithPath path = do
  appendFile path mempty
  cookie <- readMaybe <$> readFile path
  pure (fromMaybe mempty cookie)

-- cookieを書き込む
writeCookieWithPath :: FilePath -> CookieJar -> IO ()
writeCookieWithPath path cookie = do
  writeFile path (show cookie)
