{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module AtCoder.HttpClient (get, postForm, writeCookie) where

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
import Turtle (shell, empty)
import Settings (getSettingsDir)
import System.FilePath ((</>))

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

getCookiePath :: IO FilePath
getCookiePath = do
  settingsDir <- getSettingsDir
  pure (settingsDir </> "cookie")

initCookie :: IO ()
initCookie = do
  settingsDir <- getSettingsDir
  cookiePath <- getCookiePath
  shell ("mkdir -p " <> convert settingsDir)  empty
  shell ("touch " <> convert cookiePath) empty
  pure ()

-- cookieをcookiePathから読み込む
readCookie :: IO CookieJar
readCookie = do
  cookiePath <- getCookiePath
  initCookie
  readCookieWithPath cookiePath

-- cookieをcookiePathに書き込む
writeCookie :: CookieJar -> IO ()
writeCookie cookie = do
  cookiePath <- getCookiePath
  initCookie
  writeCookieWithPath cookiePath cookie

-- cookieを読み込む
-- readに失敗したらcookieのmemptyを返す
readCookieWithPath :: FilePath -> IO CookieJar
readCookieWithPath path = do
  cookie <- readMaybe <$> readFile path
  pure (fromMaybe mempty cookie)

-- cookieを書き込む
writeCookieWithPath :: FilePath -> CookieJar -> IO ()
writeCookieWithPath path cookie = do
  writeFile path (show cookie)
