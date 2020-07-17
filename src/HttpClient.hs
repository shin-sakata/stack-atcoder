{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

module HttpClient where

import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (LazyByteString, Text)
import           Data.Proxy                     (Proxy)
import           Network.HTTP.Client            (CookieJar)
import           Network.HTTP.Req               (GET (..), HttpBody,
                                                 NoReqBody (..), Option,
                                                 POST (..), ReqBodyUrlEnc (..),
                                                 Url, (=:))
import qualified Network.HTTP.Req               as Req

type Form = [(Text, Text)]

-- Cookie(optional)とUrlとFormを引数にレスポンスをTextとして返す
postForm :: Maybe CookieJar -> Url schema -> Form -> IO Text
postForm cookie url form = Req.runReq Req.defaultHttpConfig $ do
  r <- Req.req
         POST
         url
         payload
         Req.lbsResponse
         schema
  return $ convert $ Req.responseBody r
  where
    -- Form型をReqBodyUrlEncに変換
    payload :: ReqBodyUrlEnc
    payload = ReqBodyUrlEnc $ foldl (\l (k, v) -> l <> (k =: v)) ("" =: ("" :: Text)) form

    -- cookieをOption schemaに変換
    schema :: Option schema
    schema = maybe mempty Req.cookieJar cookie


-- Cookie(optional)とUrlを引数にレスポンスをTextとして返す
get :: Maybe CookieJar -> Url schema -> IO Text
get cookie url = Req.runReq Req.defaultHttpConfig $ do
  r <- Req.req
         GET
         url
         NoReqBody
         Req.lbsResponse
         schema
  return $ convert $ Req.responseBody r
  where
    -- cookieをOption schemaに変換
    schema :: Option schema
    schema = maybe mempty Req.cookieJar cookie
