{-# LANGUAGE OverloadedStrings #-}

module Scrape (getCsrfToken, hasSuccess) where

import           Data.Convertible.Utf8.Internal (Text)
import           Data.Maybe                     (isJust)
import           Text.HTML.Scalpel              (AttributeName (..),
                                                 AttributePredicate, Selector,
                                                 TagName (..), URL, (@:), (@=))
import qualified Text.HTML.Scalpel              as Scalpel
import Debug.Trace

type Html = Text

-- htmlのテキストからcsrf_tokenを取得する処理
getCsrfToken :: Html -> Maybe Text
getCsrfToken html =
  Scalpel.scrapeStringLike html $ Scalpel.attr "value" selector
  where
    selector :: Selector
    selector =
      tag @: predicates

    tag :: TagName
    tag =
      TagString "input"

    predicates :: [AttributePredicate]
    predicates =
      [AttributeString "name" @= "csrf_token"]

-- |
--  Login成功しているかどうかの判定
--  div.alert-successが存在するか
--
--  <div class="alert alert-success alert-dismissible col-sm-12 fade in" role="alert" style="margin: 10px 0;">
--    <button type="button" class="close" data-dismiss="alert" aria-label="Close">
--      <span aria-hidden="true">×</span>
--    </button>
--    <span class="glyphicon glyphicon-ok-sign" aria-hidden="true"></span> ようこそ {user name} さん。
--  </div>
hasSuccess :: Html -> Bool
hasSuccess html =
  isJust $ Scalpel.scrapeStringLike html $ Scalpel.html selector
  where
    selector :: Selector
    selector =
      tag @: predicates

    tag :: TagName
    tag =
      TagString "div"

    predicates :: [AttributePredicate]
    predicates =
      [Scalpel.hasClass "alert-success"]
