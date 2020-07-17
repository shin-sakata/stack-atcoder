{-# LANGUAGE OverloadedStrings #-}

module Scrape (getCsrfToken) where

import           Data.Convertible.Utf8.Internal (Text)
import           Text.HTML.Scalpel              (AttributeName (..),
                                                 AttributePredicate, Selector,
                                                 TagName (..), URL, (@:), (@=))
import qualified Text.HTML.Scalpel              as Scalpel

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
