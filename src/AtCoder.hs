{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AtCoder where

import           Text.HTML.Scalpel (AttributeName (..), AttributePredicate,
                                    Selector, TagName (..), URL, (@:), (@=))
import qualified Text.HTML.Scalpel as Scalpel

-- AtCoder endpoint
endpoint :: Scalpel.URL
endpoint = "https://atcoder.jp"

-- AtCoderのLoginページからCsrfTokenを取得する
getCsrfToken :: IO (Maybe String)
getCsrfToken = do
  Scalpel.scrapeURL endpoint $ Scalpel.attr "value" selector
  where
    url :: URL
    url = endpoint ++ "/login"

    selector :: Selector
    selector =
      tag @: predicates

    tag :: TagName
    tag =
      TagString "input"

    predicates :: [AttributePredicate]
    predicates =
      [AttributeString "name" @= "csrf_token"]
