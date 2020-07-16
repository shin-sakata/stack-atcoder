{-# LANGUAGE BlockArguments #-}

module AtCoderSpec (spec) where

import Test.Hspec
import AtCoder
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "atcoder" $
    it "getCsrfToken is Just" do
      token <- getCsrfToken
      token `shouldSatisfy` isJust
