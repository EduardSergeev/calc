{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /add" $ do
        it "responds with 200" $ do
            get "/add/2/3" `shouldRespondWith` 200
        it "responds with [User]" $ do
            get "/add/2/3" `shouldRespondWith` "5.0"
        it "responds with 400" $ do
            get "/add/2/abc" `shouldRespondWith` 400
