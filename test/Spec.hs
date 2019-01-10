{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Servant (runHandler)


newtype Op = Op { fromOp :: (String, Double -> Double -> Double)}

instance Show Op where
  show (Op (op, _)) = op

instance Arbitrary Op where
  arbitrary =
    elements . fmap Op $ [
        ("add", (+))
      , ("sub", (-))
      , ("mul", (*))
      , ("div", (/))
      ]

opEq :: Spec
opEq = 
  it "op equivalence" $ do
    property $ \(Op (op, f)) x y -> monadicIO $ do
      r <- run $ runHandler (valid op x y)
      assert $ r == Right (x `f` y)


wai :: Spec
wai =
  with (return app) $ do
    describe "GET /add" $ do
      it "responds with 200" $ do
        get "/add/2/3" `shouldRespondWith` 200
      it "responds with result" $ do
        get "/add/2/3" `shouldRespondWith` "5.0"
      it "responds with 200" $ do
        get "/div/2/0" `shouldRespondWith` 200
      it "responds with 400" $ do
        get "/add/2/abc" `shouldRespondWith` 400
      it "responds with 400" $ do
        get "/wrong" `shouldRespondWith` 400


spec :: Spec
spec = do
  opEq
  wai

main :: IO ()
main = hspec spec
