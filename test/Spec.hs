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
      r <- run $ runHandler (valid x op y)
      assert $ r == Right (x `f` y)


wai :: Spec
wai =
  with (return app) $ do
    describe "GET /add" $ do
      it "2 + 3" $ do
        get "/2/add/3" `shouldRespondWith` "5.0" { matchStatus = 200 }
    describe "GET /div" $ do
      it "3 / 2" $ do
        get "/3/div/2" `shouldRespondWith` "1.5" { matchStatus = 200 }
      it "2 / 0" $ do
        get "/2/div/0" `shouldRespondWith` "null" { matchStatus = 200 }
    describe "GET invalid" $ do
      it "bad argument" $ do
        get "/2/add/abc" `shouldRespondWith` 400
      it "bad request" $ do
        get "/bad/bad" `shouldRespondWith` 400


spec :: Spec
spec = do
  opEq
  wai

main :: IO ()
main = hspec spec
