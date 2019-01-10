{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (
    startApp
  , app
  , valid
  ) where

import qualified Data.Map.Strict as M
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant


type API =
  Capture "x" Double :> Capture "op" String :> Capture "y" Double :> Get '[JSON] Double :<|>
  CaptureAll "invalid" String :> Get '[JSON] NoContent


server :: Server API
server =
  valid :<|>
  invalid

valid :: Double -> String -> Double -> Handler Double
valid x op y = 
  case M.lookup op ops of
    Just o -> return $ x `o` y
    Nothing -> throwError err400 { errBody = "Non-supported operation" }

ops :: M.Map String (Double -> Double -> Double)
ops = M.fromList [
    ("add", (+))
  , ("sub", (-))
  , ("mul", (*))
  , ("div", (/))
  ]
    
invalid :: [String] -> Handler NoContent
invalid _ =
  throwError err400 { errBody = "Invalid arguments" }


app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 app
