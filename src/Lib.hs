{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (
    startApp
  , app
  , valid
  ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant


type API
  = Capture "op" String :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] Double
  :<|> CaptureAll "invalid" String :> Get '[JSON] NoContent


server :: Server API
server = valid :<|> invalid

valid :: String -> Double -> Double -> Handler Double
valid op x y = 
  case op of
    "add" -> return $ x + y
    "sub" -> return $ x - y
    "mul" -> return $ x * y
    "div" -> return $ x / y
    _ -> throwError err400 { errBody = "Non-supported operation" }

invalid :: [String] -> Handler NoContent
invalid _ =
  throwError err400 { errBody = "Invalid arguments" }


app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 app
