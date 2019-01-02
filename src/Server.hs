{-# LANGUAGE DataKinds, TypeOperators #-}

module Server (webAppEntry) where

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Lesson

type API = LessonAPI


server :: Server API
server = lessonHandlers
  
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

webAppEntry :: Int -> IO ()
webAppEntry port = run port app

