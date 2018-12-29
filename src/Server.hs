{-# LANGUAGE OverloadedStrings, DataKinds, GADTs,
   TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Server (webAppEntry) where

import Servant
import Servant.Checked.Exceptions
import Data.List
import Data.Time
import Data.Aeson hiding (Error)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
 

type API = RootAPI :<|> LessonAPI

type RootAPI = Get '[JSON] [Endpoint]
 
-- TODO Get request headers
type LessonAPI =  "lesson" :> QueryParam "sort" LessonSort :> Get '[JSON] [Lesson]
      :<|> "lesson" :> Capture "id" Int :> Throws ResponseErr :> Get '[JSON] Lesson
      :<|> "lesson" :> ReqBody '[JSON] Lesson :> Post '[JSON] Lesson


type Endpoint = String


data ResponseErr = NotFound | BadRequest

instance ErrStatus ResponseErr where
  toErrStatus NotFound = status404
  toErrStatus BadRequest = status400

instance ToJSON ResponseErr where
  toJSON NotFound = toJSON ("Lesson not found" :: String)
  toJSON BadRequest = toJSON ("This is not a valid lesson id" :: String)


data LessonSort = LessonByTitle | LessonByDate

instance FromHttpApiData LessonSort where
  parseQueryParam param =
    case param of
      "title" -> Right LessonByTitle
      "date" -> Right LessonByDate
      _ -> Left "Invalid Param. The availables params are \"title\" and \"date\""


data Lesson = Lesson
  { lessonID :: Int
  , title :: String
  , date :: Day
  } deriving (Generic, ToJSON, FromJSON)


{-- Fake DB --}
lessons :: [Lesson]
lessons =
  [ Lesson 1 "Mecanica Quantica" (fromGregorian 2018 12 24)
  , Lesson 2 "Teoria dos Numeros" (fromGregorian 2018 11 26)
  ]

endpoints :: [Endpoint]
endpoints =
  [ "/lesson[?sort=(title|date)]"
  , "/lesson/<id>"
  ]
{-- Fake DB --}


root :: Handler [Endpoint]
root = return endpoints


sortLesson :: Ord a => (Lesson -> a) -> [Lesson] -> [Lesson]
sortLesson prop = sortBy compareProps
  where compareProps :: Lesson -> Lesson -> Ordering
        compareProps x y = compare (prop x) (prop y)


listLessons :: Maybe LessonSort -> Handler [Lesson]
listLessons (Just LessonByTitle) = return (sortLesson title lessons)
listLessons (Just LessonByDate) = return (sortLesson date lessons)
listLessons Nothing = return lessons

getLesson :: Int -> Handler (Envelope '[ResponseErr] Lesson)
getLesson lid =
  if lid > 0
     then case find (\l -> lessonID l == lid) lessons of
            Nothing -> pureErrEnvelope NotFound
            Just lesson -> pureSuccEnvelope lesson
     else pureErrEnvelope BadRequest

addLesson :: Lesson -> Handler Lesson
addLesson lesson = return lesson -- TODO


server :: Server API
server = root
    :<|> listLessons
    :<|> getLesson
    :<|> addLesson
  
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

webAppEntry :: Int -> IO ()
webAppEntry port = run port app

