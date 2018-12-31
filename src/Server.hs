{-# LANGUAGE OverloadedStrings, DataKinds, GADTs,
    TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Server (webAppEntry) where

import Data.List
import Data.Time
import Data.Aeson
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Servant hiding (Unauthorized)
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Database.SQLite.Simple


type API = RootAPI :<|> LessonAPI

type RootAPI = Get '[JSON] [Endpoint]
 
type LessonAPI =  "lesson" :> QueryParam "sort" LessonSort :> Get '[JSON] [Lesson]
      :<|> "lesson" :> Capture "id" Int :> Header "Authorization" String :> Throws ResponseErr :> Get '[JSON] Lesson
      :<|> "lesson" :> ReqBody '[JSON] Lesson :> Post '[JSON] Lesson


type Endpoint = String


data ResponseErr = NotFound | BadRequest | Unauthorized

instance ErrStatus ResponseErr where
  toErrStatus NotFound = status404
  toErrStatus BadRequest = status400
  toErrStatus Unauthorized = status401

instance ToJSON ResponseErr where
  toJSON NotFound = toJSON ("Lesson not found" :: String)
  toJSON BadRequest = toJSON ("This is not a valid lesson id" :: String)
  toJSON Unauthorized = toJSON ("You do not have permission" :: String)


data LessonSort = LessonByTitle | LessonByDate

instance FromHttpApiData LessonSort where
  parseQueryParam param =
    case param of
      "title" -> Right LessonByTitle
      "date" -> Right LessonByDate
      _ -> Left "Invalid Param. The availables params are \"title\" and \"date\""


data Lesson = Lesson
  { lesson_id :: Maybe Int
  , lesson_title :: String
  , lesson_date :: Day
  } deriving (Show, Generic)

dropPrefix :: String -> [a] -> [a]
dropPrefix str = drop (length str)

instance ToJSON Lesson where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "lesson_" }

instance FromJSON Lesson where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "lesson_" }


instance ToRow Lesson where
  toRow (Lesson _id _title _date) = toRow (_id, _title, _date)

instance FromRow Lesson where
  fromRow = Lesson <$> field <*> field <*> field


{-- Real DB example --}
database :: IO ()
database = do
  conn <- open "crud.db"
  execute_ conn "create table if not exists lessons (id integer primary key, title varchar(50), date date)"
  execute conn "insert into lessons (title, date) values (?, ?)" ("análise" :: String, "2019-12-12" :: String) 
  execute conn "insert into lessons (title, date) values (?, ?)" ("geometria" :: String, "2018-12-12" :: String) 
  lesson <- query_ conn "select id, title, date from lessons" :: IO [Lesson]
  close conn
  print lesson


listLessonsDB :: IO [Lesson]
listLessonsDB = do
  conn <- open "crud.db"
  lessons <- query_ conn "select * from lessons limit 100" :: IO [Lesson]
  close conn
  return lessons

insertLessonDB :: Lesson -> IO Lesson
insertLessonDB lesson = do
  conn <- open "crud.db"
  execute conn "insert into lessons (title, date) values (?, ?)"
    ( lesson_title lesson
    , lesson_date lesson
    )
  [savedLesson] <- query_ conn "select * from lessons order by id desc limit 1" :: IO [Lesson]
  close conn
  return savedLesson
{-- Real DB example --}

{-- Fake DB --}
lessons :: [Lesson]
lessons =
  [ Lesson (Just 1) "Mecanica Quantica" (fromGregorian 2018 12 24)
  , Lesson (Just 2) "Teoria dos Numeros" (fromGregorian 2018 11 26)
  ]

endpoints :: [Endpoint]
endpoints =
  [ "/lesson[?sort=(title|date)]"
  , "/lesson/<id>"
  ]
{-- Fake DB --}


root :: Handler [Endpoint]
root = return endpoints


-- sortLesson :: Ord a => (Lesson -> a) -> [Lesson] -> [Lesson]
-- sortLesson prop = sortBy compareProps
--   where compareProps :: Lesson -> Lesson -> Ordering
--         compareProps x y = compare (prop x) (prop y)
-- 
-- 
-- listLessons :: Maybe LessonSort -> Handler [Lesson]
-- listLessons (Just LessonByTitle) = return (sortLesson lesson_title lessons)
-- listLessons (Just LessonByDate) = return (sortLesson lesson_date lessons)
-- listLessons Nothing = return lessons

listLessons :: Maybe LessonSort -> Handler [Lesson]
listLessons _ = liftIO $ listLessonsDB

getLesson :: Int -> Maybe String -> Handler (Envelope '[ResponseErr] Lesson)
getLesson _ Nothing = pureErrEnvelope Unauthorized
getLesson lid _ =
  if lid > 0
     then case find (\l -> lesson_id l == Just lid) lessons of
            Nothing -> pureErrEnvelope NotFound
            Just lesson -> pureSuccEnvelope lesson
     else pureErrEnvelope BadRequest


addLesson :: Lesson -> Handler Lesson
addLesson = liftIO . insertLessonDB

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

