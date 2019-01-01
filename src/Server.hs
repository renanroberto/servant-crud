{-# LANGUAGE OverloadedStrings, DataKinds, GADTs, KindSignatures,
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


type API = LessonAPI

type LessonAPI =  "lesson" :> QueryParam "sort" LessonSort :> Get '[JSON] [Lesson]
      :<|> "lesson" :> Capture "id" Int :> Header "Authorization" String :> Throws ResponseErr :> Get '[JSON] Lesson
      :<|> "lesson" :> ReqBody '[JSON] Lesson :> Post '[JSON] Lesson


data LessonSort = LessonByTitle | LessonByDate

instance FromHttpApiData LessonSort where
  parseQueryParam "title" = Right LessonByTitle
  parseQueryParam "date" = Right LessonByDate
  parseQueryParam _ = Left "Invalid Param. The availables params are \"title\" and \"date\""


data ResponseErr = NotFound | BadRequest | Unauthorized

instance ErrStatus ResponseErr where
  toErrStatus BadRequest = status400
  toErrStatus Unauthorized = status401
  toErrStatus NotFound = status404

instance ToJSON ResponseErr where
  toJSON BadRequest = toJSON ("This is not a valid lesson id" :: String)
  toJSON Unauthorized = toJSON ("You do not have permission" :: String)
  toJSON NotFound = toJSON ("Lesson not found" :: String)


{-- QueryBuilder experimet --}
class QueryBuilder a where
  toQuery :: a -> String

data Sort = SortTitle | SortDate | NoSort
data Order = Asc | Desc | NoOrder
data Limit = Limit Int | NoLimit

data QuerySelect where
  NoQuery :: QuerySelect
  Select :: Sort -> Order -> Limit -> QuerySelect

instance QueryBuilder Sort where
  toQuery NoSort = ""
  toQuery SortDate = " order by date "
  toQuery SortTitle = " order by title "

instance QueryBuilder Order where
  toQuery NoOrder = ""
  toQuery Asc = " asc "
  toQuery Desc = " desc "

instance QueryBuilder Limit where
  toQuery NoLimit = ""
  toQuery (Limit n) = " limit " ++ show n ++ " "

instance QueryBuilder QuerySelect where
  toQuery NoQuery = "select * from lessons"
  toQuery (Select sort order limit) =
    toQuery NoQuery <>
    toQuery sort <>
    toQuery order <>
    toQuery limit
{-- QueryBuilder experimet --}


dropPrefix :: String -> [a] -> [a]
dropPrefix str = drop (length str)


data Lesson = Lesson
  { lesson_id :: Maybe Int
  , lesson_title :: String
  , lesson_date :: Day
  } deriving Generic

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


{-- DB example --}
database :: IO ()
database = do
  conn <- open "crud.db"
  execute_ conn "create table if not exists lessons (id integer primary key, title varchar(50), date date)"
  execute conn "insert into lessons (title, date) values (?, ?)" ("análise" :: String, "2019-12-12" :: String) 
  execute conn "insert into lessons (title, date) values (?, ?)" ("geometria" :: String, "2018-12-12" :: String) 
  lesson <- query_ conn "select id, title, date from lessons" :: IO [Lesson]
  close conn
{-- DB example --}


listLessonsDB :: IO [Lesson]
listLessonsDB = do
  conn <- open "crud.db"
  let queryString = "select * from lessons" :: Query
  lessons <- query_ conn queryString :: IO [Lesson]
  close conn
  return lessons

getLessonDB :: Int -> IO (Maybe Lesson)
getLessonDB _id = do
  conn <- open "crud.db"
  mlesson <- query conn "select * from lessons where id = ?" [_id] :: IO [Lesson]
  close conn
  return $ if null mlesson then Nothing else Just (head mlesson)

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


listLessons :: Maybe LessonSort -> Handler [Lesson]
listLessons (Just LessonByTitle) = liftIO $ listLessonsDB
listLessons (Just LessonByDate) = liftIO $ listLessonsDB
listLessons Nothing = liftIO $ listLessonsDB

getLesson :: Int -> Maybe String -> Handler (Envelope '[ResponseErr] Lesson)
getLesson _ Nothing = pureErrEnvelope Unauthorized
getLesson _id _ = do
  mlesson <- liftIO $ getLessonDB _id
  case mlesson of
    Nothing -> pureErrEnvelope NotFound
    Just lesson -> pureSuccEnvelope lesson

addLesson :: Lesson -> Handler Lesson
addLesson = liftIO . insertLessonDB

server :: Server API
server = listLessons
    :<|> getLesson
    :<|> addLesson
  
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

webAppEntry :: Int -> IO ()
webAppEntry port = run port app

