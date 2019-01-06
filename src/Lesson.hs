{-# LANGUAGE OverloadedStrings, DataKinds, GADTs, KindSignatures,
    TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Lesson (LessonAPI, lessonHandlers) where

import Data.Time
import Data.Aeson
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Servant hiding (Unauthorized)
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status
import Database.SQLite.Simple


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

(<<) :: Monad m => m a -> m b -> m a
mx << my = mx >>= \x -> my >> return x

db :: (Connection -> IO a) -> IO a
db action =
  open "crud.db" >>= \conn ->
    action conn << close conn


listLessonsDB :: Maybe LessonSort -> IO [Lesson]
listLessonsDB Nothing = db $ \conn ->
  query_ conn "SELECT * FROM lessons"
listLessonsDB (Just LessonByTitle) = db $ \conn ->
  query_ conn "SELECT * FROM lessons ORDER BY title"
listLessonsDB (Just LessonByDate) = db $ \conn ->
  query_ conn "SELECT * FROM lessons ORDER BY date"

getLessonDB :: Int -> IO (Maybe Lesson)
getLessonDB _id = db $ \conn -> do
  lessons <- query conn "SELECT * FROM lessons WHERE id = ?" (Only _id) :: IO [Lesson]
  return $ if null lessons then Nothing else Just (head lessons)

insertLessonDB :: Lesson -> IO Lesson
insertLessonDB lesson = db $ \conn -> do
  execute conn "INSERT INTO lessons (title, date) VALUES (?, ?)"
    ( lesson_title lesson
    , lesson_date lesson
    )
  [savedLesson] <- query_ conn "SELECT * FROM lessons ORDER BY id DESC LIMIT 1" :: IO [Lesson]
  return savedLesson


listLessons :: Maybe LessonSort -> Handler [Lesson]
listLessons = liftIO . listLessonsDB

getLesson :: Int -> Maybe String -> Handler (Envelope '[ResponseErr] Lesson)
getLesson _ Nothing = pureErrEnvelope Unauthorized
getLesson _id _ = do
  mlesson <- liftIO $ getLessonDB _id
  case mlesson of
    Nothing -> pureErrEnvelope NotFound
    Just lesson -> pureSuccEnvelope lesson

addLesson :: Lesson -> Handler Lesson
addLesson = liftIO . insertLessonDB


lessonHandlers :: Server LessonAPI
lessonHandlers = listLessons
            :<|> getLesson
            :<|> addLesson

