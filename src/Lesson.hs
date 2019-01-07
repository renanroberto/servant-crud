{-# LANGUAGE OverloadedStrings, DataKinds, GADTs, KindSignatures,
    TypeOperators, DeriveGeneric, DeriveAnyClass #-}

module Lesson (LessonAPI, lessonHandlers) where

import Data.Time
import Data.Aeson
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Servant
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status
import Database.SQLite.Simple


type LessonAPI = "lesson" :>
  (    QueryParam "sort" LessonSort :> Get '[JSON] [Lesson]
  :<|> Capture "id" ID :> Throws ResponseErr :> Get '[JSON] Lesson
  :<|> ReqBody '[JSON] Lesson :> Post '[JSON] Lesson
  :<|> Capture "id" ID :> Header "Authorization" String :> Throws ResponseErr :> Delete '[JSON] String
  )


type ID = Int

data LessonSort = LessonByTitle | LessonByDate

instance FromHttpApiData LessonSort where
  parseQueryParam "title" = Right LessonByTitle
  parseQueryParam "date" = Right LessonByDate
  parseQueryParam _ = Left "Invalid Param. The availables params are \"title\" and \"date\""


data ResponseErr = NotFound | WithoutToken | InvalidToken

instance ErrStatus ResponseErr where
  toErrStatus NotFound = status404
  toErrStatus WithoutToken = status400
  toErrStatus InvalidToken = status400

instance ToJSON ResponseErr where
  toJSON NotFound = toJSON ("Lesson not found" :: String)
  toJSON WithoutToken = toJSON ("You need an authentication token" :: String)
  toJSON InvalidToken = toJSON ("This token is not valid" :: String)


dropPrefix :: String -> [a] -> [a]
dropPrefix str = drop (length str)


data Lesson = Lesson
  { lesson_id :: Maybe ID
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

getLessonDB :: ID -> IO (Maybe Lesson)
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

deleteLessonDB :: ID -> IO ()
deleteLessonDB _id = db $ \conn ->
  execute conn "DELETE FROM lessons WHERE id = ?" (Only _id)


listLessons :: Maybe LessonSort -> Handler [Lesson]
listLessons = liftIO . listLessonsDB

getLesson :: ID -> Handler (Envelope '[ResponseErr] Lesson)
getLesson _id = do
  mlesson <- liftIO $ getLessonDB _id
  case mlesson of
    Nothing -> pureErrEnvelope NotFound
    Just lesson -> pureSuccEnvelope lesson

addLesson :: Lesson -> Handler Lesson
addLesson = liftIO . insertLessonDB

deleteLesson :: ID -> Maybe String -> Handler (Envelope '[ResponseErr] String)
deleteLesson _ Nothing = pureErrEnvelope WithoutToken
deleteLesson _id (Just token) =
  (liftIO $ deleteLessonDB _id) >> pureSuccEnvelope "Lesson deleted"


lessonHandlers :: Server LessonAPI
lessonHandlers = listLessons
            :<|> getLesson
            :<|> addLesson
            :<|> deleteLesson

