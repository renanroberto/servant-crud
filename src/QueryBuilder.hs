{-# LANGUAGE OverloadedStrings,
    DataKinds, GADTs, KindSignatures#-}

module QueryBuilder where

import Database.SQLite.Simple (Query)


class ToQuery a where
  toQuery :: a -> String

data Direction = ASC | DESC
  deriving Show


data QueryType = Base | Option

data QueryBuilder (t :: QueryType) where
  Select :: String -> QueryBuilder Base
  Insert :: String -> [String] -> QueryBuilder Base
  Order :: String -> Direction -> QueryBuilder Option
  Limit :: Int -> QueryBuilder Option
  (:<>) :: QueryBuilder Base -> QueryBuilder Option -> QueryBuilder Base


surround :: Char -> Char -> String -> String
surround left right str = [left] ++ str ++ [right]

parenthesis :: String -> String
parenthesis = surround '(' ')'

formatArgs :: [String] -> String
formatArgs [] = ""
formatArgs [s] = parenthesis s
formatArgs args = (parenthesis . concat) (xs ++ [x])
  where xs = fmap (++ ", ") (init args)
        x = last args

formatHoles :: [String] -> String
formatHoles [] = ""
formatHoles [_] = parenthesis "?"
formatHoles args = (parenthesis . concat) [if x < n then "?, " else "?" | x <- [1..n]]
  where n = length args


instance ToQuery (QueryBuilder t) where
  toQuery (Select table) = "SELECT * from " ++ table
  toQuery (Insert table args) =
    unwords ["INSERT INTO", table, formatArgs args, "VALUES", formatHoles args]
  toQuery (Order column direction) =
    unwords ["ORDER BY", column, show direction]
  toQuery (Limit n) = "LIMIT " ++ show n
  toQuery (q :<> q') = toQuery q ++ " " ++ toQuery q'

qtest :: QueryBuilder Base
qtest = Select "lessons" :<> Order "title" DESC :<> Limit 20

qtest' :: QueryBuilder Base
qtest' = Insert "lessons" ["title", "date"]
