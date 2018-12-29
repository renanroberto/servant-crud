module Main where

import Server

port :: Int
port = 8080

main :: IO ()
main =
  putStrLn ("Starting Server on port " ++ show port)
  >> webAppEntry port
