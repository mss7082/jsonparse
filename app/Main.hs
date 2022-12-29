module Main (main) where

-- https://www.youtube.com/watch?v=IMlDZNWTurw
import Lib

main :: IO ()
main = do
  eitherUsers <- getUser
  case eitherUsers of
    Left e -> error e
    Right users -> do
      print $ map userName users
