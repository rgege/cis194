{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

-- Exercise 1.

parseMessage :: String -> LogMessage
parseMessage = go . words
  where
    go []      = Unknown ""
    go o@[_]   = Unknown (unwords o)
    go t@[_,_] = Unknown (unwords t)
    go xss@(x:y:z:xs)
      | x == "I" = LogMessage Info    (read y :: Int) (unwords (z:xs))
      | x == "W" = LogMessage Warning (read y :: Int) (unwords (z:xs))
      | x == "E" = LogMessage (Error (read y :: Int)) (read z :: Int) (unwords xs)
      | otherwise = Unknown (unwords xss)

parse :: String -> [LogMessage]
parse = go . lines
  where
    go []     = []
    go (x:xs) = parseMessage x : go xs
