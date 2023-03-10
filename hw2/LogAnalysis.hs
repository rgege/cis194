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

-- Exercise 2.

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (Unknown _)        = (-1)
getTimeStamp (LogMessage _ t _) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node f t a)
  | (getTimeStamp m) < (getTimeStamp t) = Node (insert m f) t a
  | otherwise                           = Node f t (insert m a)

-- Exercise 3.

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4.

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node f m a) = inOrder f ++ [m] ++ inOrder a

-- Exercise 5.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . inOrder . build . filter getSeverity . filter getError
  where
    getString (LogMessage _ _ s) = s
    getString (Unknown _)        = ""
    getError (LogMessage (Error _) _ _) = True
    getError _                          = False
    getSeverity (LogMessage (Error n) _ _) = n > 50
    getSeverity _                          = False

-- Exercise 6.

iCan :: IO [String]
iCan = testWhatWentWrong parse whatWentWrong "error.log"
