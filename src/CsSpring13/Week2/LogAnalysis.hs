{-# OPTIONS_GHC -Wall #-}
module CsSpring13.Week2.LogAnalysis where
import CsSpring13.Week2.Log

parseMessage :: String -> LogMessage
parseMessage xs
            | head xs == 'E' = parseHelper (Error (read (head ws))) (tail ws)
            | head xs == 'I' = parseHelper Info ws
            | head xs == 'W' = parseHelper Warning ws
            | otherwise = Unknown xs
        where
            ws = words (drop 2 xs)

parseHelper :: MessageType -> [String] -> LogMessage
parseHelper a ws = LogMessage a stamp info
        where
            stamp = read (head ws)::TimeStamp
            info = unwords (drop 1 ws)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node left n right)
        | timeStamp m <= timeStamp n = Node (insert m left) n right
        | otherwise = Node left n (insert m right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right


isSeverError :: Int -> LogMessage -> Bool
isSeverError m (LogMessage (Error n) _ _) = n >= m
isSeverError _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map message $ filter (isSeverError 50) $ (inOrder . build) xs
