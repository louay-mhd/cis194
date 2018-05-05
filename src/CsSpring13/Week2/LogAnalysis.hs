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
