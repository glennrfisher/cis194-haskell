----------------------------------------------
-- CIS 194, Homework 2
-- Author: Glenn R. Fisher
-- Date: March 30, 2016
----------------------------------------------

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parse a line from a log file.
--
-- > parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- > parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- > parseMessage "Incorrect format" == Unknown "Incorrect format"

parseMessage :: String -> LogMessage
parseMessage line =
    let wordList = words line in
    case wordList of
        ("I":ts:msg) -> LogMessage Info timestamp message
            where
                timestamp = read ts
                message = unwords msg
        ("W":ts:msg) -> LogMessage Warning timestamp message
            where
                timestamp = read ts
                message = unwords msg
        ("E":s:ts:msg) -> LogMessage (Error severity) timestamp message
            where
                severity = read s
                timestamp = read ts
                message = unwords msg
        _ -> Unknown message
            where
                message = unwords wordList

-- Parse all lines from a log file.
--
-- > parse "" == []
-- > parse "E 2 562 help help\nI 29 la la la"
--   == [LogMessage (Error 2) 562 "help help", LogMessage Info 29 "la la la"]

parse :: String -> [LogMessage]
parse logs =
    let lineList = lines logs in
    map parseMessage lineList

-- Insert a log message into a message tree that is ordered by timestamp.
--
-- If the timestamp of the given log message is the same as the timestamp of
-- a log message already stored in the message tree, then the collision will
-- be resolved by substituting the given log message in place of the one
-- already contained within the tree.
-- 
-- Since an Unknown log message does not have a timestamp, it will be ignored
-- and not inserted into the tree.
--
-- > insert (LogMessage (Error 2) 562 "help help") Leaf
--   == Node Leaf (LogMessage (Error 2) 562 "help help") Leaf
--
-- > insert (LogMessage Info 29 "la la la")
--          (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf)
--   == Node (Node Leaf (LogMessage Info 29 "la la la") Leaf)
--           (LogMessage (Error 2) 562 "help help")
--           Leaf
--
-- > insert (Unknown "Incorrect format") Leaf
--   == Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage messageTree =
    let (LogMessage _ messageTimestamp _) = logMessage in
    let (Node left node right) = messageTree in
    let (LogMessage _ nodeTimestamp _) = node in
    case compare messageTimestamp nodeTimestamp of
        LT -> Node (insert logMessage left) node right
        GT -> Node left node (insert logMessage right)
        EQ -> Node left logMessage right

-- Construct a message tree (ordered by timestamp) by successively inserting
-- log messages.
-- 
-- If the timestamp of any log messages collide, then only the log message
-- with the larger list index (subscript) will be present in the tree.
--
-- Since Unknown log messages do not have a timestamp, they are ignored and
-- not inserted into the tree.
--
-- > build [(LogMessage (Error 2) 562 "help help"),
--          (LogMessage Info 29 "la la la"),
--          (Unknown "Incorrect format")]
--   == Node Leaf
--           (LogMessage Info 29 "la la la")
--           (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Perform an in-order traversal of a message tree that is ordered by timestamp.
-- That is, produce a list of all log messages contained in the tree, sorted
-- by timestamp from smallest to largest.
-- 
-- > inOrder (Node Leaf
--                 (LogMessage Info 29 "la la la")
--                 (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf))
--   == [LogMessage Info 29 "la la la", LogMessage (Error 2) 562 "help help"]

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder messageTree =
    let (Node left logMessage right) = messageTree in
    inOrder left ++ [logMessage] ++ inOrder right

-- Return True if the given log message is an error with a severity greater
-- than or equal to the given severity threshold.
--
-- > isSevereError 50 (LogMessage (Error 2) 562 "help help") == False
-- > isSevereError  1 (LogMessage (Error 2) 562 "help help") == True
-- > isSevereError  1 (LogMessage Info 29 "la la la") == False

isSevereError :: Int -> LogMessage -> Bool
isSevereError severityThreshold logMessage =
    case logMessage of
        LogMessage (Error severity) _ _ -> severity >= severityThreshold
        _ -> False

-- Transforms an unsorted list of log messages into a list of messages
-- corresponding to any errors with a severity of 50 or greater, sorted
-- by timestamp.
--
-- > whatWentWrong [LogMessage (Error 2) 562 "help help",
--                  LogMessage Info 29 "la la la"]
--   == []
--
-- > whatWentWrong [LogMessage (Error 50) 562 "help help",
--                  LogMessage Info 29 "la la la"]
--   == ["help help"]

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages =
    let severeErrors = filter (isSevereError 50) logMessages in
    let messageTree = build severeErrors in
    let severeErrorsOrdered = inOrder messageTree in
    map (\(LogMessage _ _ message) -> message) severeErrorsOrdered
