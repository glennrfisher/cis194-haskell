----------------------------------------------
-- CIS 194, Homework 7
-- Author: Glenn R. Fisher
-- Date: April 26, 2016
----------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer
import Editor
import Data.Monoid

-- A JoinList is a tree to efficiently store and process list-like data.
-- Data is stored in the leaves of the tree. Tree nodes have an associated
-- annotation to store (typically monoidal) metadata.

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Get the monoidal annotation at the root of the JoinList.
--
-- > tag (Single (Sum 4) "data") == Sum 4
-- > tag (Append (Sum 8) (Single (Sum 4) "data") (Single (Sum 4) "data")) 
--   == Sum 8

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Append two JoinLists with monoidal annotations.
--
-- > (Single (Sum 4) "data") +++ (Single (Sum 4) "data")
--   == Append (Sum 8) (Single (Sum 4) "data") (Single (Sum 4) "data")

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append m jl1 jl2
    where m = (tag jl1) <> (tag jl2)

-- Safe list index (subscript) operator, starting from 0.
--
-- > [0,1,2] !!? (-1) == Nothing
-- > [0,1,2] !!? 1 == Just 1
-- > [0,1,2] !!? 3 == Nothing

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- Convert a JoinList's data into a list. (Ignores all metadata.)
--
-- > jlToList (Empty) == []
-- > jlToList (Single (Sum 4) "data") == ["data"]
-- > jlToList (Append (Sum 8) (Single (Sum 4) "data") (Single (Sum 4) "data"))
--   == ["data", "data"]

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Safe JoinList index (subscript) operator, starting from 0.
--
-- > (indexJ i jl) == (jlToList jl !!? i)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x) = if i == 0 then Just x else Nothing
indexJ i center@(Append _ left right)
    | i < 0 || i > centerSize = Nothing
    | i < leftSize = indexJ i left
    | otherwise = indexJ (i - leftSize) right
    where
        centerSize = getSize $ size $ tag center
        leftSize = getSize $ size $ tag left

-- Drop the first n elements from a JoinList.
--
-- > jlToList (dropJ n jl) == drop n (jlToList jl)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ x) = if i <= 0 then jl else Empty
dropJ i center@(Append _ left right)
    | i <= 0 = center
    | i >= centerSize = Empty
    | otherwise = (dropJ i left) +++ (dropJ (i-leftSize) right)
    where
        centerSize = getSize $ size $ tag center
        leftSize = getSize $ size $ tag left

-- Take the first n elements from a JoinList.
--
-- > jlToList (takeJ n jl) == take n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ x) = if i >= 1 then jl else Empty
takeJ i center@(Append _ left right)
    | i <= 0 = Empty
    | i >= centerSize = center
    | otherwise = (takeJ i left) +++ (takeJ (i-leftSize) right)
    where
        centerSize = getSize $ size $ tag center
        leftSize = getSize $ size $ tag left

-- Create a JoinList from a string, with a score as metadata. The score
-- is calculated according to the scoring metric of the game Scrabble.
--
-- > scoreLine "data" == Single (Score 5) "data"

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- Support Buffer operations for JoinLists with String data and 
-- (Score, Size) metadata.

instance Buffer (JoinList (Score, Size) String) where
    
    toString Empty = ""
    toString (Single _ str) = str
    toString (Append _ left right) = (toString left) ++ (toString right)

    fromString = foldr (+++) Empty . map lineToJL . lines
        where lineToJL = \line -> Single (scoreString line, (Size 1)) line

    line = indexJ

    replaceLine i str buf = takeJ i buf +++ fromString str +++ dropJ (i+1) buf

    numLines = getSize . snd . tag 

    value = getScore . fst . tag

-- The greeting is the string contained in the initial buffer.

greeting :: String
greeting = unlines
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ]

-- Run the editor with the greeting as the initial buffer.

main = runEditor editor initialBuffer
    where initialBuffer = (fromString greeting :: JoinList (Score, Size) String)
