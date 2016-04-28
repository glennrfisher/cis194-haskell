----------------------------------------------
-- CIS 194, Homework 7
-- Author: Glenn R. Fisher
-- Date: April 26, 2016
----------------------------------------------

module Scrabble where

import Data.Char

-- A Score is an integer.

data Score = Score Int
    deriving (Eq, Ord, Show)

-- Support Monoid operations for Scores.

instance Monoid Score where
    mempty = Score 0
    mappend (Score x) (Score y) = Score (x+y)

-- Score a character according to its Scrabble scoring value.
--
-- > score 'a' == Score 1
-- > score 'b' == Score 3
-- > score 'z' == Score 10
-- > score ' ' == Score 0

score :: Char -> Score
score c
    | c' `elem` "aeioulnstr" = Score 1
    | c' `elem` "dg" = Score 2
    | c' `elem` "bcmp" = Score 3
    | c' `elem` "fhvwy" = Score 4
    | c' `elem` "k" = Score 5
    | c' `elem` "jx" = Score 8
    | c' `elem` "qz" = Score 10
    | otherwise = Score 0
    where c' = toLower c

-- Score a string according to its Scrabble scoring value.
--
-- > scoreString "hello" == Score 8
-- > scoreString "world" == Score 9
-- > scoreString "!@#$%" == Score 0

scoreString :: String -> Score
scoreString str = foldr mappend (Score 0) (map score str)

-- Convert a Score to an Int.
--
-- > getScore (Score 0) == 0
-- > getScore (Score 8) == 8

getScore :: Score -> Int
getScore (Score x) = x
