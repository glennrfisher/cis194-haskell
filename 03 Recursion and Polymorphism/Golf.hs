----------------------------------------------
-- CIS 194, Homework 3
-- Author: Glenn R. Fisher
-- Date: April 1, 2016
----------------------------------------------

module Golf where

----------------------------------------------
-- 1. Hopscotch
----------------------------------------------

-- Transform a list into a list of lists, where the nth list of the output
-- contains every nth element from the input list.
--
-- > skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- > skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- > skips [1] == [[1]]
-- > skips [True, False] == [[True, False], [False]]
-- > skips [] == []

skips :: [a] -> [[a]]
skips xs = [each i xs | i <- [1..length xs]]

-- Take every nth element of the input list.
--
-- > each 1 "ABCD" == "ABCD"
-- > each 2 "ABCD" == "BD"
-- > each 3 "ABCD" == "C"
-- > each 4 "ABCD" == "D"
-- > each 5 "ABCD" == ""

each :: Int -> [a] -> [a]
each n xs =
    case drop (n-1) xs of
        (y:ys) -> y : each n ys
        [] -> []

----------------------------------------------
-- 2. Local Maxima
----------------------------------------------

-- Find all the local maxima in the input list. A local maximum is an
-- element of the list that is strictly greater than both the elements
-- immediately before and after it.
--
-- > localMaxima [2,9,5,6,1] == [9, 6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x < y && y > z = y : localMaxima (y:z:zs)
    | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

----------------------------------------------
-- 3. Histogram
----------------------------------------------

-- Count the frequency of the digits 0 through 9 in the input list.
--
-- > frequency []      == [0,0,0,0,0,0,0,0,0,0]
-- > frequency [1,2,3] == [0,1,1,1,0,0,0,0,0,0]
-- > frequency [1,1,1] == [0,3,0,0,0,0,0,0,0,0]

frequency :: [Integer] -> [Int]
frequency xs = map (\n -> length (filter (== n) xs)) [0..9]

-- Convert a frequency list into a histogram line for the given y value.
--
-- > line [1,0,3,0,5,0,3,0,1,0] 6 == "          "
-- > line [1,0,3,0,5,0,3,0,1,0] 5 == "    *     "
-- > line [1,0,3,0,5,0,3,0,1,0] 2 == "  * * *   "
-- > line [1,0,3,0,5,0,3,0,1,0] 1 == "* * * * * "
-- > line [1,0,3,0,5,0,3,0,1,0] 0 == "**********"

line :: [Int] -> Int -> String
line xs y = map (\x -> if x >= y then '*' else ' ') xs

-- Construct a vertical histogram representing the frequency of
-- each digit (0 through 9, inclusive) in the input list.
-- (Use `putStr` to render the histogram in the console.)
--
-- > putStr (histogram []) == 
--     ==========
--     0123456789
-- > putStr (histogram [1,1,1,5]) == 
--     *
--     *
--     *   *
--    ==========
--    0123456789

histogram :: [Integer] -> String
histogram xs = rows ++ "==========\n0123456789\n"
    where f = frequency xs
          m = maximum f
          rows = unlines (map (line f) [m, m-1..1])
