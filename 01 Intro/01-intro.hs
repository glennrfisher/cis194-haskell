----------------------------------------------
-- CIS 194, Homework 1
-- Author: Glenn R. Fisher
-- Date: March 29, 2016
----------------------------------------------

----------------------------------------------
-- Part 1: Validating Credit Card Numbers
----------------------------------------------

-- | Convert positive integers to a list of digits. Non-positive integers are
-- converted to an empty list.
--
-- > toDigits 1234 == [1, 2, 3, 4]
-- > toDigits 0 == []
-- > toDigits (-1234) == []

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- | Convert positive integers to a reversed list of digits. Non-positive
-- integers are converted to an empty list.
--
-- > toDigitsRev 1234 == [4, 3, 2, 1]
-- > toDigitsRev 0 == []
-- > toDigitsRev (-1234) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

-- | Double every other number in a list, starting from the left. The result is
-- a list of the same length where the second, fourth, etc. numbers are doubled.
--
-- > doubleEveryOtherFromLeft [1, 2, 3] == [1, 4, 3]
-- > doubleEveryOtherFromLeft [1, 2, 3, 4] == [1, 4, 3, 8]

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:y:xs) = [x, 2*y] ++ doubleEveryOtherFromLeft xs

-- | Double every other number in a list, starting from the right. The result is
-- a list of the same length where the second-to-last, fourth-to-last, etc.
-- numbers are doubled.
--
-- > doubleEveryOtherFromRight [1, 2, 3] == [1, 4, 3]
-- > doubleEveryOtherFromRight [1, 2, 3, 4] == [2, 2, 6, 4]

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight xs = reverse (doubleEveryOtherFromLeft (reverse xs))

-- | Double every other number in a list, starting from the right. The result is
-- a list of the same length where the second-to-last, fourth-to-last, etc.
-- numbers are doubled.
--
-- > doubleEveryOther [1, 2, 3] == [1, 4, 3]
-- > doubleEveryOther [1, 2, 3, 4] == [2, 2, 6, 4]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleEveryOtherFromRight

-- | Calculate the sum of all digits in the list.
--
-- > sumDigits [1, 2, 3] == 6
-- > sumDigits [10, 11, 12] == 6

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- | Validate a credit card number. Returns True if the credit card number is
-- valid, and False otherwise.
--
-- Validation Algorithm:
-- 1. Double the value of every other second digit beginning from the right.
-- 2. Add the digits of the doubled values and the undoubled digits from the
--    original number.
-- 3. Calculate the remainder when the sum is divided by 0.
-- 4. Check if the remainder is 0. If so, then the credit card number is valid.
--
-- Example:
-- 1. 1386 -> [2, 3, 16, 6]
-- 2. 2+3+1+6+6 == 18
-- 3. 18 `mod` 10 == 8
-- 4. 8 /= 0, so 1386 is not a valid credit card number.
--
-- > validate 4012888888881881 == True
-- > validate 4012888888881882 == False

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

----------------------------------------------
-- Part 2: The Towers of Hanoi
----------------------------------------------

-- Pegs are identified by a String.
type Peg = String

-- A move transfers the top disk from one peg to another.
type Move = (Peg, Peg)

-- Return a list of moves to solve the Tower of Hanoi puzzle. The moves transfer
-- a given number of stacked discs from the first peg to the second, using the
-- third peg as temporary storage.
--
-- > hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x peg1 peg2 peg3
    | x <= 0 = []
    | x == 1 = [(peg1, peg2)]
    | otherwise =
        let moveToTemp = hanoi (x-1) peg1 peg3 peg2 in
        let moveLargest = [(peg1, peg2)] in
        let moveToSolution = hanoi (x-1) peg3 peg2 peg1 in
        moveToTemp ++ moveLargest ++ moveToSolution
        