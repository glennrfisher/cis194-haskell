----------------------------------------------
-- CIS 194, Homework 6
-- Author: Glenn R. Fisher
-- Date: April 26, 2016
----------------------------------------------

-- Compute the nth Fibonacci number.
-- (This computation has an exponential running time.)
--
-- > fib 0 == 0
-- > fib 1 == 1
-- > fib 2 == 1
-- > fib 10 == 55

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- An infinite list of all Fibonacci numbers.
-- (Each element has an exponential running time.)
--
-- > take 10 fibs1 == [0,1,1,2,3,5,8,13,21,34]


fibs1 :: [Integer]
fibs1 = map fib [0..]

-- An infinite list of all Fibonacci numbers.
-- (The list is computed in linear time.)
--
-- > take 10 fibs2 == [0,1,1,2,3,5,8,13,21,34]

fibs2 :: [Integer]
fibs2 = f 0 1 where
    f a b = a : f b (a + b)

-- A stream is an infinite list of elements.
-- It is like a list with only the "cons" operator.

data Stream a = Cons a (Stream a)

-- Convert a Stream to an infinite list.
--
-- > streamToList (streamRepeat 1) == [1,1,1,1,...]

streamToList :: Stream a -> [a]
streamToList (Cons x stream) = x : streamToList stream

-- Show a Stream by printing its first 20 elements.
--
-- > show (streamRepeat 1)
--   == "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Generate a stream containing infinitely
-- many copies of the given element.
--
-- > show (streamRepeat 1)
--   == "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Apply a function to every element of a Stream.
--
-- > show (streamMap (+1) (streamRepeat 1))
--   == "[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]"

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Generate a Stream from a seed and an unfolding rule.
--
-- The seed is the first element of the stream. The
-- unfolding rule specifies how to transform the seed
-- into a new seed, to be used for generating the rest of
-- the stream.

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- A Stream of the natural numbers: 0, 1, 2, ...

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Interleave two Streams.
--
-- > show $ interleaveStreams nats (streamRepeat 0)
--   == "[0,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0]"

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- A Stream of the ruler function, where the nth element
-- in the stream (assuming the first element corresponds
-- to n = 1) is the largest power of 2 which evenly
-- divides n.
--
-- > show ruler == "[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]"

ruler :: Stream Integer
ruler = rulerFrom 0
    where rulerFrom x = interleaveStreams (streamRepeat x) (rulerFrom (x+1))
