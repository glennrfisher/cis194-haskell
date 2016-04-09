----------------------------------------------
-- CIS 194, Homework 4
-- Author: Glenn R. Fisher
-- Date: April 8, 2016
----------------------------------------------

----------------------------------------------
-- Part 1. Wholemeal Programming
----------------------------------------------

-- Reimplement `fun1` using wholemeal programming.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

-- Reimplement `fun2` using wholemeal programming.

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even
            . takeWhile (>1)
            . iterate (\x -> if even x then x `div` 2 else 3 * x +1)

----------------------------------------------
-- Part 2. Folding with Trees
----------------------------------------------

-- A binary tree is either:
--   a leaf, or
--   a node with a height, a left child, an element, and a right child

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- Return the height of the given tree. The height of a binary tree
-- is defined to be the length of a path from the root to the deepest
-- node.
--
-- > height Leaf = 0
-- > height (Node 0 Leaf x Leaf) = 0
-- > height (Node 1 Leaf x (Node 0 Leaf y Leaf)) = 1

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

-- Insert a node into a balanced binary tree. The resulting tree is also
-- balanced.
-- 
-- A binary tree is balanced if the height of its left and right subtrees
-- differ by no more than 1, and its left and right subtrees are also
-- balanced.
--
-- > insert "J" Leaf == Node 0 Leaf "J" Leaf
-- > insert "I" (insert "J" Leaf) == Node 1 (Node 0 Leaf "I" Leaf) "J" Leaf
-- > insert "H" (insert "I" (insert "J" Leaf))
--   == Node 1 (Node 0 Leaf "I" Leaf) "J" (Node 0 Leaf "H" Leaf)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf y Leaf) = Node 1 (insert x Leaf) y Leaf
insert x (Node h Leaf y right) = Node h (insert x Leaf) y right
insert x (Node h left y Leaf) = Node h left y (insert x Leaf)
insert x (Node h left y right) =
    let (leftH, rightH) = (height left, height right) in
    case compare leftH rightH of
        LT -> Node h (insert x left) y right
        GT -> Node h left y (insert x right)
        EQ -> Node (1 + height right') left y right'
                where right' = insert x right

-- Construct a balanced binary tree from a list of values.
--
-- A binary tree is balanced if the height of its left and right subtrees
-- differ by no more than 1, and its left and right subtrees are also
-- balanced.
--
-- > foldTree "ABCDEFGHIJ"
--   == Node 3
--        (Node 2
--          (Node 0 Leaf 'F' Leaf)
--          'I'
--          (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
--        'J'
--        (Node 2 
--          (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
--          'H'
--          (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

----------------------------------------------
-- Part 3. More Folds!
----------------------------------------------

-- Return `True` if and only if there are an odd number of
-- `True` values contained in the input list. It does not
-- matter how many `False` values the input list contains.
--
-- > xor [False, True, False] == True
-- > xor [False, True, False, False] == True
-- > xor [False, True, False, False, True] == False

xor :: [Bool] -> Bool
xor = odd . length . filter (\x -> x)

-- Implement map as a fold.

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

-- Implement `foldl` using `foldr`.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

----------------------------------------------
-- Part 4. Finding Primes
----------------------------------------------

-- Generate all the odd prime numbers up to 2n + 2,
-- using the Sieve of Sundaram.
--
-- > sieveSundaram 2 == [3, 5]
-- > sieveSundaram 10 == [3,5,7,11,13,17,19]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let exclude = [i+j+2*i*j | i <- [1..n], j <- [1..i], i+j+2*i*j <= n] in
    [2*i+1 | i <- [1..n], i `notElem` exclude]
