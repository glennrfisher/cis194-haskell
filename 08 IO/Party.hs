----------------------------------------------
-- CIS 194, Homework 8
-- Author: Glenn R. Fisher
-- Date: April 28, 2016
----------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Party where

import Employee
import Data.List
import Data.Tree

-- Add an employee to the guest list, irrespective of the employees that are
-- already on the guest list. That is, the employee, his or her boss, and his
-- or her subordinates should not already be on the guest list.
--
-- > glCons (Emp { empName = "Glenn", empFun = 10 }) (GL [] 0)
--   == GL [Emp { empName = "Glenn", empFun = 10 }] 10

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL gl fun) = GL (employee:gl) (fun + empFun employee)

-- Support Monoid operations on GuestLists.

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

-- Return the guest list that is more fun.
--
-- > moreFun (GL [] 0) (GL [Emp { empName = "Glenn", empFun = 10 }] 10)
--   == GL [Emp { empNAme = "Glenn", empFun = 10 }] 10

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1 >= fun2 = gl1
    | otherwise = gl2

-- Fold a "rose tree".
--
-- > let one = Node { rootLabel = 1, subForest = [] }
-- > let two = Node { rootLabel = 2, subForest = [] }
-- > let three = Node { rootLabel = 3, subForest = [] }
-- > let tree = Node { rootLabel = 4, subForest = [one, two, three] }
-- > treefold (\x xs -> x + (sum xs)) 0 tree == 10

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f e t = f value foldChildren
    where
        foldChildren = map (treeFold f e) (subForest t)
        value = rootLabel t

-- Compute the best guest list both with and without the given employee.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (withBoss, withoutBoss)
    where
        withBoss = GL [boss] (empFun boss)
        withoutBoss = GL [] 0
nextLevel boss guestLists = (withBoss, withoutBoss)
    where
        withBoss = foldr mappend (GL [boss] (empFun boss)) (map snd guestLists)
        withoutBoss = mconcat (map fst guestLists)

-- Given a company hierarchy, compute the optimal guest list by maximizing
-- the amount of fun that would be had.

maxFun :: Tree Employee -> GuestList
maxFun tree =
    let (withBoss, withoutBoss) = treeFold nextLevel (mempty, mempty) tree in
    let (GL _ funWithBoss, GL _ funWithoutBoss) = (withBoss, withoutBoss) in
    case compare funWithBoss funWithoutBoss of
        LT -> withBoss
        EQ -> withBoss
        GT -> withoutBoss

-- Compare employees by their names.

instance Ord Employee where
    compare e1 e2 = compare (empName e1) (empName e2)
    
-- Format a guest list for printing.

format :: GuestList -> String
format (GL employees totalFun) = 
    let fun = ["Total fun: " ++ show totalFun] in
    let names = map empName (sort employees) in
    unlines (fun ++ names)

-- Read a company hierarchy to compute the optimal guest list.

main :: IO ()
main =
    (readLn :: IO (Tree Employee)) >>= putStr . format . maxFun
