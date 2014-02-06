module Homework1 (pascal, balanceParen, countChange) where

-- Problem 1: compute the value of Pascals triangle for a given column
--            and row.
--  The solution here goes back to the definition of n choose m
pascal :: (Integral a) => a -> a -> a
pascal col row = choose row col

-- Implementation of choose operator
choose :: (Integral a) => a -> a -> a
choose 0 _ = 1
choose n 1 = n
choose n m
       | m < 0 = 0
       | m > n = 0
       | m > n `div` 2 = choose_acc n (n - m) 1 1 -- use symmetry
       | otherwise = choose_acc n m 1 1

-- Backend to compute n choose m using accumulators
-- Based on the recurrence n choose m = (n + 1 - m) (n choose m - 1) / m
--
-- If we want to stick with integer arithmetic, it is necessary to
--  accumulate the quotiend and divisor separately.
choose_acc :: (Integral a) => a -> a -> a -> a -> a
choose_acc 0 _ _ _ = 1
choose_acc n 0 q d = q `div` d
choose_acc n 1 q d = (n * q) `div` d
choose_acc n m q d = choose_acc n (m - 1) ((n + 1 - m) * q) (m * d)

-- Problem 2
-- balanceParen checks to see if the parenthesis in an
--  expression are balanced
-- For example balanceParen "hello (world (that) I do not (know))" is
--  balanced, but balanceParen "( ()" is not, nor is
--  balanceParen ")(" since a closed ) is encountered first
balanceParen :: String -> Bool
balanceParen str = balance_acc str 0

-- Accumulator for balanceParen
balance_acc :: String -> Int -> Bool
balance_acc [] nopen = if nopen == 0 then True else False
balance_acc _ nopen
   | nopen < 0 = False
balance_acc ('(':xs) nopen = balance_acc xs (nopen+1)
balance_acc (')':xs) nopen = balance_acc xs (nopen-1)
balance_acc (x:xs) nopen = balance_acc xs nopen -- any other character


-- Problem 3 
-- countChange money [change]
-- Count the number of ways to make change for money
--  using change.  change is the list of coins, not
--  the number available
-- For example countChange 4 [1, 2] returns 3 since
--  1 + 1 + 1 + 1, 1 + 1 + 2, and 2 + 2 are all valid
--  ways to make change
countChange :: (Integral a) => a -> [a] -> a
countChange _ [] = 0
countChange 0 _ = 1
countChange money coins@(c:cs)
    | money < 0 = 0
    | otherwise = countChange (money-c) coins + countChange money cs