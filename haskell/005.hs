{- Chris Johnson
 - 2009.06.14
 -
- Problem 5
 - 2520 is the smallest number that can be divided by each of the numbers from
 - 1 to 10 without any remainder.
 -
 - What is the smallest number that is evenly divisible by all of the numbers
 - from 1 to 20?
 -}
prob5 = product (reduce [1..20])

reduce [] = []
reduce (x:xs) = x : reduce ([if x' `mod` x == 0 then x' `div` x else x' | x' <- xs])
