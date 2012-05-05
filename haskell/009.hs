{- Chris Johnson
 - 2008
 -
 - Problem 1
 - If we list all the natural numbers below 10 that are multiples of 3 or 5, we
 - get 3, 5, 6 and 9. The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000.
 -}
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [3 .. n - 2],
                     y <- [x + 1 .. n - 1],
                     z <- [y + 1 .. n],
                     x^2 + y^2 == z^2 ]

prob9 = head [x*y*(1000 - x - y) | x <- [3 .. 500], y <- [x + 1 .. 500], 500000 == 1000*(x + y) - x*y]
