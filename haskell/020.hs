{- Chris Johnson
 - 2009.06.14
 -
 - Problem 20
 - n! means n × (n − 1) × ... × 3 × 2 × 1
 -
 - Find the sum of the digits in the number 100!
 -}
import Char

prob20 = sumdigits (factorial 100)

sumdigits :: Integral a => a -> Int
sumdigits = sum . (map digitToInt) . show

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

factorial n = factorials !! n
