{- Chris Johnson
 - 2009.06.14
 -
 - Problem 16
 - 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 -
 - What is the sum of the digits of the number 2^(1000)?
 -}
import Char

sumdigits :: Integral a => a -> Int
sumdigits = sum . (map digitToInt) . show

prob16 = sumdigits (2 ^ 1000)
