#!/usr/bin/runhaskell

import Euler (digits,undigits,primes)
import Data.List (tails,permutations, nub)

divisible x y = mod y x == 0

pandigitals = permutations [0..9]

valid = and . zipWith divisible primes . map (undigits . reverse . take 3) . take 7 . tail . tails

result = sum . map (undigits . reverse) . nub $ filter valid pandigitals

main = print result

