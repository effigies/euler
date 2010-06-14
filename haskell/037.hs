#!/usr/bin/runhaskell

import Euler (digits, undigits, primes, isPrime)
import Data.List (init,inits,tails)

reductions xs = [inits xs, tails xs] >>= init . tail
ds = map (all isPrime . map undigits . reductions . digits) primes
result = sum . take 11 . map snd . filter fst . drop 4 $ zip ds primes

main = print result

