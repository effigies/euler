#!/usr/bin/runhaskell

import Euler (isPrime, digits, undigits, permutations, choose, choose', filters)
import Data.List (sort, nub)

digitsets = choose 4 [1..9]

primeQuartets = map (filter isPrime . map undigits . permutations) digitsets

candidates = map sort $ filter ((> 2) . length) primeQuartets

{- Filters -}
different [x,y,_] = x /= y
arithmetic [x,y,z] = y - x == z - y
new [x,_,_] = x /= 1487

triplet = head . nub . filters [arithmetic,different,new] $ candidates >>= choose' 3

result = undigits $ reverse triplet >>= digits

main = print result

