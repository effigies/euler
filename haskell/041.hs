#!/usr/bin/runhaskell

import Euler (isPrime, undigits, permutations)

-- Why no 9? Because 9 | sum [1..9], so no 9-digit pandigitals can be prime
pandigitals = map undigits ([[1..n] | n <- [1..8]] >>= permutations)

result = maximum $ filter isPrime pandigitals

main = print result

