#!/usr/bin/runhaskell

import Euler (propers)

candidate = sum . propers

isAmicable n = let c = candidate n in
		n /= c && n == candidate c

result = sum $ filter isAmicable [2..10000]

main = print result
