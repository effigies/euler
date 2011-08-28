#!/usr/bin/runhaskell

import Euler (digits)

valid (n:ns) x | ld x n == n	= 1 + valid ns x
	       | otherwise	= 0
	where
		ld x n = length . digits $ x^n

result = sum $ map (valid [1..]) [1..9]

main = print result
