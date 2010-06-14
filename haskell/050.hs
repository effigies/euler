#!/usr/bin/runhaskell

import Euler (primes, isPrime)

candidate ps = last . filter (isPrime . fst) .
		takeWhile ((<1000000) . fst) $ zip (scanl (+) 0 ps) [0..]

consider (hopeful,len) (p:ps)	| done		= hopeful
				| otherwise	= consider (h',l') ps
	where
		done = len * p >= 1000000
		(c,n) = candidate (p:ps)
		l' = max n len
		h' = if n > len then c else hopeful

result = consider (0,0) primes

main = print result

