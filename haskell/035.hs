#!/usr/bin/runhaskell

import Euler (primes, isPrime, digits, undigits)

candidates = takeWhile (<1000000) primes

isCircular n = isCircular' (length ns) ns
	where
		ns = digits n

isCircular' 0 _ = True
isCircular' n (x:xs) | isPrime $ undigits (x:xs) = isCircular' (n-1) (xs ++ [x])
		     | otherwise		 = False

result = length $ filter isCircular candidates

main = print result
