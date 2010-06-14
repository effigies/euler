#!/usr/bin/runhaskell

import Euler (primes)

oddprimes = tail primes

oddcomposites = composites' (drop 4 primes) [9,11..]
	where
		composites' (p:ps) (c:cs) | c == p	= composites' ps cs
					  | otherwise	= c : composites' (p:ps) cs

doublesquares = [2*i*i | i <- [1..]]

fits c = fits' c oddprimes
	where
		fits' c (p:ps)	| p < c = isDblSqr (c - p) || fits' c ps
				| otherwise = False
		isDblSqr x = isDblSqr' x doublesquares
		isDblSqr' x (y:ys) | y < x	= isDblSqr' x ys
				  | y == x	= True
				  | otherwise	= False

result = head . filter (not . fits) $ oddcomposites

main = print result

