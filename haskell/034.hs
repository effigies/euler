#!/usr/bin/runhaskell

import Euler (digits)

factorials = scanl (*) 1 [1..]
f n = factorials !! n

upperBound = upperBound' 9 (f 9)
	where
		upperBound' n fn | n < fn = upperBound' (10 * n + 9) (fn + f 9)
				 | otherwise = fn

curious n = n == (sum . map f $ digits n)

result = sum $ filter curious [3..upperBound]

main = putStrLn $ show result
