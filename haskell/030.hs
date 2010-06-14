#!/usr/bin/runhaskell

import Euler (digits)

upperBound = upperBound' 9 (9 ^ 5)
	where
		upperBound' n fn | n < fn = upperBound' (10 * n + 9) (fn + 9^5)
				 | otherwise = fn

fits n = n == (sum . map (^5) $ digits n)

result = sum $ filter fits [2..upperBound]

main = print result
