#!/usr/bin/runhaskell

import Euler (digits)
import Data.Ratio ((%), denominator)
import Data.List (delete)

noTens = filter (\x -> x `mod` 10 /= 0)

search = do
	d <- noTens [12 .. 99]
	n <- noTens [11 .. d - 1]

	let ds = digits d
	let ns = digits n

	d' <- ds
	n' <- ns

	let d'' = head $ delete d' ds
	let n'' = head $ delete n' ns

	if (n % d == n' % d' && d'' == n'')
		then return (n % d)
		else []

result = denominator $ product search

main = print result
