#!/usr/bin/runhaskell

import Euler (digits,undigits)
import Data.List (sort)

candidates = concat (zipWith (map) (map (,) [2..5]) [[5000..9999],[100..333],[26..33],[5..9]]) 

run n x = map (x*) [1..n] >>= reverse . digits

test n x = if sort r == [1..9] then undigits $ reverse r else 0
	where
		r = run n x

result = maximum $ map (uncurry test) candidates

main = print result

