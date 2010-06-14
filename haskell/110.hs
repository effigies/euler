#!/usr/bin/runhaskell

import Euler (factors, propers)

impropers x = x : propers x

test n x y = x + y == x*y `div` n

-- a*x + b*y == a*b*x*y `div` n


split :: Integral a => a -> [(a,a)]
split x = zip divs $ map (x `div`) divs
	where
		divs = filter (\y -> y*y <= x) $ propers x

result = ""

main = print result

