#!/usr/bin/runhaskell

import Euler (triangles)

pentagonals = scanl1 (+) [1,4..]
hexagonals = scanl1 (+) [1,5..]

union (x:xs) (y:ys) = case compare x y of
			EQ -> x : union xs ys
			LT -> union xs (y:ys)
			GT -> union (x:xs) ys

union3 xs ys zs = union xs $ union ys zs

result = head $ union3 t p h
	where
		[t,p,h] = map (dropWhile (<=40755)) [triangles, pentagonals, hexagonals]

main = print result

