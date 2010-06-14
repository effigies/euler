#!/usr/bin/runhaskell

import Euler (triangles, factors)

groups xs = groups' 1 xs
	where
		groups' _ [] = [0]
		groups' n [_] = [n]
		groups' n (x:xs@(y:_))	| x == y	= groups' (n+1) xs
					| otherwise	= n : groups' 1 xs

divisors n = product . map (+1) . groups $ factors n

result = snd . head . dropWhile ((< 500) . fst) $ zip (map (divisors) triangles) triangles

main = putStrLn $ show result
