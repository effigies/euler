#!/usr/bin/runhaskell

collatz = takeWhile (/= 1) . iterate f
	where
		f n	| even n = n `div` 2
			| otherwise = 3 * n + 1


lens = zip (map (length . collatz) [1..999999]) [1..999999]

taggedMax (x:xs) = taggedMax' x xs
	where
		taggedMax' (t,m) [] = m
		taggedMax' (t,m) ((t',c):xs)	| t' > t	= taggedMax' (t',c) xs
						| otherwise	= taggedMax' (t,m) xs

result = taggedMax lens

main = putStrLn $ show result
