#!/usr/bin/runhaskell

import Euler (primes, isPrime)
import Data.List (maximumBy)

candidates = do
		b <- takeWhile (< 1000) primes
		a <- filter (> -1000) . takeWhile (< 1000) $ map (-b - 1 +) primes
		return (a,b)

run a b = length . takeWhile (\x -> x > 1 && isPrime x) $ map (f a b) [0..]
	where
		f a b x = x*x + a*x + b

runs = flip zip candidates $ map (uncurry run) candidates

result = uncurry (*) . snd $ maximumBy (flip (flip (compare . fst) . fst)) runs

main = print result
