#!/usr/bin/runhaskell

import Euler (factors, primes)
import qualified Data.Array as A

{-
upperCartesian xs ys = zip xs' ys'
	where
		xs' = concat [take n xs | n <- [1..]]
		ys' = concat $ zipWith ($) (map replicate [1..]) ys
-}

twoFactors n = twoFactors' primes n

twoFactors' _ 1 = False
twoFactors' (p:ps) n | n `mod` p == 0 = oneFactor (p:ps) (n `div` p)
		     | n < p*p = False
		     | otherwise = twoFactors' ps n
oneFactor _ 1 = False
oneFactor (p:ps) n | n `mod` p == 0	= n == p
		   | n < p*p		= True
		   | otherwise		= oneFactor ps n

a = A.listArray (4,100000000) $ replicate 99999997 1

{-
count = count' 0
count' acc _ [] = acc
count' acc x (y:ys) | y == x	= count' (acc+1) x ys
		    | otherwise	= count' acc x ys

result = count True $ map twoFactors [4..100000000]
-}

-- result = length $ do
-- 	candidate <- [4..100000000]
-- 	p <- takeWhile (\x -> x*x <= candidate) primes
-- 	let y = candidate `div` p
-- 	p' <- takeWhile (\x -> x*x <= y) primes
-- 	if candidate `mod` p == 0 && y `mod` p' == 0 && y 
-- 	return 1

main = print result

