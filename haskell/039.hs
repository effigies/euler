#!/usr/bin/runhaskell
{-# OPTIONS -XNoMonomorphismRestriction #-}

import Data.List (sort, maximumBy)

perimeters = filter (<1000) $ do
	b <- [1..998]
	let m = min b (999-b)
	a <- [1 .. m]
	let ss = ((fromIntegral a) ^ 2) + ((fromIntegral b) ^ 2)
	let c = truncate $ sqrt ss
	if ss == c^2 then return (a+b+c) else []

groups xs = groups' 1 xs
	where
		groups' _ [] = []
		groups' n [x] = [(n,x)]
		groups' n (x:xs@(y:_))	| x == y	= groups' (n+1) xs
					| otherwise	= (n,x) : groups' 1 xs

result = ""

main = print result

