#!/usr/bin/runhaskell

import Euler (factorials)

f = (!!) factorials
factratio a b = product [b+1 .. a]

choose n r = factratio n r `div` f (fromInteger (n - r))

choices = do
	n <- [1..100]
	r <- [1..n]
	if choose n r > 1000000 then return (n,r) else []

result = length choices

main = print result

