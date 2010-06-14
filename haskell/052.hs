#!/usr/bin/runhaskell

import Euler (digits)
import Control.Applicative
import Data.List (sort)

s = sort . digits

handle = do
	a <- [9009,9018..]
	let xs = map s $ (*) <$> pure a <*> [2..6]
	if all (== s a) xs then return a else []

result = head handle

main = print result

