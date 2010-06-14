#!/usr/bin/runhaskell

import Euler (digits,undigits,isPalindrome)

rsum x = x + (undigits . reverse $ digits x)

isLychrel = isLychrel' 50
	where
		isLychrel' 0 _ = True
		isLychrel' n x	| isPalindrome (digits rs) = False
				| otherwise = isLychrel' (n-1) rs
			where
				rs = rsum x

lychrels = filter isLychrel [0..9999]

result = length lychrels

main = print result

