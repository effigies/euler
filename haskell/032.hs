#!/usr/bin/runhaskell

import Euler (digits)
import Data.List (sort, nub)
import Control.Monad (when)

set = [1..9]

choose 0 _ = [[]]
choose n source = do
	x <- source
	map (x:) . choose (n - 1) $ filter (/= x) source

decimalize = foldr (\x y -> x + 10 * y) 0

{- First, we must consider our search space. Obviously, we won't be
 - looking at a * b = cdefghi. Because 9 * 999 and 99 * 99 are both
 - <10000, a * bcd and ab * cd aren't candidates, either.
 -
 - Obviously, a * bcdef /= ghi, nor does ab * cdef, or abc * def.
 -
 - So we'll be looking at a * bcde, and ab * cde.
 -}
result = sum . nub $ do
	[a,b,c,d,e] <- choose 5 set
	let fghi = filter (not . flip elem [a,b,c,d,e]) set

	{- Let's generate our possible multipliers and
	 - multiplicants. a is already taken care of.
	 -}
	let ab = decimalize [a,b]
	let cde = decimalize [c,d,e]
	let bcde = decimalize [b,cde]
	{- To anybody reading this code, I use these names because
	 - they're pretty and they get the idea across. In actuality,
	 - the digits are reversed. It doesn't matter, since we go
	 - through all possible combinations, but this might confuse
	 - somebody looking closely. This is thanks to the right fold.
	 -}

	{- These are our possible results -}
	fghi' <- [ab * cde, a * bcde]

	{- Keep products with the leftover digits -}
	if ((sort $ digits fghi') == fghi)
		then return fghi'
		else []

main = print result
