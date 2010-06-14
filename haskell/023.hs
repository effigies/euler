#!/usr/bin/runhaskell

import Euler (propers)
import Data.Set (fromList, member)

isAbundant n = (n <) . sum $ propers n

abundants = filter isAbundant [1..]

abundantSums = fromList $ do
		summands <- takeWhile (not . null) $ iterate tail options
		filter (< 28123) $ zipWith (+) options summands
	where
		options = takeWhile (< 28123) abundants

result = sum $ filter (not . flip member abundantSums) [1..28123]

main = print result
