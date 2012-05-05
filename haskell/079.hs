{- 2009.06.14 -}
import Data.List

prob79 = tsort (map (show) nums)

contains :: Eq a => [a] -> a -> Bool
contains = flip elem

tsort :: Eq a => [[a]] -> [a]
tsort [] = []
		 -- Drop empty lists
tsort seqs = let seqs'      = filter (not . null) seqs
		 heads      = nub (map head seqs')
		 -- Tails of sequences are inaccessible
		 restricted = nub (tail =<< seqs') -- (=<<) acts like concatMap
		 -- Heads that are not restricted are just grand
		 winners    = filter (not . contains restricted) heads
		 -- Remove any winners from the beginning of our sequences
		 losers     = map (dropWhile (contains winners)) seqs'
		in if null winners && (not . null) losers
			then error "No ordering exists"
			else winners ++ tsort losers


nums = [319,
	680,
	180,
	690,
	129,
	620,
	762,
	689,
	762,
	318,
	368,
	710,
	720,
	710,
	629,
	168,
	160,
	689,
	716,
	731,
	736,
	729,
	316,
	729,
	729,
	710,
	769,
	290,
	719,
	680,
	318,
	389,
	162,
	289,
	162,
	718,
	729,
	319,
	790,
	680,
	890,
	362,
	319,
	760,
	316,
	729,
	380,
	319,
	728,
	716]

