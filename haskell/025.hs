{- 2009.06.14 -}

prob25 = length (takeWhile (id) (map (\x -> length (show x) < 1000) fibs))

-- fibs : produce list of Fibonnacci numbers
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
