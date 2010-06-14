#!/usr/bin/runhaskell

denominations = [200,100,50,20,10,5,2,1]

combinations = combinations' (head denominations)

combinations' _ 0 = [[]]
combinations' last n = do
		c <- dropWhile (\x -> x > last || x > n) denominations
		map (c:) $ combinations' c (n - c)

result = length $ combinations 200

main = print result
