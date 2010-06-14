#!/usr/bin/runhaskell

import Euler (primes)
import qualified Data.Set as S

nines = map (-1 +) $ iterate (10*) 10

considered = S.fromAscList . takeWhile (<1000) $ drop 3 primes

reduce set _ | S.size set == 1 = set
reduce set (x:xs) = reduce (S.filter (\y -> y ^ 2 > x || x `mod` y /= 0) set) xs

result = head . S.toList $ reduce considered nines

main = putStrLn $ show result
