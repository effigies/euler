#!/usr/bin/runhaskell

import Euler (primes)

result = sum $ takeWhile (<2000000) primes

main = putStrLn $ show result
