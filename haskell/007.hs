#!/usr/bin/runhaskell

import Euler (primes)

result = primes !! 10000

main = putStrLn $ show result
