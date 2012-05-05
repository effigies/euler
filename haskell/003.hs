#!/usr/bin/runhaskell

import Euler (factors)

result = head . reverse $ factors 600851475143

main = putStrLn $ show result
