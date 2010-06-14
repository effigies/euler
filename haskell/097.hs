#!/usr/bin/runhaskell

p = 28433 * 2 ^ 7830457 + 1

result = p `mod` 10^10

main = print result

