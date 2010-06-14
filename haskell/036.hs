#!/usr/bin/runhaskell

import Euler (digits)

bits 0 = []
bits n = (n `mod` 2) : bits (n `div` 2) 

isPalindromic xs = xs == reverse xs

dualPalindromes = filter (isPalindromic . bits) . filter (isPalindromic . digits) $ [1,3..1000000]

result = sum dualPalindromes

main = print result

