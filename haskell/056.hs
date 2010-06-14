#!/usr/bin/runhaskell

import Euler (digits)

ab = [a^b | a <- [2..100], b <- [2..100]]

result = maximum $ map (sum . digits) ab

main = print result

