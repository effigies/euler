#!/usr/bin/runhaskell

import Euler (wordValue)
import Names
import Data.List (sort)

n = sort names

result = sum . zipWith (*) [1..] $ map wordValue n

main = print result
