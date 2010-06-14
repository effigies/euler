#!/usr/bin/runhaskell

import Euler (triangles, wordValue)
import Words
import Data.Set (fromAscList, member)

values = map (toInteger . wordValue) dictionary

testSet = fromAscList $ takeWhile (< maximum values) triangles

result = length $ filter (flip member testSet) values

main = print result
