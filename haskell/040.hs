#!/usr/bin/runhaskell

import Euler (digits)

significand = 0 : ([1..] >>= reverse . digits)

powers = iterate (*10) 1

result = product . map (($ Main.significand) . flip (!!)) $ take 7 powers

main = print result

