import Data.List (last)

{-
Grid of binomial coefficients
The thing to note is that each point describes the number of ways to get there
from the origin.

Proof:
There is 1 way to get to the origin.

Given that those points immediately up and to the left describe the number of
ways to reach them, we need only note that there is only one way from each to
the point in question. Therefore, the number of ways to reach the point in
question is their sum.
-}

grid = repeat 1 : map comb grid
	where
		comb prev = 1 : zipWith (+) (tail prev) (comb prev)

result = grid !! 20 !! 20

main = putStrLn $ show result
