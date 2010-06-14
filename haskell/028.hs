
-- Some explanation may be in order. Basically, you are starting at the top right corner
-- of the previous layer of the spiral. You then move out one, and take the length of
-- the side down, giving you (previous length + 1) distance between your corners. The
-- next three will also be this far away. And then you must increase the length by 2
-- again.
corners = 1 : corners' 4 2 corners
	where
		corners' 0 i xs = corners' 4 (i+2) xs
		corners' n i (x:xs) = x + i : corners' (n-1) i xs

-- We're going for a 1001x1001 spiral. Which means we're on the 501st "layer"
-- There are 4 corners for each layer but the innermost, which only has 1
n :: Int
n = 4 * 501 - 3

result = sum $ take n corners

main = putStrLn $ show result
