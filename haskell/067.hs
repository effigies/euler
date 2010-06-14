import Control.Monad (liftM)

main = do
	text <- readFile "triangle.txt"
	let parsed = reverse $ rows text
	    result = head . head . last $ takeWhile (not . null) (iterate reduce parsed)
	putStrLn (show result)

rows :: String -> [[Int]]
rows = map (map read) . map words . lines

reduce [x] = []
reduce (x:y:z) = (zipWith max (zipWith (+) x y) (zipWith (+) (tail x) y)) : z
