import Euler (factors)
import Data.List (nub)

ints = map (length . nub . factors) [2..]

fours = map snd . filter ((==4) . fst) $ zip ints [2..]

findConsecutive (x:xs@(w:y:z:_)) | [x..z] == [x,w,y,z]	= x
				 | otherwise		= findConsecutive xs

result = findConsecutive fours

main = print result
