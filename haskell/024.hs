factorials = reverse . take 9 $ scanl (*) 1 [2..]

initial = 10 ^ 6

digits = [0..9]

reduce _ (d:ds) sd 0 =d : (reverse sd ++ ds)
reduce _ ds sd 0 = reverse sd ++ ds
reduce (f:fs) (d:ds) sd acc | acc >= f = reduce (f:fs) ds (d:sd) (acc - f)
			    | otherwise = d : reduce fs (reverse sd ++ ds) [] acc

result = concatMap show $ reduce factorials digits [] (initial - 1)

main = putStrLn result
