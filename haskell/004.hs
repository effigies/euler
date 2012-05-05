{- Chris Johnson
 - 2009.06.14
 -
 - Problem 4
 - A palindromic number reads the same both ways. The largest palindrome made
 - from the product of two 2-digit numbers is 9009 = 91  99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}
prob4 = head [i * j | (i,j) <- cart [999,998..100] [999,998..100], isPalindrome (show (i * j))]

cart xs ys = zip xs' ys'
	where
		xs' = concat [          take n xs  | n <- [1..] ]
		ys' = concat [ reverse (take n ys) | n <- [1..] ]

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (first:ss) = let (mid,last) = splitEnd ss
	in first == last && isPalindrome mid

splitEnd :: [a] -> ([a],a)
splitEnd [x] = ([],x)
splitEnd (x:xs) = let (rs,r) = splitEnd xs
		in (x:rs,r)
