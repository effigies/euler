module Euler (factorials, triangles,
	primes, isPrime,
	factors, propers,
	digits, undigits,
	wordValue,
	permutations, isPermutation,
	isPalindrome,
	choose, choose',
	filters)
	where

import Data.List (nub, unfoldr, sort)
import Data.Char (ord)

factorials = scanl (*) 1 [1..]

triangles = scanl1 (+) [1..]

{- Primes -}
data Wheel a = Wheel a [a]

roll :: Integral a => Wheel a -> [a]
roll (Wheel n rs) = [n*k+r | k <- [0..], r <- rs]

w0 :: Integral a => Wheel a
w0 = Wheel 1 [1]

nextSize :: Integral a => Wheel a -> a -> Wheel a
nextSize (Wheel n rs) p =
  Wheel (p*n) [r' | k <- [0..(p-1)], r <- rs,
                      let r' = n*k+r, r' `mod` p /= 0]

mkWheel :: Integral a => [a] -> Wheel a
mkWheel ds = foldl nextSize w0 ds

primes :: Integral a => [a]
primes = small ++ large
    where
    1:p:candidates = roll $ mkWheel small
    small          = [2,3,5,7]
    large          = p : filter isPrime candidates
    isPrime n      = all (not . divides n) 
                       $ takeWhile (\p -> p*p <= n) large
    divides n p    = n `mod` p == 0
{- /Primes -}

isPrime 1 = False
isPrime n = all (passes n) $ takeWhile (\p -> p*p <= n) primes
	where
		passes n p = n `mod` p /= 0

factors :: Integral a => a -> [a]
factors = factors' primes
	where
		factors' _ 1 = []
		factors' (p:ps) n	| n `mod` p == 0	= p : factors' (p:ps) (n `div` p)
					| n < p * p		= [n]
					| otherwise		= factors' ps n

powerset [] = [[]]
powerset (x:xs) = map (x:) remnant ++ remnant
	where
		remnant = powerset xs

propers :: Integral a => a -> [a]
propers n = nub . map product . tail . powerset $ factors n

digits 0 = []
digits n = (n `mod` 10) : digits (n `div` 10)

undigits = foldr (\x y -> x + 10 * y) 0

wordValue :: String -> Int
wordValue = sum . map (\x -> ord x - ord 'A' + 1)

permutations [x] = [[x]]
permutations (x:xs) = do
		subperm <- permutations xs
		map (insert x subperm) [0 .. length subperm]
	where
		insert x xs 0 = x:xs
		insert x' (x:xs) n = x : insert x' xs (n-1)

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == reverse a


{- Choose n elements of a list, with repetition -}
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = [[]]
choose n [x] = [replicate n x]
choose n (x:xs)	= map (x:) (choose (n-1) (x:xs)) ++
					choose n xs

{- Choose n elements of a lits, without repetition -}
choose' :: Int -> [a] -> [[a]]
choose' 0 _ = [[]]
choose' _ [] = [[]]
choose' n (x:xs)	| lxs < n	= []
			| lxs == n	= [x:xs]
			| otherwise	= map (x:) (choose' (n-1) xs) ++
					choose' n xs
	where
		lxs = length (x:xs)

filters :: [a -> Bool] -> [a] -> [a]
filters [] xs = xs
filters (p:ps) xs = filter p $ filters ps xs

