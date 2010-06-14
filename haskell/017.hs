thousands n | n `div` 1000 > 0 = subhundred (n `div` 1000) ++ "thousand" ++ hundreds (n `mod` 1000)
	    | otherwise = hundreds n

hundreds n | n `div` 100 > 0 && n `mod` 100 == 0 = ones (n `div` 100) ++ "hundred"
	   | n `div` 100 > 0 = ones (n `div` 100) ++ "hundredand" ++ subhundred (n `mod` 100)
	   | otherwise = subhundred n

subhundred n | n < 10 = ones n
	     | n < 20 = teens n
	     | n < 30 = "twenty" ++ ones (n - 20)
	     | n < 40 = "thirty" ++ ones (n - 30)
	     | n < 50 = "forty" ++ ones (n - 40)
	     | n < 60 = "fifty" ++ ones (n - 50)
	     | n < 70 = "sixty" ++ ones (n - 60)
	     | n < 80 = "seventy" ++ ones (n - 70)
	     | n < 90 = "eighty" ++ ones (n - 80)
	     | otherwise = "ninety" ++ ones (n - 90)

ones 0 = ""
ones 1 = "one"
ones 2 = "two"
ones 3 = "three"
ones 4 = "four"
ones 5 = "five"
ones 6 = "six"
ones 7 = "seven"
ones 8 = "eight"
ones 9 = "nine"
teens 10 = "ten"
teens 11 = "eleven"
teens 12 = "twelve"
teens 13 = "thirteen"
teens 14 = "fourteen"
teens 15 = "fifteen"
teens 16 = "sixteen"
teens 17 = "seventeen"
teens 18 = "eighteen"
teens 19 = "nineteen"

result = length $ concatMap thousands [1..1000]

main = putStrLn $ show result
