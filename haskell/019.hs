#!/usr/bin/runhaskell

import Control.Applicative

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
	deriving (Show, Enum, Eq)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
	deriving (Show, Enum, Eq)

type Year = Int
type Date = (Int, Month, Year)

weekdays = cycle [Monday .. Sunday]

leap year | year `mod` 400 == 0	= True
	  | year `mod` 100 == 0	= False
	  | year `mod` 4 == 0	= True
	  | otherwise		= False

days Jan _			= 31
days Feb year | leap year	= 29
	      | otherwise	= 28
days Mar _			= 31
days Apr _			= 30
days May _			= 31
days Jun _			= 30
days Jul _			= 31
days Aug _			= 31
days Sep _			= 30
days Oct _			= 31
days Nov _			= 30
days Dec _			= 31

years :: [Year]
years = [1900 .. 2000]
months = flip (,) <$> years <*> [Jan .. Dec]

monthToDays :: Month -> Year -> [Date]
monthToDays month year = (,,) <$> [1 .. days month year] <*> [month] <*> [year]

allDays = zipWith (\w (x,y,z) -> (w,x,y,z)) weekdays (months >>= uncurry monthToDays)

century20 = dropWhile (\(_,_,_,x) -> x < 1901) allDays

firstSundays = filter isFS century20
	where
		isFS (Sunday,1,_,_)	= True
		isFS _			= False

result = length firstSundays

main = print result
