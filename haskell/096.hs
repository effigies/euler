#!/usr/bin/runhaskell

import Array
import Control.Applicative
import Data.List (nub, delete)
import qualified Data.Set as S
import Control.Monad (guard)

data Column = LL | LC | LR | CL | CC | CR | RL | RC | RR
	deriving (Enum, Eq, Ord, Show, Ix)
data Row = TT | TM | TB | MT | MM | MB | BT | BM | BB
	deriving (Enum, Eq, Ord, Show, Ix)

data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Blank
	deriving (Enum, Eq, Ord)

instance Read Value where
	readsPrec _ ('1':x) = [(One,x)]
	readsPrec _ ('2':x) = [(Two,x)]
	readsPrec _ ('3':x) = [(Three,x)]
	readsPrec _ ('4':x) = [(Four,x)]
	readsPrec _ ('5':x) = [(Five,x)]
	readsPrec _ ('6':x) = [(Six,x)]
	readsPrec _ ('7':x) = [(Seven,x)]
	readsPrec _ ('8':x) = [(Eight,x)]
	readsPrec _ ('9':x) = [(Nine,x)]
	readsPrec _ ('0':x) = [(Blank,x)]
	readsPrec _ ('_':x) = [(Blank,x)]
	readsPrec _ _	    = []

instance Show Value where
	show One	= show 1
	show Two	= show 2
	show Three	= show 3
	show Four	= show 4
	show Five	= show 5
	show Six	= show 6
	show Seven	= show 7
	show Eight	= show 8
	show Nine	= show 9
	show Blank	= "_"

type Sudoku = Array (Row, Column) Value

sudoku :: [Value] -> Sudoku
sudoku = array ((TT,LL),(BB,RR)) . zip (range ((TT,LL),(BB,RR)))

--pretty :: Sudoku -> String
pretty board = unlines $ map (show . (board !) =<<) [(,) r <$> [LL .. RR] | r <- [TT .. BB]]

test =  "003020600\
	\900305001\
	\001806400\
	\008102900\
	\700000008\
	\006708200\
	\002609500\
	\800203009\
	\005010300"

test2 =	"200080300\
	\060070084\
	\030500209\
	\000105408\
	\000000000\
	\402706000\
	\301007040\
	\720040060\
	\004010003"

test50 = "300200000\
	 \000107000\
	 \706030500\
	 \070009080\
	 \900020004\
	 \010800050\
	 \009040301\
	 \000702000\
	 \000008006"

allValues = S.fromAscList [One .. Nine]

complement :: [Value] -> [Value]
complement vs = S.toList . S.difference allValues $ S.fromList vs 

possibilities :: Sudoku -> (Row, Column) -> [Value]
possibilities board (r,c) | board ! (r,c) /= Blank = return $ board ! (r,c)
			  | otherwise		   = complement . map (board !) $ neighbors (r,c)

-- neighbors (r,c) = nub $ ($) <$> ([rneighbors,cneighbors,bneighbors] <*> [(r,c)])
neighbors = nub . ([rneighbors, cneighbors, bneighbors] >>=) . flip ($)

rneighbors :: (Row,Column) -> [(Row,Column)]
rneighbors (r,c) = delete (r,c) ((,) <$> [r] <*> [LL .. RR])

cneighbors :: (Row,Column) -> [(Row,Column)]
cneighbors (r,c) = delete (r,c) ((,) <$> [TT .. BB] <*> [c])

bneighbors :: (Row,Column) -> [(Row,Column)]
bneighbors (r,c) = delete (r,c) ((,) <$> rows r <*> cols c)

cols :: Column -> [Column]
cols c | c `elem` lfts = lfts
       | c `elem` ctrs = ctrs
       | c `elem` rits = rits 
	where
		lfts = [LL,LC,LR]
		ctrs = [CL,CC,CR]
		rits = [RL,RC,RR]

rows :: Row -> [Row]
rows r | r `elem` tops = tops
       | r `elem` mids = mids
       | r `elem` btms = btms 
	where
		tops = [TT,TM,TB]
		mids = [MT,MM,MB]
		btms = [BT,BM,BB]



pass board level = do
	r <- [TT .. BB]
	c <- [LL .. RR]
	guard (board ! (r,c) == Blank)
	level board (r,c)

-- level1 :: Sudoku -> [((Row,Column),Value)]
level1 board (r,c) = do
	let ps = possibilities board (r,c)
	guard (length ps == 1)
	return ((r,c),head ps)

level2 board (r,c)= do
	let	rs = filter ((Blank ==) . (board !)) $ rneighbors (r,c)
		cs = filter ((Blank ==) . (board !)) $ cneighbors (r,c)
		bs = filter ((Blank ==) . (board !)) $ bneighbors (r,c)
		rps = rs >>= possibilities board
		cps = cs >>= possibilities board
		bps = bs >>= possibilities board
		invalid p = and [p `elem` rps, p `elem` cps, p `elem` bps]
		ps = filter (not . invalid) $ possibilities board (r,c)
	guard (length ps == 1)
	return ((r,c),head ps)

solve :: Sudoku -> (Sudoku -> (Row,Column) -> [((Row,Column),Value)]) -> Sudoku
solve board level | null fills	= board
	    	  | otherwise		= solve (board // fills) level
	where
		fills = pass board level

result = ""

main = print result

