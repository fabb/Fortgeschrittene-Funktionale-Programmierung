module AufgabeFFP9 where

import Test.QuickCheck
import AufgabeFFP5
import Array
import Data.List


-- Assignment 9.1
{-
In Kapitel 4.2 der Vorlesung ist mittels “equational reasoning” ein Algorith-
mus zur Lösung des MNSS-Problems entwickelt worden, dessen Berechnungs-
aufwand linear in der Länge der Argumentliste ist.
Versuchen Sie auf ähnliche Weise zu einer möglicherweise effizienteren Vari-
ante Ihres Verfahrens zur Berechnung der maximalen Abschnittssumme einer
Liste zu gelangen, d.h. zu einer effizienteren Lösung für das MAS-Problem von
Aufgabenblatt 5.
-}

mss [] = error "Minus Infinity"
mss [x] = x
mss [x,y] = maximum [x,y,x+y]
mss xs = (\(s,c) -> max s c) (foldl h (start (take 3 xs)) (drop 3 xs))
start [x,y,z] = (maximum [x+y+z,y+z,z], maximum [x,x+y,y])
h (s, c) x = (if s>0 then s+x else x, max c s)


-- History below

{-
mss xs = (\(_,s,c) -> max s c) (foldl h (start (take 3 xs)) (drop 3 xs))
start [x,y,z] = (0, maximum [x+y+z,y+z,z], maximum [x,x+y,y])
h (e, s, c) x = (e, (max s e)+x, max c s)
-}

{-
mss xs = (\(_,s,c,_) -> max s c) (foldl h (start (take 3 xs)) (drop 3 xs))
start [x,y,z] = (0, maximum [x+y+z,y+z,z], maximum [x,x+y,y], x+z)
h (e, s, c, f) x = (e, (max s e)+x, max c s, max f ((max f c) + x))
-}

--mss = (\(_,s,c,_) -> max s c) . foldl h (0, minusInfty,minusInfty,minusInfty)
--minusInfty = -10000000000000000000000000000000000000000
--mss = (\(_,s,c,_) -> max s c) . tuple (safeMaximum . map sum) . pickall
--mss = (\(_,s,c,_) -> max s c) . tuple (maximum . map sum) . pickall
--mss = maximum . map sum . (\(_,s,c,_) -> s++c) . pickall


-- stupid hack
--safeMaximum = maximum . (:)(-10000000000000000000000000000000000000000)

--tuple f (w,x,y,z) = (f w, f x, f y, f z)

-- allowed: F* T+ F*
{-
data State = E -- F* -- Empty
	| S -- F* T+ -- Suffix of Ts -- final state
	| C -- F* T+ F+ -- Closed segment -- final state
	| F -- F* T+ F+ T+ -- Fail
	deriving (Show, Eq)
-}

{-
pickall xs = (pick E xs, pick S xs, pick C xs, pick F xs)

pick :: State -> [a] -> [[a]]
pick q = extract . 
	filter ((== q) . foldl step E . map snd) .
	markings

segs :: [a] -> [[a]]
segs = extract . filter seg . markings

seg = (\s -> s == S || s == C) . foldl step E . map snd

step E False = E
step E True = S

step S False = C
step S True = S

step C False = C
step C True = F

step F _ = F
-}

{-
pickall = foldl step ([[]],[],[],[])

step (ess, sss, css, fss) x =
	(ess,
	map (++[x]) (sss++ess),
	css ++ sss,
	fss ++ map (++[x]) (fss++css))

markings :: [a] -> [[(a,Bool)]]
markings xs = [zip xs bs |
	bs <- booleans (length xs)]

booleans 0 = [[]]
booleans n = [b:bs | b <- [True,False],
	bs <- booleans (n-1)]

extract :: [[(a,Bool)]] -> [[a]]
extract = map (map fst . filter snd)
-}


-- QuickCheck

allEqual :: Eq a => [a] -> Bool
allEqual [] = undefined
allEqual xs = all (== head xs) $ tail xs

mas5 xs = fromIntegral $ AufgabeFFP5.mas $ listArray (0, length xs - 1) $ map fromIntegral xs

allMas :: [Integer] -> [Integer]
allMas xs = map (\f -> f xs) [mas5, mss]

checkAllMasEqual :: [Integer] -> Bool
checkAllMasEqual = allEqual . allMas

prop_mssImplsEq :: [Integer] -> Property
prop_mssImplsEq xs = invariant xs ==> checkAllMasEqual xs

invariant :: [Integer] -> Bool
invariant = not . null


-- Assignment 9.2
{-
Samurai-Puzzles sind, wie in der folgenden Abbildung illustriert, aus 5 sich
überlappenden Sudoku-Puzzles zusammengesetzt. Wie beim Sudoku ist es die
Aufgabe, alle leeren Felder so mit Zahlen von 1 bis 9 zu füllen, dass in jeder
Zeile, Reihe und 3×3-Kasten die Zahlen von 1 bis 9 genau einmal vorkommen.
In Kapitel 4.3 der Vorlesung wird mittels “equational reasoning” ein Algorith-
mus zur Lösung von Sudoku-Puzzles entwickelt. Erweitern Sie diese Lösung so,
dass Sie zur Lösung von Samurai-Puzzles verwendet werden kann.
-}

sudoku1 = [
	['0','5','0','0','6','0','0','0','1'],
	['0','0','4','8','0','0','0','7','0'],
	['8','0','0','0','0','0','0','5','2'],
	['2','0','0','0','5','7','0','3','0'],
	['0','0','0','0','0','0','0','0','0'],
	['0','3','0','6','9','0','0','0','5'],
	['7','9','0','0','0','0','0','0','8'],
	['0','1','0','0','0','6','5','0','0'],
	['5','0','0','0','3','0','0','6','0']
	]

sudoku2 = [
	['0','0','0','0','6','0','0','8','0'],
	['0','2','0','0','0','0','0','0','0'],
	['0','0','1','0','0','0','0','0','0'],
	['0','7','0','0','0','0','1','0','2'],
	['5','0','0','0','3','0','0','0','0'],
	['0','0','0','0','0','0','4','0','0'],
	['0','0','4','2','0','1','0','0','0'],
	['3','0','0','7','0','0','6','0','0'],
	['0','0','0','0','0','0','0','5','0']
	]


solve = search . choices
search m
	| not (safe m) = []
	| complete m' = [map (map head) m']
	| otherwise = concat (map search (expand1 m'))
		where m' = prune m

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits = ['1'..'9']
blank = (== '0')

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map choice)
choice d = if blank d then digits else [d]

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [ [x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . groupS . map groupS

groupS :: [a] -> [[a]]
groupS [] = []
groupS xs = take 3 xs : groupS (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
	where
		fixed = [d | [d] <- row]
		remove xs ds = if singleton ds then ds else ds \\ xs
			where
				singleton [_] = True
				singleton _ = False

pruneBy f = f . map pruneRow . f

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

expand1 :: Matrix Choices -> [Matrix Choices]
expand1 rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c<-cs]
	where
	(rows1,row:rows2) = break (any smallest) rows
	(row1, cs:row2) = break smallest row
	smallest cs = length cs == n
	n = minimum (counts rows)
	counts = filter (/=1) . map length . concat
	break p xs = (takeWhile (not . p) xs, dropWhile (not . p) xs)

complete = all (all single)
	where
		single [_] = True
		single _ = False

safe m = all ok (rows m) &&
	all ok (cols m) &&
	all ok (boxs m)
		where
			ok row = nodups [d | [d] <- row]
