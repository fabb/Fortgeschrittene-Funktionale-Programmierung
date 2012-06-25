module AufgabeFFP9 where

import AufgabeFFP9_Sudokus
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

solve = search . choices

search m
	| not (safeSamurai m) = []
	| complete m' = [map (map (map head)) m']
	| otherwise = concat (map search (expand1 m'))
		where m' = prune m

type SamuraiMatrix a = [Matrix a] -- order is top-left, top-right, bottom-right, bottom-left, middle
type SamuraiGrid = [Grid] -- order is top-left, top-right, bottom-right, bottom-left, middle
type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits = ['1'..'9']
blank = (== '0')

type Choices = [Digit]

choices :: SamuraiGrid -> SamuraiMatrix Choices
choices = map (map (map choice))
choice d = if blank d then digits else [d]

validSamurai :: SamuraiGrid -> Bool
validSamurai = all valid

valid :: Grid -> Bool
valid g = all nodups0 (rows g) && all nodups0 (cols g) && all nodups0 (boxs g)
	where
		nodups0 = nodups . filter (not . blank)

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

prune :: SamuraiMatrix Choices -> SamuraiMatrix Choices
prune = repairSamuraiMatrix . map (pruneBy boxs . pruneBy cols . pruneBy rows)
-- warning: samurai data structure contains duplicated boxes due to overlapping in samurai sudoku
-- these duplicated boxes must be cross updated afterwards

repairSamuraiMatrix :: SamuraiMatrix Choices -> SamuraiMatrix Choices
repairSamuraiMatrix = map boxs . crossupdate . map boxs
	where
		crossupdate :: SamuraiMatrix Choices -> SamuraiMatrix Choices
		crossupdate sm@[s1,s2,s3,s4,s5] =
			[
				take 8 s1 ++ [boxtopleft sm],
				take 6 s2 ++ [boxtopright sm] ++ drop 7 s2,
				[boxbottomright sm] ++ drop 1 s3,
				take 2 s4 ++ [boxbottomleft sm] ++ drop 3 s4,
				[boxtopleft sm] ++ (take 1 . drop 1) s5 ++ [boxtopright sm] ++ (take 3 . drop 3) s5 ++ [boxbottomleft sm] ++ (take 1 . drop 7) s5 ++ [boxbottomright sm]
			]

		boxtopleft sm     = intersectChoices (sm!!0!!8,sm!!4!!0)
		boxtopright sm    = intersectChoices (sm!!1!!6,sm!!4!!2)
		boxbottomright sm = intersectChoices (sm!!2!!0,sm!!4!!8)
		boxbottomleft sm  = intersectChoices (sm!!3!!2,sm!!4!!6)
		intersectChoices = uncurry (zipWith intersect)

expand1 :: SamuraiMatrix Choices -> [SamuraiMatrix Choices]
expand1 sudokus = [sudokus1 ++ [rows1 ++ [row1 ++ [c] : row2] ++ rows2] ++ sudokus2 | c<-cs]
	where
	(sudokus1,rows:sudokus2) = break (any (any smallest)) sudokus
	(rows1,row:rows2) = break (any smallest) rows
	(row1, cs:row2) = break smallest row
	smallest cs = length cs == n
	n = minimum (counts rows)
	counts = filter (/=1) . map length . concat
	break p xs = (takeWhile (not . p) xs, dropWhile (not . p) xs)

complete :: SamuraiMatrix Choices -> Bool
complete = all (all (all single))
	where
		single [_] = True
		single _ = False

safeSamurai :: SamuraiMatrix Choices -> Bool
safeSamurai = all safe

safe :: Matrix Choices -> Bool
safe m = all ok (rows m) &&
	all ok (cols m) &&
	all ok (boxs m)
		where
			ok row = nodups [d | [d] <- row]
