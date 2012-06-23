module AufgabeFFP9 where

import Test.QuickCheck
import AufgabeFFP5
import Array


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

-- function description
f2 :: a -> b
f2 = undefined

