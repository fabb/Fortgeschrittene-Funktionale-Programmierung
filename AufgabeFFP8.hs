module AufgabeFFP8 where

import Test.QuickCheck
import Data.List
import Array

-- Assignment 8.1
{-
In Kapitel 4.1 der Vorlesung sind verschiedene Algorithmen zur Lösung des
“Smallest Free Number (SFN)”-Problems angegeben, darunter:
(i) Mithilfe der Funktion minfree, die sich in ihrer Grundversion zur Lösung
des SFN-Problems auf die Differenz des Stroms natürlicher Zahlen und der
anfänglich gegebenen Zahlenmenge abstützt, siehe Folie 247.
(ii) Zwei Array-basierte Algorithmen:
– Mithilfe der Funktion checklist, siehe Folie 251.
– Mithilfe der Funktion countlist, siehe Folie 252.
(iii) Drei “teile-und-herrsche”-basierte Algorithmen:
– Der “Basic Divide-and-Conquer-Algorithm”, siehe Folie 256.
– Der “Refined Divide-and-Conquer-Algorithm”, siehe Folie 259.
– Der “Optimized Divide-and-Conquer-Algorithm”, siehe Folie 262.
• Implementieren Sie die obigen 6 Algorithmen zur Lösung des SFN-Problems
wie in der Vorlesung angegeben und vergleichen Sie (ohne Abgabe!) die relati-
ve Performanz der verschiedenen Implementierungen miteinander. Untersuchen
Sie insbesondere, ob Sie die Aussagen zu den relativen Geschwindigkeiten der
Algorithmen von Folie 263 bestätigt finden.
• Die obigen 3 nach dem “Teile-und-Herrsche”-Prinzip vorgehenden Implemen-
tierungen folgen nicht dem Schema des “Teile-und-Herrsche”-Funktionals aus
Kapitel 3.1 der Vorlesung.
Geben Sie für jeden dieser 3 Algorithmen eine entsprechende Implementie-
rung mit Hilfe des “Teile-und-Herrsche”-Funktionals aus Kapitel 3.1 der Vorle-
sung an. Geben Sie dazu jeweils die Implementierungen der Funktionen indiv,
solve, divide und combine an und verwenden Sie für die 3 Implementierun-
gen für jeden Namen jeweils den Präfix b , r und o für die “basic”, “refined”
und “optimized” Algorithmusvariante. Die Funktionen heißen also b indiv,
r indiv, o indiv usw.
• Validieren Sie mithilfe von QuickCheck, dass die obigen 9 Implementierungen
zur Lösung des SFN-Problems
– für (zulässige) duplikatfreie gleiche Argumente die gleiche Funktion festle-
gen, also gleiche Resultate liefern
– für (zulässige) nichtduplikatfreie gleiche Argumente sich möglicherweise
Unterschiede beobachten lassen.
Definieren Sie dafür zwei entsprechende Eigenschaften
prop allImplsEq a :: [Int] -> Bool
prop allImplsEq b :: [Int] -> Property
in Ihrem Programm. Die obigen Eigenschaften sollen also “wahr” sein, wenn alle
9 Implementierungen gleichzeitig überprüft werden und alle dabei zum selben
Ergebnis kommen.
Für die Eigenschaft prop allImplsEq b soll durch eine geeignete Vorbedingung
sichergestellt werden, dass negative Listenelemente enthaltende automatisch
generierte Testfälle verworfen und nicht als gültiger Testfall behandelt werden.
Verwenden Sie für die 9 Varianten von minfree die Namen
• minfree bv: “basic version”
• minfree chl: “checklist”
• minfree col: “countlist”
• minfree b: “basic divide-and-conquer”
• minfree r: “refined divide-and-conquer”
• minfree o: “optimized divide-and-conquer”
• minfree bhof: “basic divide-and-conquer mittels higher-order function”
• minfree rhof: “refined divide-and-conquer mittels higher-order function”
• minfree ohof: “optimized divide-and-conquer mittels higher-order function”
-}

type Nat = Int -- Typ der natürlichen Zahlen beginnend ab 0


minfree_bv :: [Nat] -> Nat -- basic version
minfree_bv xs = head $ ([0..]) \\ xs


minfree_chl :: [Nat] -> Nat -- checklist
minfree_chl = search_checklist . checklist

search_checklist :: Array Int Bool -> Int
search_checklist = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n)
	(zip (filter (<=n) xs) (repeat True))
	where n = maximum (0:xs)


minfree_col :: [Nat] -> Nat -- countlist
minfree_col = search_countlist . countlist

search_countlist :: Array Int Int -> Int
search_countlist = length . takeWhile (/=0) . elems

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,n) (zip xs (repeat 1))
	where n = maximum (0:xs)

--sort_countlist xs = concat [replicate k x | (x,k) <- assocs (countlist xs)]


minfree_b :: [Nat] -> Nat -- basic divide-and-conquer
minfree_b xs = if (null (([0..b-1]) \\ us))
	then (head ([b..] \\ vs))
	else (head ([0..] \\ us))
	where
		(us,vs) = partition (<b) xs
		b = 10 -- any natural number


minfree_r :: [Nat] -> Nat -- refined divide-and-conquer => no duplicates allowed
minfree_r = minfrom_r 0

minfrom_r a xs
	| null xs = a
	| length us == b-a = minfrom_r b vs
	| otherwise = minfrom_r a us
	where
		(us,vs) = partition (<b) xs
		b = a + 1 + (n `div` 2)
		n = length xs


minfree_o :: [Nat] -> Nat -- optimized divide-and-conquer => no duplicates allowed
minfree_o xs = minfrom_o 0 (length xs, xs)

minfrom_o a (n,xs)
	| n == 0 = a
	| m == b-a = minfrom_o b (n-m,vs)
	| otherwise = minfrom_o a (m,us)
	where
		(us,vs) = partition (<b) xs
		b = a + 1 + (n `div` 2)
		m = length us


type CheckFor = [Nat]

minfree_bhof :: [Nat] -> Nat -- basic divide-and-conquer mittels higher-order function
minfree_bhof xs = divideAndConquer b_indiv b_solve b_divide b_combine (xs,[])

b_indiv :: ([Nat],CheckFor) -> Bool
b_indiv prob@(p,c) = not $ null c

b_solve :: ([Nat],CheckFor) -> Nat
b_solve prob@(p,c) = head (c \\ p)

b_divide :: ([Nat],CheckFor) -> [([Nat],CheckFor)]
b_divide prob@(p,c) = if (null (([0..b-1]) \\ us))
		then [(vs, [b..])]
		else [(us, [0..])]
	where
		(us,vs) = partition (<b) p
		b = 10 -- any natural number

b_combine :: ([Nat],CheckFor) -> [Nat] -> Nat
b_combine _ sols = head sols


type From = Nat

minfree_rhof :: [Nat] -> Nat -- refined divide-and-conquer mittels higher-order function
minfree_rhof prob = divideAndConquer r_indiv r_solve r_divide r_combine (prob,0)

r_indiv :: ([Nat],From) -> Bool
r_indiv prob@(p,a) = null p

r_solve :: ([Nat],From) -> Nat
r_solve prob@(p,a) = a

r_divide :: ([Nat],From) -> [([Nat],From)]
r_divide prob@(p,a) = [newprob]
	where
		(us,vs) = partition (<b) p
		b = a + 1 + (n `div` 2)
		n = length p
		newprob = if length us == b-a
			then (vs,b)
			else (us,a)

r_combine :: ([Nat],From) -> [Nat] -> Nat
r_combine _ sols = head sols


type Length = Int

minfree_ohof :: [Nat] -> Nat -- optimized divide-and-conquer mittels higher-order function
minfree_ohof prob = divideAndConquer o_indiv o_solve o_divide o_combine (prob,0,length prob)

o_indiv :: ([Nat],From,Length) -> Bool
o_indiv prob@(p,a,n) = n == 0

o_solve :: ([Nat],From,Length) -> Nat
o_solve prob@(p,a,n) = a

o_divide :: ([Nat],From,Length) -> [([Nat],From,Length)]
o_divide prob@(p,a,n) = [newprob]
	where
		(us,vs) = partition (<b) p
		b = a + 1 + (n `div` 2)
		m = length us		
		newprob = if m == b-a
			then (vs,b,n-m)
			else (us,a,m)

o_combine :: ([Nat],From,Length) -> [Nat] -> Nat
o_combine _ sols = head sols


-- HOF from lecture
divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb
	= dAC initPb
		where
			dAC pb
				| indiv pb = solve pb
				| otherwise = combine pb (map dAC (divide pb))


-- QuickCheck Properties zum Vergleich der 9 Implementierungen

allEqual :: Eq a => [a] -> Bool
allEqual [] = undefined
allEqual xs = all (== head xs) $ tail xs

allMinfree :: [Nat] -> [Nat]
allMinfree xs = map (\f -> f xs) [minfree_bv, minfree_chl, minfree_col, minfree_b, minfree_r, minfree_o, minfree_bhof, minfree_rhof, minfree_ohof]

checkAllMinfreeEqual :: [Nat] -> Bool
checkAllMinfreeEqual = allEqual . allMinfree

prop_allImplsEq_a :: [Int] -> Bool
prop_allImplsEq_a = checkAllMinfreeEqual

-- Für die Eigenschaft prop allImplsEq b soll durch eine geeignete Vorbedingung sichergestellt werden, dass negative Listenelemente enthaltende automatisch generierte Testfälle verworfen und nicht als gültiger Testfall behandelt werden.
prop_allImplsEq_b :: [Int] -> Property
prop_allImplsEq_b xs = invariant xs ==> checkAllMinfreeEqual xs

invariant :: [Nat] -> Bool
invariant xs = invariant_positive xs && invariant_nodups xs

invariant_positive :: [Nat] -> Bool
invariant_positive = all (>=0)

invariant_nodups :: [Nat] -> Bool
invariant_nodups xs = xs == nub xs
