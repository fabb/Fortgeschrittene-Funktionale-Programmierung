module AufgabeFFP3 where

-- Assignment 3.1
{-
Wir betrachten folgende einfache Variante eines Knapsack- oder Rucksackpro-
blems.
Gegeben ist eine endliche Menge von Gegenst�nden, die durch ihr Gewicht und
ihren Wert gekennzeichnet sind. Aufgabe ist es, den Rucksack unter verschie-
denen Randbedingungen bestm�glich zu bepacken, z.B. so, dass die Summe
der Werte der eingepackten Gegenst�nde maximal ist, ohne ein vorgegebenes
H�chstgewicht zu �berschreiten.
Dazu sollen vier Funktionen generator, transformer, filter, und selector
geschrieben werden, deren Komposition
selector . filter . transformer . generator
die gestellte Aufgabe erf�llt.
Wir verwenden folgende Typen und Deklarationen zur Modellierung des Ruck-
sackproblems:
type Weight
= Int
-- Gewicht
type Value
= Int
-- Wert
type Item
= (Weight,Value)
-- Gegenstand als Gewichts-/Wertpaar
type Items
= [Item]
-- Menge der anfaenglich gegebenen
Gegenstaende
type Load
= [Item]
-- Auswahl aus der Menge der anfaenglich
gegebenen Gegenstaende; moegliche
Rucksackbeladung, falls zulaessig
type Loads
= [Load]
-- Menge moeglicher Auswahlen
type LoadWghtVal = (Load,Weight,Value) -- Eine moegliche Auswahl mit
Gesamtgewicht/-wert dieser Auswahl
type MaxWeight
= Weight
-- Hoechstzulaessiges Rucksackgewicht
generator
:: Items -> Loads
transformer :: Loads -> [LoadWghtVal]
filter
:: [LoadWghtVal] -> MaxWeight -> [LoadWghtVal]
selector
:: [LoadWghtVal] -> [LoadWghtVal]

Die einzelnen Funktionen leisten dabei folgendes:
� generator: baut aus der gegebenen Menge von Gegenst�nden die Menge
aller m�glichen Auswahlen auf (ohne auf Zul�ssigkeit zu achten).
� transformer: erg�nzt die m�glichen Auswahlen jeweils um deren Gesamt-
gewicht und -wert
� filter: streicht alle Auswahlen, die nicht zul�ssig sind, z.B. das zul�ssige
H�chstgewicht einer Auswahl �bersteigen.
� selector: w�hlt aus der Menge der zul�ssigen Auswahlen alle diejenigen
aus, die bez�glich des vorgegebenen Optimalit�tsziels am besten sind. Dies
kann eine, mehrere oder keine Auswahl sein, wenn z.B. keine Auswahl
zul�ssig ist.
1. Implementieren Sie die Funktionen generator, transformer, filter zu-
sammen mit einer Funktion selector1 derart, dass selector1 die oder
diejenigen Auswahlen mit h�chstem Wert liefert:
(selector1 . filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 5
->> [([(2,7),(2,6)],4,13)]
(selector1 . filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 13
->> [([(2,7),(10,100)],12,107)]
(selector1 . filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 1
->> []
(selector1 . filter . transformer . generator) [(5,13),(2,7),(2,6),(10,100)] 5
->> [([(2,7),(2,6)],4,13),([(5,13)],5,13)]
2. Implementieren Sie zus�tzlich eine Funktion selector2, die bei gleichem
Gesamtwert die oder diejenigen Auswahlen herausgreift, die diesen Wert
mit geringstem Gesamtgewicht erreichen.
(selector2 . filter . transformer . generator) [(5,13),(2,7),(2,6),(10,100)] 5
->> [([(2,7),(2,6)],4,13)]
Hinweis: Die Reihenfolgen der Elemente in den Ergebnislisten spielt keine Rolle.
-}

type Weight = Int -- Gewicht
type Value = Int -- Wert
type Item = (Weight,Value) -- Gegenstand als Gewichts-/Wertpaar
type Items = [Item] -- Menge der anfaenglich gegebenen Gegenstaende
type Load = [Item] -- Auswahl aus der Menge der anfaenglich gegebenen Gegenstaende; moegliche Rucksackbeladung, falls zulaessig
type Loads = [Load] -- Menge moeglicher Auswahlen
type LoadWghtVal = (Load,Weight,Value) -- Eine moegliche Auswahl mit Gesamtgewicht/-wert dieser Auswahl
type MaxWeight = Weight -- Hoechstzulaessiges Rucksackgewicht


-- Assignment 3.1.1

generator :: Items -> Loads
generator [] = [[]]
generator (x:xs) = (map (x:) (generator xs)) ++ generator xs

transformer :: Loads -> [LoadWghtVal]
transformer = map (\xs -> (xs, sum (map fst xs), sum (map snd xs)))

filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
filter maxweight = Prelude.filter (\(load,_,_) -> (not . null) load) . Prelude.filter (\(_,weight,_) -> weight <= maxweight)

selector1 :: [LoadWghtVal] -> [LoadWghtVal]
selector1 xs = Prelude.filter (\(_,_,value) -> value == maxval) xs
	where maxval = maximum (map (\(_,_,value) -> value) xs)

-- Assignment 3.1.2
 
selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 xs = Prelude.filter (\(_,weight,_) -> weight == minweight) $ selector1 xs
	where minweight = minimum (map (\(_,weight,_) -> weight) (selector1 xs))
 


-- Assignment 3.2
{-
F�r Binomialkoeffizienten gilt folgende Beziehung:
 n
k
!
=
 n - 1
k - 1
!
+
 n - 1
k
!
Daraus l�sst sich folgende Implementierung zur Berechnung der Binomialkoef-
fizienten ableiten:
binom :: (Integer,Integer) -> Integer
binom (n,k)
| k==0 || n==k = 1
| otherwise
= binom (n-1,k-1) + binom (n-1,k)
Schreiben Sie nach dem Vorbild aus Kapitel 2.4 der Vorlesung zwei effizientere
Varianten zur Berechnung der Binomialkoeffizienten mithilfe von
1. Stromprogrammierung: binomS :: (Integer,Integer) -> Integer
2. Memoization: binomM :: (Integer,Integer) -> Integer
Vergleichen Sie (ohne Abgabe!) das Laufzeitverhalten der drei Implementierun-
gen binom, binomS und binomM miteinander.
-}

binom :: (Integer,Integer) -> Integer
binom (n,k)
	| k==0 || n==k = 1
	| otherwise = binom (n-1,k-1) + binom (n-1,k)


-- Assignment 3.2.1

binomS :: (Integer,Integer) -> Integer
binomS (n,k) = pd !! fromIntegral n !! fromIntegral k


-- stream of pascal triangle from assignment 1
pd :: [[Integer]]
pd = pd2


pd1 = map pd1_ [1..]
pd1_ n
	| n == 1 = [1]
	| otherwise = zipWith (+) (0 : (pd1_ (n-1))) ((pd1_ (n-1)) ++ [0])

pd2 = [1] : zipWith (zipWith (+)) (map (0:) pd2) (map (++[0]) pd2)

pd3 = [1] : map (\xs -> zipWith (+) (0:xs) (xs ++ [0])) pd3

pd4 = [1] : [zipWith (+) (0:x) (x++[0]) | x<-pd]


-- Assignment 3.2.2

binomM :: (Integer,Integer) -> Integer
binomM (n,k)
	| k==0 || n==k = 1
	| otherwise = binomMT !! (fromIntegral n-1) !! (fromIntegral k-1) + binomMT !! (fromIntegral n-1) !! fromIntegral k

binomMT = [[binomM (n,k) | k <- [0..]] | n <- [0..]]

