module AufgabeFFP4 where

import Data.Ix
import Array

-- Assignment 4.1
{-
Wir betrachten eine Variante des Knapsack- bzw. Rucksackproblems von Auf-
gabenblatt 3.
Gegeben ist eine endliche Menge von Gegenst�nden, die durch ihr Gewicht und
ihren Wert gekennzeichnet sind. Aufgabe ist es, den Rucksack so zu bepacken,
dass die Summe der Werte der eingepackten Gegenst�nde maximal ist, ohne
ein vorgegebenes H�chstgewicht zu �berschreiten.
Dazu sollen drei Funktionen succKnp, goalKnp und knapsack geschrieben wer-
den, so dass knapsack sich auf das Funktional searchDfs abst�tzt und das
Optimierungsproblem mittels backtracking l�st. Dabei wird ein Baum aufge-
baut, dessen Knoten mit folgenden Informationen benannt sind: dem Wert
und Gewicht des aktuellen Rucksackinhalts, dem nicht zu �berschreitenden
H�chstgewicht, der Liste der noch ausw�hlbaren noch nicht eingepackten Ge-
genst�nde, sowie der Liste der bereits eingepackten Gegenst�nde.
type NodeKnp = [Value,Weight,MaxWeight,[Object],SolKnp]
Wir verwenden folgende Typen und Deklarationen zur Modellierung des Ruck-
sackproblems:
type Weight
= Int
-- Gewicht
type Value
= Int
-- Wert
type MaxWeight
= Weight
-- Hoechstzulaessiges Rucksackgewicht
type Object
= (Weight,Value)
-- Gegenstand als Gewichts-/Wertpaar
type Objects
= [Object]
-- Menge der anfaenglich gegebenen
Gegenstaende
type SolKnp
= [Object]
-- Auswahl aus der Menge der anfaenglich
gegebenen Gegenstaende; moegliche
Rucksackbeladung, falls zulaessig
type NodeKnp
= (Value,Weight,MaxWeight,[Object],SolKnp]) -- s.o.
succKnp :: NodeKnp -> [NodeKnp]
succKnp (v,w,limit,objects,psol) = ...
goalKnp :: NodeKnp -> Bool
goalKnp (_,w,limit,((w�,_):_),_) = ...
knapsack :: Objects -> MaxWeight -> (SolKnp,Value)
knapsack objects limit = ...
where ... = ... searchDfs ...
Beispiel:
knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10
->> ([(2,3),(2,3),(3,4),(3,4)],14)
Hinweis: Die Reihenfolge der Elemente in der Ergebnisliste spielt keine Rolle.
Gibt es mehr als eine beste L�sung, reicht es, eine davon auszuw�hlen.
-}

type Weight = Int -- Gewicht
type Value = Int -- Wert
type MaxWeight = Weight -- Hoechstzulaessiges Rucksackgewicht
type Object = (Weight,Value) -- Gegenstand als Gewichts-/Wertpaar
type Objects = [Object] -- Menge der anfaenglich gegebenen Gegenstaende
type SolKnp = [Object] -- Auswahl aus der Menge der anfaenglich gegebenen Gegenstaende; moegliche Rucksackbeladung, falls zulaessig
type NodeKnp = (Value,Weight,MaxWeight,[Object],SolKnp) -- wird ein Baum aufgebaut, dessen Knoten mit folgenden Informationen benannt sind: dem Wert und Gewicht des aktuellen Rucksackinhalts, dem nicht zu �berschreitenden H�chstgewicht, der Liste der noch ausw�hlbaren noch nicht eingepackten Gegenst�nde, sowie der Liste der bereits eingepackten Gegenst�nde.

succKnp :: NodeKnp -> [NodeKnp]
succKnp (v,w,limit,objects,psol) = undefined

goalKnp :: NodeKnp -> Bool
goalKnp (_,w,limit,((w',_):_),_) = undefined

knapsack :: Objects -> MaxWeight -> (SolKnp,Value)
knapsack objects limit = undefined
	--where ... = ... searchDfs ...


-- backtracking HOF from LVA
searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
	where
		search' s
			| stackEmpty s = []
			| goal (top s) = top s : search' (pop s)
			| otherwise
				= let x = top s
					in search' (foldr push (pop s) (succ x))


-- Assignment 4.2
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
f�r 0 < k < n,
 n
k
!
= 1
f�r k = 0 oder k = n und
 n
k
!
= 0
sonst.
Schreiben Sie nach dem Vorbild aus Kapitel 3.5 der Vorlesung eine Variante
zur Berechnung der Binomialkoeffizienten mithilfe dynamischer Programmie-
rung. St�tzen Sie Ihre Implementierung dazu auf das Funktional dynamic und
geeignete Funktionen compB und bndsB ab:
binomDyn :: (Integer,Integer) -> Integer
binomDyn (m,n) = ... where ... dynamic compB... bndsB...
Vergleichen Sie (ohne Abgabe!) das Laufzeitverhalten der Implementierung
binomDyn mit denen der drei Implementierungen binom, binomS und binomM
von Aufgabenblatt 3 miteinander.
-}

binomDyn :: (Integer,Integer) -> Integer
binomDyn (m,n) = undefined
--where ... dynamic compB... bndsB...

--bndsB :: Int -> (Int,Int)
bndsB = undefined

--compB :: Table Int Int -> Int -> Int
compB = undefined

{-
bndsFibs :: Int -> (Int,Int)
bndsFibs n = (0,n)
compFib :: Table Int Int -> Int -> Int
compFib t i
| i <= 1 = i
| otherwise = findTable t (i-1) + findTable t (i-2)
-}


-- dynamic programming HOF from LVA
--dynamic :: (Ix coord) => (Table entry coord -> coord -> entry) -> (coord,coord) -> (Table entry coord)
dynamic compute bnds = t
	where t = newTable (map (\coord -> (coord,compute t coord)) (range bnds))

-- binom naive implementation for comparison
binom :: (Integer,Integer) -> Integer
binom (n,k)
	| k==0 || n==k = 1
	| otherwise = binom (n-1,k-1) + binom (n-1,k)



--module Stack (Stack,push,pop,top,emptyStack,stackEmpty) where

data Stack a = EmptyStk
	| Stk a (Stack a)

push :: a -> Stack a -> Stack a
push x s = Stk x s

pop :: Stack a -> Stack a
pop EmptyStk = error "pop from an empty stack"
pop (Stk _ s) = s

top :: Stack a -> a
top EmptyStk = error "top from an empty stack"
top (Stk x _) = x

emptyStack :: Stack a
emptyStack = EmptyStk

stackEmpty :: Stack a -> Bool
stackEmpty EmptyStk = True
stackEmpty _ = False


--module Table (Table,newTable,findTable,updTable) where

newtype Table a b = Tbl (Array a b)

--newTable :: (Ix b) => [(b,a)] -> Table a b
newTable l = Tbl (array (lo,hi) l)
	where
		indices = map fst l
		lo = minimum indices
		hi = maximum indices

--findTable :: (Ix b) => Table a b -> b -> a
findTable (Tbl a) i = a ! i

--updTable :: (Ix b) => (b,a) -> Table a b -> Table a b
updTable p@(i,x) (Tbl a) = Tbl (a // [p])

