module AufgabeFFP6 where

import Data.List
import Data.Ix
--import Array
import Data.Array.IArray


-- Assignment 6.1
{-
Sei f = (f1,...,fk), k = 2, eine nichtleere endliche Folge ganzer Zahlen und
g = (g1,...,gk-1) eine um 1 kürzere Folge der arithmetischen Operatoren +, *, -
und / mit / ganzzahlige Division mit Rest.
Eine Auswertung von f bzgl. g ist der Wert des Ausdrucks
f1g1f2g2 ... fk-1gk-1fk
ohne Beachtung der Regel Punkt- vor Strichrechnung.
Beispiele:
f = (3,5,2,(-2),7,0) hat bzgl. g = (+,/,*,+,-) den Wert (3+5/2*(-2)+7-0) =
(-1); bzgl. g'= (*,-,+,/,+) den Wert (3 * 5 - 2 + (-2)/7 + 0) = 1.
f = (4,2,3,(-4),5,2) hat bzgl. g = (+,/,*,+,-) den Wert (4+2/3*(-4)+5-2) =
(-5); bzgl. g'= (*,-,+,/,+) den Wert (4 * 2 - 3 + (-4)/5 + 2) = 2.

Schreiben Sie eine Haskell-Rechenvorschrift eval :: Array Int Int -> Array
Int (Int->Int->Int) -> Int, die angewendet auf eine Zahlenfolge f und eine
Operatorfolge g passender Länge den Wert von f bzgl. g im Sinne des vorste-
hend eingeführten Auswertungsbegriffs bestimmt. Divisionen durch 0 führen
dabei zu einem Fehlerabbruch der Auswertung.
Beispiele:
eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(+)),(2,(-))])
->> 0
eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(*)),(2,(+))])
->> 5
eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(-)),(2,(*))])
->> (-3)
Die Funktion eval wird nur auf zueinander passende Zahlen- und Operations-
folgen angewendet.
-}

eval :: Array Int Int -> Array Int (Int->Int->Int) -> Int
eval nums ops = eval' (elems nums) (elems ops)

eval' :: [Int] -> [Int->Int->Int] -> Int
eval' (n:nums) ops = foldl (\num op -> op num) n $ zipWith (\op num -> (`op` num)) ops nums
eval' [] _ = 0


-- Assignment 6.2
{-
Die Funktion yield :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
soll angewendet auf eine Zahlenfolge f und einen Zielwert w eine Liste derjeni-
gen Folgen arithmetischer Operationen liefern, so dass die Auswertung der Ar-
gumentliste bzgl. dieser Operationsfolge den Zielwert w ergibt. Die Reihenfolge
verschiedener Operationsfolgen innerhalb der Ergebnisliste ist dabei unerheb-
lich.
Beispiele:
yield array (1,3) [(1,1),(2,2),(3,3)] 6
->> [array (1,2) (1,(+)),(2,(+)),array (1,2) (1,(*)),(2,(*))]
yield array (1,3) [(1,1),(2,2),(3,3)] 4
->> []
yield array (1,3) [(1,1),(2,2),(3,3)] 0
->> [array (1,2) (1,(+)),(2,(-)),array (1,2) (1,(/)),(2,(*)),
array (1,2) (1,(/)),(2,(/))]
Implementieren Sie zwei unterschiedliche Varianten
– yield bt :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
– yield gtf :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
die funktional äquivalent zur Funktion yield sind, wobei yield bt sich auf das
Backtracking-Funktional aus Kapitel 3.2 aus der Vorlesung und yield gtf sich
auf das generate/transform/filter-Prinzip abstützt.
Implementieren Sie für yield gtf drei Funktionen generate,
transform und filt, so dass sich yield gtf als sequentielle Komposition
filt . transform . generate
ergibt. Die Funktion generate erzeugt alle Operationsfolgen (passender Länge),
transform übernimmt die Auswertung, filt wählt die Auswertungen mit pas-
sendem Zielwert aus.
(Hinweis: Eine unmittelbare Ausgabe der Resultate ist nicht möglich, da der
Resultattyp von yield bt bzw. yield gtf nicht in der Klasse Show liegt. Beide
Funktionen werden nur mit mindestens zweielementigen Zahlenfolgen aufgeru-
fen.)
-}

-- TODO noch nicht abgesegnet
data ArithOp = Plus | Minus | Times | Div deriving (Eq, Show)

conv :: ArithOp -> (Int->Int->Int)
conv Plus = (+)
conv Minus = (-)
conv Times = (*)
conv Div = div


yield :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield a i = map (amap conv) $ yield' a i

yield' :: Array Int Int -> Int -> [Array Int ArithOp]
yield' = yield'_bt


----

type CurrentValue = Int -- current calculation value
type TargetValue = Int -- current calculation value
type Operations = [ArithOp] -- allowed operations
type SeriesOfNumbers = [Int] -- rest of the input number sequence
type SolY = [ArithOp] -- solution is sequence of operations
type NodeY = (CurrentValue,TargetValue,Operations,SeriesOfNumbers,SolY)


yield_bt :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield_bt a i = map (amap conv) $ yield'_bt a i

yield'_bt :: Array Int Int -> Int -> [Array Int ArithOp]
yield'_bt a sval = map (\xs -> listArray (1, length xs) xs) solution
	where
		aElems = elems a
		solutionNodes = searchDfs succY goalY (head aElems, sval, [Plus, Minus, Times, Div], tail aElems, [])
		solution = map (\(_,_,_,_,psol) -> psol) solutionNodes

succY :: NodeY -> [NodeY]
succY (_,_,_,[],_) = []
succY (v,sv,os,ss,psol) = map (\arith -> ((conv arith) v (head ss), sv, os, tail ss, psol ++ [arith])) os'
	where
		os'
			| head ss == 0 = take 3 os -- workaround to avoid division by 0
			| otherwise = os

goalY :: NodeY -> Bool
goalY (v,sv,_,[],_)
	| v == sv = True
	| otherwise = False
goalY _ = False

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

----

yield_gtf :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield_gtf a i = map (amap conv) $ yield'_gtf a i

yield'_gtf :: Array Int Int -> Int -> [Array Int ArithOp]
yield'_gtf = filt . transform . generate



filt = undefined
transform = undefined
generate = undefined


-- Assignment 6.3
{-
Machen Sie den Datentyp Array Int (Int->Int->Int) zu einer Instanz der
Klasse Show. Die arithmetischen Operationen aus den vorigen Aufgaben sollen
als Zeichenreihen
"plus", "minus", "times", "div"
dargestellt werden. Für mögliche andere Operationen wird keine besondere Aus-
gabe verlangt.
-}

{-
instance Show (Array Int (Int->Int->Int)) where
	show = undefined
-}

