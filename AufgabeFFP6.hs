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
eval = undefined


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

bla a i = map (amap conv) $ f a i

f = undefined


yield :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield a i = map (amap conv) $ yield' a i

yield' :: Array Int Int -> Int -> [Array Int ArithOp]
yield' = yield'_bt


yield_bt :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield_bt a i = map (amap conv) $ yield'_bt a i

yield'_bt :: Array Int Int -> Int -> [Array Int ArithOp]
yield'_bt = undefined


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

	