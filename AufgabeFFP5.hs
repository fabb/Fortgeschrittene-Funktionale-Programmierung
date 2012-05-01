module AufgabeFFP5 where

import Data.Maybe
import Data.Ix
import Array

{-
Seien low und high ganze Zahlen mit low = high und sei a ein eindimensionales
Feld mit kleinstem Index low und größtem Index high. Seien weiter i und j ganze
Zahlen mit low = i = j = high.
Dann heißt der Ausschnitt von a mit kleinstem Index i und größtem Index j, in
Zeichen a ? [i,j], ein Abschnitt von a.
Für das Weitere nehmen wir an, dass die Elemente von a ebenfalls ganze Zahlen
sind und dass b ein Abschnitt von a ist. Die Abschnittsumme von b ist dann die
Summe der Elementwerte von b.
Beispiel:
a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]
Dann gilt: Die Abschnittsumme von
a ? [2,5] ist: (-5) + 0 + 9 + 2 = 6
a ? [7,9] ist: 2 + (-5) + 1 = -2
a ? [4,7] ist: 9 + 2 + (-1) + 2 = 12
a ? [3,8] ist: 0 + 9 + 2 + (-1) + 2 + (-5) = 7
-}

-- Assignment 5.1
{-
Schreiben Sie eine Haskell-Rechenvorschrift mas :: Array Int Int -> Int,
die angewendet auf ein Feld den maximalen Wert der Abschnittsummen dieses
Feldes berechnet.
Beispiel: Für das Feld a liefern die Abschnitte a ? [3,7] und a ? [4,7] die größte
Abschnittsumme mit Wert 12. Es gilt:
mas a ->> 12
-}

-- maximaler wert der abschnittsummen
mas :: Array Int Int -> Int
mas = undefined


-- Assignment 5.2
{-
Schreiben Sie eine Haskell-Rechenvorschrift amas :: Array Int Int -> [(Int,Int)],
die angewendet auf ein Feld die Liste derjenigen Abschnitte berechnet, darge-
stellt jeweils durch ihren kleinsten und größten Index, deren Abschnittsumme
maximal für das Argumentfeld ist.
Die Ergebnisliste soll dabei so geordnet sein, dass (i,j) genau dann weiter links
in der Ergebnisliste stehen soll als (k,l), wenn gilt:
i < k
oder
i = k ? j < k
Beispiel: Für die Felder a und b liefert die Funktion amas die Resultate:
amas a ->> [(3,7),(4,7)]
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),
(7,2),(8,0),(9,(-1))]
amas b ->> [(1,7),(1,8),(4,7),(4,8)]
-}

-- liste der abschnitte mit maximaler abschnittsumme
-- sortiert zuerst nach anfangsindex und dann nach endindex
amas :: Array Int Int -> [(Int,Int)]
amas = undefined


-- Assignment 5.3
{-
Schreiben Sie eine Haskell-Rechenvorschrift lmas :: Array Int Int -> (Int,Int),
die den längsten Abschnitt mit maximaler Abschnittsumme berechnet. Gibt es
mehrere, so liefert die Funktion lmas den Abschnitt mit kleinstem Anfangsin-
dex.
Beispiel:
lmas a ->> (3,7)
lmas b ->> (1,8)
c = array (1,5) [(1,2),(2,3),(3,(-10)),(4,1),(5,4)]
lmas c ->> [(1,2)]
Hinweis: Die Funktionen mas, amas und lmas werden ausschließlich mit vollständig
definierten Feldern aufgerufen; es gibt in Argumenten keine Indizes mit “unde-
finiertem” Elementwert.
-}

-- längster abschnitt mit maximaler abschnittsumme
-- mehrere -> kleinster anfangsindex
lmas :: Array Int Int -> (Int,Int)
lmas = undefined


-- Assignment 5.4
{-
Sei a ein eindimensionales Feld und wf eine Wahrheitswertfunktion. Schreiben
Sie eine Haskell-Rechenvorschrift minIndex :: (Ix a, Show a) => Array a
b -> (b -> Bool) -> a, die angewendet auf ein Feld und eine Wahrheitswert-
funktion den kleinsten Index bestimmt, für dessen Elementwert die Wahrheits-
wertfunktion erfüllt ist, also den Wert True liefert. Erfüllt kein Element die
Wahrheitswertfunktion, bricht die Berechnung mittels eines Aufrufs der Funk-
tion error ab.
Implementieren Sie die Funktion minIndex mithilfe des “Teile und Herrsche”-
Prinzips. Stützen Sie dazu die Implementierung von minIndex auf das Funk-
tional divideAndConquer aus Kapitel 3.1 der Vorlesung ab. Geben Sie da-
zu Implementierungen der Funktionen mi-indiv, mi-solve, mi-divide und
mi-combine an und rufen Sie damit das Funktional divideAndConquer ent-
sprechend auf.
Wandeln Sie dabei das als Feld gegebene Argument in eine Liste um, damit Sie
divideAndConquer unverändert anwenden können.
Beispiele:
minIndex a (>5)
->> 4
minIndex a (<0)
->> 2
minIndex a (even) ->> 3
minIndex b (odd)
->> 1
minIndex b (>100) ->> error "No matching index"
data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Ix,Show)
d :: Array Week String
d = array (Tue,Sat) [(Wed,"work"),(Thu,"study"),(Tue,"study"),
(Fri,"chill"),(Sat,"relax")]
minIndex d (=="relax") ->> Sat
minIndex d (=="work")
->> Wed
minIndex d (=="chill") ->> Fri
minIndex d (/="chill") ->> Tue
minIndex d (=="swim")
->> error "No matching index"
Hinweis: Die Funktion minIndex wird ausschließlich mit vollständig definierten
Feldern aufgerufen; es gibt in Argumenten keine Indizes mit “undefiniertem”
Elementwert.
-}

-- function description
minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex a f = fromMaybe (error "No matching index") (minIndex' a f)

-- for testability we don't use error
minIndex' :: (Ix a, Show a) => Array a b -> (b -> Bool) -> Maybe a
minIndex' = undefined
