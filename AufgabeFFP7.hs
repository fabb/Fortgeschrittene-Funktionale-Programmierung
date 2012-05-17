module AufgabeFFP7 where

import Test.QuickCheck
import Data.List

-- Assignment 7.1
{-
Ein Editorpuffer kann als Zeichenreihe zusammen mit einer Cursorposition in
folgender Weise in Haskell modelliert werden:
type Buffer = (Int,String)
empty
:: Buffer
-- the empty buffer
insert
:: Char -> Buffer -> Buffer -- insert character before cursor
delete
:: Buffer -> Buffer
-- delete character before cursor
left
:: Buffer -> Buffer
-- move cursor left one character
right
:: Buffer -> Buffer
-- move cursor right one character
atLeft
:: Buffer -> Bool
-- is cursor at left end?
atRight :: Buffer -> Bool
-- is cursor at right end?
– Geben Sie Implementierungen für die obigen Funktionen eines Editorpuf-
fers an.
– Eine Modellierung des Editorpuffers mithilfe des Haskelltyps
type BufferI = (String,String)
wobei die erste Zeichenreihe in umgekehrter Reihenfolge die Zeichen bis zur
Cursorposition, die zweite Zeichenreihe die Zeichen ab der Cursorposition
enthält (einschließlich des Zeichens an der Cursorposition), erlaubt eine
effizientere Implementierung der Pufferoperationen.
Geben Sie Implementierungen für die obigen Pufferfunktionen auf dem
neuen Datentyp BufferI an, die mit emptyI, insertI, usw. bezeichnet
werden sollen.
– Definieren Sie eine Haskell-Rechenvorschrift retrieve mit der Signatur
retrieve :: BufferI -> Buffer, die Puffer in effizienter Darstellung in
ihr bedeutungsgleichesÄquivalent in Standarddarstellung überführt.
–Überprüfen Sie mithilfe von QuickCheck die Korrektheit der Implemen-
tierung der Pufferfunktionen auf BufferI bezüglich der entsprechenden
Funktionen auf Buffer, die wir zu diesem Zweck als Spezifikation der
Pufferoperationen auffassen. Definieren Sie dazu analog zum Beispiel über
Schlangen aus Kapitel 5 der Vorlesung in Ihrem Haskell-Programm für jede
der obigen Funktionen ein (oder mehrere) entsprechende Eigenschaft(en)
und übernehmen Sie für die Bezeichnung dieser Eigenschaften die Namens-
konventionen aus dem Beispiel über Schlangen aus der Vorlesung.
-}

-- Implementierung 1

type Buffer = (Int,String)

empty :: Buffer -- the empty buffer
empty = (0,"")

insert :: Char -> Buffer -> Buffer -- insert character before cursor
insert c b@(p,s) = (p+1, take p s ++ [c] ++ drop p s)

delete :: Buffer -> Buffer -- delete character before cursor
delete b@(p,s) = if atLeft b then b else (p-1, take (p-1) s ++ drop p s)

left :: Buffer -> Buffer -- move cursor left one character
left b@(p,s) = if atLeft b then b else (p-1,s)

right :: Buffer -> Buffer -- move cursor right one character
right b@(p,s) = if atRight b then b else (p+1,s)

atLeft :: Buffer -> Bool -- is cursor at left end?
atLeft (p,_) = p == 0

atRight :: Buffer -> Bool -- is cursor at right end?
atRight (p,s) = p == length s -- cursor is AFTER last char


-- Implementierung 2

type BufferI = (String,String)

emptyI :: BufferI -- the empty BufferI
emptyI = ("","")

insertI :: Char -> BufferI -> BufferI -- insert character before cursor
insertI c (l,r) = (c:l, r)

deleteI :: BufferI -> BufferI -- delete character before cursor
deleteI b@(l:ls,r) = (ls, r)
deleteI b = b

leftI :: BufferI -> BufferI -- move cursor left one character
leftI (l:ls,r) = (ls,l:r)
leftI b = b

rightI :: BufferI -> BufferI -- move cursor right one character
rightI (l,r:rs) = (r:l,rs)
rightI b = b

atLeftI :: BufferI -> Bool -- is cursor at left end?
atLeftI ([],_) = True
atLeftI _ = False

atRightI :: BufferI -> Bool -- is cursor at right end?
atRightI (_,[]) = True
atRightI _ = False


-- Umwandlung

retrieve :: BufferI -> Buffer
retrieve (l,r) = (length l, reverse l ++ r)


-- QuickCheck Properties zum Vergleich von Buffer und BufferI

prop_BufferI_empty = retrieve emptyI == empty
prop_BufferI_insert c b = retrieve (insertI c b) == AufgabeFFP7.insert c (retrieve b)
prop_BufferI_delete b = retrieve (deleteI b) == AufgabeFFP7.delete (retrieve b)
prop_BufferI_left b = retrieve (leftI b) == left (retrieve b)
prop_BufferI_right b = retrieve (rightI b) == right (retrieve b)
prop_BufferI_atLeft b = atLeftI b == atLeft (retrieve b)
prop_BufferI_atRight b = atRightI b == atRight (retrieve b)


-- Assignment 7.2
{-
In Kapitel 4.1 der Vorlesung sind zwei einfache Algorithmen zur Lösung des
“Smallest Free Number (SFN)”-Problems angegeben:
(i) Mithilfe der Funktionen ssfn und sap, siehe Folie 245.
(ii) Mithilfe der Funktion minfree, die sich in ihrer Grundversion zur Lösung
des SFN-Problems auf die Differenz des Stroms natürlicher Zahlen und der
anfänglich gegebenen Zahlenmenge abstützt, siehe Folie 247.
– Implementieren Sie die Funktionen ssfn, sap und minfree wie in der Vor-
lesung angegeben. Ergänzen Sie insbesondere eine Implementierung der
Funktion removeDuplicates.
– Vergleichen Sie (ohne Abgabe!) die relative Performanz der beiden Imple-
mentierungen für die Lösung des SFN-Problems.
– Validieren Sie mithilfe von QuickCheck, dass die Funktionen ssfn und
minfree dieselbe Funktion festlegen, für gleiche (zulässige) Argumente also
gleiche Resultate liefern.
Definieren Sie dafür zwei entsprechende Eigenschaften
prop ssfn eq minfree a :: Nat -> Bool
prop ssfn eq minfree b :: Nat -> Property
in Ihrem Programm, wobei type Nat = [Int] den Typ der natürlichen
Zahlen beginnend ab 0 bezeichnet.
Für die Eigenschaft prop ssfn eq minfree b soll durch eine geeignete Vor-
bedingung sichergestellt werden, dass negative Listenelemente enthaltende
automatisch generierte Testfälle verworfen und nicht als gültiger Testfall
behandelt werden.
-}

-- ssfn from lecture
ssfn :: [Integer] -> Integer
ssfn = (sap 0) . removeDuplicates . quickSort

removeDuplicates = undefined

-- quickSort from lecture
quickSort :: Ord a => [a] -> [a]
quickSort lst = divideAndConquer indiv solve divide combine lst
	where
		indiv ls = length ls <= 1
		solve = id
		divide (l:ls) = [[ x | x <- ls, x <= l], [ x | x <- ls, x > l] ]
		combine (l:_) [l1,l2] = l1 ++ [l] ++ l2

-- HOF from lecture
divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb
	= dAC initPb
		where
			dAC pb
				| indiv pb = solve pb
				| otherwise = combine pb (map dAC (divide pb))

-- sap from lecture
sap :: Integer -> [Integer] -> Integer
sap n [] = 0
sap n (x:xs)
	| n /= x = n
	| otherwise = sap (n+1) xs

type Nat = Int -- Typ der natürlichen Zahlen beginnend ab 0

minfree :: [Nat] -> Nat
minfree xs = head $ ([0..]) \\ xs

-- TODO compare function performance


-- QuickCheck Properties zum Vergleich von Buffer und BufferI

prop_ssfn_eq_minfree_a :: Nat -> Bool
prop_ssfn_eq_minfree_a = undefined


-- Für die Eigenschaft prop ssfn eq minfree b soll durch eine geeignete Vorbedingung sichergestellt werden, dass negative Listenelemente enthaltende automatisch generierte Testfälle verworfen und nicht als gültiger Testfall behandelt werden.
prop_ssfn_eq_minfree_b :: Nat -> Property
prop_ssfn_eq_minfree_b = undefined
