module AufgabeFFP1 where

-- Assignment 1.1
{-
Schreiben Sie eine 0-stellige Haskell-Rechenvorschrift pof2s :: [Integer], die
den Strom der 2er-Potenzen liefert, d.h. den Strom [1,2,4,8,16,32,....
Versuchen Sie, pof2s rekursiv ausschließlich mit Hilfe von map (sowie dem Li-
stenkonstruktor “:” und arithmetischen Funktionen zu definieren).
-}

-- function description

pof2s :: [Integer]
pof2s = 1 : map (*2) pof2s


-- Assignment 1.2
{-
Im folgenden ist ein Anfangsstück des Pascalschen Dreiecks dargestellt:
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
...
Wir bezeichnen die Zeilen dieses Dreiecks als Pascaltupel.
Schreiben Sie eine 0-stellige Haskell-Rechenvorschrift pd :: [[Integer]], die
den Strom der Pascaltupel liefert, d.h. den Strom [[1],[1,1],[1,2,1],[1,3,3,1],....
Versuchen Sie, pd rekursiv ausschließlich mit Hilfe von zipWith (sowie den
Standardkonstruktoren und -operatoren auf Listen und arithmetischen Funk-
tionen zu definieren).
-}

pd :: [[Integer]]
pd = pd2


pd1 = map pd1_ [1..]
pd1_ n
	| n == 1 = [1]
	| otherwise = zipWith (+) (0 : (pd1_ (n-1))) ((pd1_ (n-1)) ++ [0])

pd2 = [1] : zipWith (zipWith (+)) (map (0:) pd2) (map (++[0]) pd2)

pd3 = [1] : map (\xs -> zipWith (+) (0:xs) (xs ++ [0])) pd3

pd4 = [1] : [zipWith (+) (0:x) (x++[0]) | x<-pd]


-- Assignment 1.3
{-
ie folgende Abbildung veranschaulicht, dass das Pascalsche Dreieck implizit
die Folge der Fibonacci-Zahlen enthält.

Schreiben Sie eine Haskell-Rechenvorschrift fibdiag
fibdiag :: Integer -> [Integer]
vor, die angewendet auf ein positives Argument n, n > 0, die Liste der in
Pfeilrichtung angeordneten Summanden liefert, die sich zur n-ten Fibonacci-
Zahl summieren.
Folgende Beispiele zeigen das Aufruf/Resultatverhalten von fibdiag:
fibdiag 1 ->> [1]
fibdiag 2 ->> [1]
fibdiag 3 ->> [1,1]
fibdiag 5 ->> [1,3,1]
fibdiag 8 ->> [1,6,10,4]
-}

fibdiag :: Integer -> [Integer]
fibdiag = fibdiag1

fibdiag1 n = zipWith (!!) (reverse $ take (fromIntegral n) pd) [0..fromIntegral (n - 1) `div` 2]

fibdiag2 n = [pd !! (fromIntegral n-1 - i) !! i | i <- [0..(fromIntegral n-1) `div` 2]]


-- Assignment 1.4
{-
 Schreiben Sie mithilfe von fibdiag eine Haskell-Rechenvorschrift fibdiags,
die den Strom der Diagonalen aus der obigen Abbildung liefert:
fibdiags :: Integer -> [[Integer]]
-}

fibdiags :: [[Integer]]
fibdiags = map fibdiag [1..]


-- Assignment 1.5
{-
Geben Sie weiters eine Haskell-Rechenvorschrift fibspd an, die den Strom der
Fibonacci-Zahlen in der durch die Abbildung nahegelegten Weise aus dem Pas-
calschen Dreieck generiert.
fibspd :: [Integer]
Die Implementierung des Stroms fibspd soll sich dabei geeignet auf den Strom
pd (oder auf fibdiags) aus den vorherigen Teilaufgaben abstützen.
Die Generator/Selektor-Aufrufe sollen folgende Resultate liefern:
take 5 pd
->>
[ [1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1] ]
take 5 fibdiags ->> [ [1], [1], [1,1], [1,2], [1,3,1] ]
take 5 fibspd
->>
[1,1,2,3,5]
-}

fibspd :: [Integer]
fibspd = map sum fibdiags
