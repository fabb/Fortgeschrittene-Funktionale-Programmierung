module AufgabeFFP2 where

-- Assignment 2.1
{-
Sei p = 2 natürliche Zahl. Paare der Form (p,p+2) heißen Primzahlpaar genau
dann, wenn p und p + 2 beides Primzahlen sind.
Schreiben Sie eine 0-stellige Haskell-Rechenvorschrift pps :: [(Integer,Integer)],
die den Strom der Primzahlpaare generiert.
Beispiele:
take 10 pps ->> [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),
(59,61),(71,73),(101,103),(107,109)]
pps!!20 ->> (347,349)
head (drop 30 pps) ->> (809,811)
-}

-- prime couples
pps :: [(Integer,Integer)]
pps = filter (\(x,y) -> x == y-2) $ zip primes $ tail primes

-- primes with sieve of eratosthenes
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]


-- Assignment 2.2
{-
Die Haskell-Rechenvorschrift
pow :: Int -> Integer
pow 0 = 1
pow n = pow (n-1) + pow (n-1)
berechnet die Funktion 2n, n = 0.
Implementieren Sie analog zum Beispiel zur Berechnung der Fibonacci-Zahlen
aus der Vorlesung (vgl. Chapter 2, Memoization) eine Variante powFast ::
Int -> Integer der Rechenvorschrift pow, die die Memoizationsidee aus dem
Fibonacci-Beispiel aufgreift und die Berechnung auf eine Memo-Tafel abstützt:
powFast :: Int -> Integer
powFast 0 = ...
powFast n = ...
...
Vergleichen Sie (ohne Abgabe!) anhand von nach und nach größer gewählten
Argumenten das unterschiedliche Laufzeitverhalten der Rechenvorschriften pow
und powFast.
-}

-- fast power function using memo table
powFast :: Int -> Integer
powFast 0 = 1
powFast n = powFastList !! (n-1) + powFastList !! (n-1)

powFastList = [powFast x | x <- [0..]]

-- plain power function not using memo table
pow :: Int -> Integer
pow 0 = 1
pow n = pow (n-1) + pow (n-1)

-- Assignment 2.3
{-
Gegeben sei die Funktion g:
g(z,k) = 1 + z +z2
2!+z3
3!+ ··· =
k
X
n=0
zn
n!
Implementieren Sie zwei Haskell-Rechenvorschriften
f :: Int -> Int -> Float
fMT :: Int -> Int -> Float
zur Berechnung der Funktion g, wobei die Implementierung von fMT eine (oder
mehrere) Memo-Tafel(n) verwendet (z.B. eine Memo-Tafel für jeden z-Wert),
die von f nicht. Beide Funktionen f und fMT mögen sich auf die Hilfsfunktion
h abstützen
h(z,i) =zi
i!
mit deren Hilfe sich g wie folgt ausdrücken lässt:
g(z,k) =
k
X
i=0
h(z,i)
Vergleichen Sie (ohne Abgabe!) auch hier wieder das Laufzeitverhalten der bei-
den Implementierungen miteinander.
-}

-- plain f
f :: Int -> Int -> Float
f = undefined

-- f using memo tables
fMT :: Int -> Int -> Float
fMT = undefined


-- Assignment 2.4
{-
Sei n = 1 eine natürliche Zahl und sei d1d2...dkdie Folge der Dezimalziffern
von n. Sei weiters p1,p2,p3,...,pk,... die Folge der Primzahlen. Dann heißt die
Zahl
2d13d25d3...pdk
k
die Gödelzahl von n. Z.B. hat die Zahl 42 die Gödelzahl 144 = 24*32, die Zahl
402 die Gödelzahl 400 = 24* 30* 52.
1. Schreiben Sie eine Haskell-Rechenvorschrift gz :: Integer -> Integer,
die positive Argumente auf ihre Gödelzahl abbildet, nicht-positive Argu-
mente auf 0.
2. Schreiben Sie eine 0-stellige Haskell-Rechenvorschrift gzs :: [Integer],
die den Strom der Gödelzahlen beginnend für 1 liefert, d.h. das erste Listen-
element ist die Gödelzahl von 1, das zweite Listenelement die Gödelzahl
von 2, usw.
-}

-- goedel number calculator
gz :: Integer -> Integer
gz = undefined

-- goedel number stream
gzs :: [Integer]
gzs = undefined

