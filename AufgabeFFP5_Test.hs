module AufgabeFFP5_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP5

import Array


a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)] :: Array Int Int
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),(7,2),(8,0),(9,(-1))] :: Array Int Int
c = array (1,5) [(1,2),(2,3),(3,(-10)),(4,1),(5,4)] :: Array Int Int

data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Ix,Show)
d :: Array Week String
d = array (Tue,Sat) [(Wed,"work"),(Thu,"study"),(Tue,"study"),(Fri,"chill"),(Sat,"relax")]


cases1 = TestLabel "Assignment 5.1" $ TestList [

  TestCase $ assertEqual "Fail mas" (12) (mas a)
  
  ]


cases2 = TestLabel "Assignment 5.2" $ TestList [

  TestCase $ assertEqual "Fail amas" ([(3,7),(4,7)]) (amas a)
  ,
  TestCase $ assertEqual "Fail amas" ([(1,7),(1,8),(4,7),(4,8)]) (amas b)
  
  ]


cases3 = TestLabel "Assignment 5.3" $ TestList [

  TestCase $ assertEqual "Fail lmas" ((3,7)) (lmas a)
  ,
  TestCase $ assertEqual "Fail lmas" ((1,8)) (lmas b)
  ,
  TestCase $ assertEqual "Fail lmas" ((1,2)) (lmas c)
  
  ]


cases4 = TestLabel "Assignment 5.4" $ TestList [

  TestCase $ assertEqual "Fail minIndex" (4) (minIndex a (>5))
  ,
  TestCase $ assertEqual "Fail minIndex" (2) (minIndex a (<0))
  ,
  TestCase $ assertEqual "Fail minIndex" (3) (minIndex a (even))
  ,
  TestCase $ assertEqual "Fail minIndex" (1) (minIndex b (odd))
  ,
  TestCase $ assertEqual "Fail minIndex error" (Nothing) (minIndex' b (>100))
  ,
  TestCase $ assertEqual "Fail minIndex" (Sat) (minIndex d (=="relax"))
  ,
  TestCase $ assertEqual "Fail minIndex" (Wed) (minIndex d (=="work"))
  ,
  TestCase $ assertEqual "Fail minIndex" (Fri) (minIndex d (=="chill"))
  ,
  TestCase $ assertEqual "Fail minIndex" (Tue) (minIndex d (/="chill"))
  ,
  TestCase $ assertEqual "Fail minIndex error" (Nothing) (minIndex' d (=="swim"))
  
  ]


main = runTestTT $ TestList [cases1, cases2, cases3, cases4]
