module AufgabeFFP4_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP4


cases1 = TestLabel "Assignment 4.1" $ TestList [

  TestCase $ assertEqual "Fail 1" (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10) ([(2,3),(2,3),(3,4),(3,4)],14)
  ,
  TestCase $ assertEqual "Fail 3.1" (knapsack [(5,3),(2,7),(2,6),(10,100)] 5) (([(2,7),(2,6)],13))
  ,
  TestCase $ assertEqual "Fail 3.2" (knapsack [(5,3),(2,7),(2,6),(10,100)] 13) (([(2,7),(10,100)],107))
  ,
  TestCase $ assertEqual "Fail 3.3" (knapsack [(5,3),(2,7),(2,6),(10,100)] 1) (([],0))
  ,
  TestCase $ assertEqual "Fail 3.4" ((knapsack [(5,13),(2,7),(2,6),(10,100)] 5) `elem` [([(2,7),(2,6)],13),([(5,13)],13)]) (True)

  
  ]


cases2 = TestLabel "Assignment 4.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (binomDyn (1,1)) (binom (1,1))
  ,
  TestCase $ assertEqual "Fail 2" (binomDyn (10,7)) (binom (10,7))
  
  ]


main = runTestTT $ TestList [cases1, cases2]
