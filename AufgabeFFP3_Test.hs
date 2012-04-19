module AufgabeFFP3_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP3


cases1_1 = TestLabel "Assignment 3.1.2" $ TestList [

  TestCase $ assertEqual "Fail 1" ((selector1 . AufgabeFFP3.filter 5 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([([(2,7),(2,6)],4,13)])
  ,
  TestCase $ assertEqual "Fail 2" ((selector1 . AufgabeFFP3.filter 13 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([([(2,7),(10,100)],12,107)])
  ,
  TestCase $ assertEqual "Fail 3" ((selector1 . AufgabeFFP3.filter 1 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([])
  ,
  TestCase $ assertEqual "Fail 4" ((selector1 . AufgabeFFP3.filter 5 . transformer . generator) [(5,13),(2,7),(2,6),(10,100)]) ([([(2,7),(2,6)],4,13),([(5,13)],5,13)])
  
  ]


cases1_2 = TestLabel "Assignment 3.1.2" $ TestList [

  TestCase $ assertEqual "Fail 1" ((selector2 . AufgabeFFP3.filter 5 . transformer . generator) [(5,13),(2,7),(2,6),(10,100)]) ([([(2,7),(2,6)],4,13)])
  
  ]


cases2_1 = TestLabel "Assignment 3.2.1" $ TestList [

  TestCase $ assertEqual "Fail 1" (binomS (1,1)) (binom (1,1))
  ,
  TestCase $ assertEqual "Fail 2" (binomS (10,7)) (binom (10,7))
  
  ]


cases2_2 = TestLabel "Assignment 3.2.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (binomM (1,1)) (binom (1,1))
  ,
  TestCase $ assertEqual "Fail 2" (binomM (10,7)) (binom (10,7))
  
  
  ]


main = runTestTT $ TestList [cases1_1, cases1_2, cases2_1, cases2_2]
