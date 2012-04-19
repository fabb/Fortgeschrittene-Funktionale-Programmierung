module AufgabeFFP3_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP3


cases1_1 = TestLabel "Assignment 3.1.1" $ TestList [

  TestCase $ assertEqual "generator Fail 1" (generator []) ([[]])
  ,
  TestCase $ assertEqual "generator Fail 2" (generator [(5,3),(2,7),(2,6),(10,100)]) ([[(5,3),(2,7),(2,6),(10,100)],[(5,3),(2,7),(2,6)],[(5,3),(2,7),(10,100)],[(5,3),(2,7)],[(5,3),(2,6),(10,100)],[(5,3),(2,6)],[(5,3),(10,100)],[(5,3)],[(2,7),(2,6),(10,100)],[(2,7),(2,6)],[(2,7),(10,100)],[(2,7)],[(2,6),(10,100)],[(2,6)],[(10,100)],[]])
  ,
  TestCase $ assertEqual "transformer Fail 1" (transformer []) ([])
  ,
  TestCase $ assertEqual "transformer Fail 2" (transformer [[]]) ([([],0,0)])
  ,
  TestCase $ assertEqual "transformer Fail 3" (transformer [[(1,1),(2,3)],[(3,4),(5,6)]]) ([([(1,1),(2,3)],3,4),([(3,4),(5,6)],8,10)])
  ,
  TestCase $ assertEqual "filter Fail 1" (AufgabeFFP3.filter 1 []) ([])
  ,
  TestCase $ assertEqual "filter Fail 2" (AufgabeFFP3.filter 1 [([],0,0)]) ([])
  ,
  TestCase $ assertEqual "filter Fail 3" (AufgabeFFP3.filter 5 [([(5,3),(2,7),(2,6),(10,100)],19,116),([(5,3),(2,7),(2,6)],9,16),([(5,3),(2,7),(10,100)],17,110),([(5,3),(2,7)],7,10),([(5,3),(2,6),(10,100)],17,109),([(5,3),(2,6)],7,9),([(5,3),(10,100)],15,103),([(5,3)],5,3),([(2,7),(2,6),(10,100)],14,113),([(2,7),(2,6)],4,13),([(2,7),(10,100)],12,107),([(2,7)],2,7),([(2,6),(10,100)],12,106),([(2,6)],2,6),([(10,100)],10,100),([],0,0)]) ([([(5,3)],5,3),([(2,7),(2,6)],4,13),([(2,7)],2,7),([(2,6)],2,6)])
  ,
  TestCase $ assertEqual "selector Fail 1" (selector1 []) ([])
  ,
  TestCase $ assertEqual "selector Fail 2" (selector1 [([(5,3)],5,3),([(2,7),(2,6)],4,13),([(2,7)],2,7),([(2,6)],2,6)]) ([([(2,7),(2,6)],4,13)])
  ,
  TestCase $ assertEqual "Fail 1" ((selector1 . AufgabeFFP3.filter 5 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([([(2,7),(2,6)],4,13)])
  ,
  TestCase $ assertEqual "Fail 2" ((selector1 . AufgabeFFP3.filter 13 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([([(2,7),(10,100)],12,107)])
  ,
  TestCase $ assertEqual "Fail 3" ((selector1 . AufgabeFFP3.filter 1 . transformer . generator) [(5,3),(2,7),(2,6),(10,100)]) ([])
  ,
  TestCase $ assertEqual "Fail 4" ((selector1 . AufgabeFFP3.filter 5 . transformer . generator) [(5,13),(2,7),(2,6),(10,100)]) (reverse [([(2,7),(2,6)],4,13),([(5,13)],5,13)])
  
  ]


cases1_2 = TestLabel "Assignment 3.1.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (selector2 []) ([])
  ,
  TestCase $ assertEqual "Fail 2" (selector2 [([(5,13)],5,13),([(2,7),(2,6)],4,13),([(2,7)],2,7),([(2,6)],2,6)]) ([([(2,7),(2,6)],4,13)])
  ,
  TestCase $ assertEqual "Fail 3" ((selector2 . AufgabeFFP3.filter 5 . transformer . generator) [(5,13),(2,7),(2,6),(10,100)]) ([([(2,7),(2,6)],4,13)])
  
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
