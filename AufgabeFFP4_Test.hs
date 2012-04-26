module AufgabeFFP4_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP4


cases1 = TestLabel "Assignment 4.1" $ TestList [

  TestCase $ assertEqual "Fail 1" (f1) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f1) (True)
  
  ]


cases2 = TestLabel "Assignment 4.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (f2) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f2) (True)
  
  ]


main = runTestTT $ TestList [cases1, cases2]
