module AufgabeFFPX_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFPX


cases1 = TestLabel "Assignment X.1" $ TestList [

  TestCase $ assertEqual "Fail 1" (f1) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f1) (True)
  
  ]


cases2 = TestLabel "Assignment X.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (f2) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f2) (True)
  
  ]


cases3 = TestLabel "Assignment X.3" $ TestList [

  TestCase $ assertEqual "Fail 1" (f3) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f3) (True)
  
  ]


cases4 = TestLabel "Assignment X.4" $ TestList [

  TestCase $ assertEqual "Fail 1" (f4) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f4) (True)
  
  ]


cases5 = TestLabel "Assignment X.5" $ TestList [

  TestCase $ assertEqual "Fail 1" (f5) (False)
  ,
  TestCase $ assertEqual "Fail 2" (f5) (True)
    
  ]

main = runTestTT $ TestList [cases1, cases2, cases3, cases4, cases5]
