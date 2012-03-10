module Assignment1_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import Assignment1


cases1 = TestLabel "Assignment 1.1" $ TestList [

  TestCase $ assertEqual "Fail 1" (True) (False)
  ,
  TestCase $ assertEqual "Fail 2" (False) (True)
  
  ]


cases2 = TestLabel "Assignment 1.2" $ TestList [

  
  ]


cases3 = TestLabel "Assignment 1.3" $ TestList [

  
  ]


cases4 = TestLabel "Assignment 1.4" $ TestList [

  
  ]


cases5 = TestLabel "Assignment 1.5" $ TestList [

    
  ]

main = runTestTT $ TestList [cases1, cases2, cases3, cases4, cases5]
