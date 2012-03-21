module AufgabeFFP1_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP1


cases1 = TestLabel "Assignment 1.1" $ TestList [

  TestCase $ assertEqual "pof2s: first 7 not equal" (take 7 pof2s) ([1,2,4,8,16,32,64])
  
  ]


cases2 = TestLabel "Assignment 1.2" $ TestList [

  TestCase $ assertEqual "pd: first ones not equal" (take 5 pd) ([ [1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1] ])

  ]


cases3 = TestLabel "Assignment 1.3" $ TestList [

  TestCase $ assertEqual "fibdiag 1" (fibdiag 1) ([1])
  ,
  TestCase $ assertEqual "fibdiag 2" (fibdiag 2) ([1])
  ,
  TestCase $ assertEqual "fibdiag 3" (fibdiag 3) ([1,1])
  ,
  TestCase $ assertEqual "fibdiag 4" (fibdiag 4) ([1,2])
  ,
  TestCase $ assertEqual "fibdiag 5" (fibdiag 5) ([1,3,1])
  ,
  TestCase $ assertEqual "fibdiag 8" (fibdiag 5) ([1,6,10,4])
  
  ]


cases4 = TestLabel "Assignment 1.4" $ TestList [

  TestCase $ assertEqual "fibdiags: first ones not equal" (take 5 fibdiags) ([ [1], [1], [1,1], [1,2], [1,3,1] ])
  
  ]


cases5 = TestLabel "Assignment 1.5" $ TestList [

  TestCase $ assertEqual "fibspd: first ones not equal" (take 5 fibspd) ([1,1,2,3,5])
    
  ]

main = runTestTT $ TestList [cases1, cases2, cases3, cases4, cases5]
