module AufgabeFFP2_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP2


cases1 = TestLabel "Assignment 2.1" $ TestList [

  TestCase $ assertEqual "take 10 pps" (take 10 pps) ([(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)])
  ,
  TestCase $ assertEqual "pps!!20" (pps!!20) ((347,349))
  ,
  TestCase $ assertEqual "head (drop 30 pps)" (head (drop 30 pps)) ((809,811))
    
  ]


cases2 = TestLabel "Assignment 2.2" $ TestList [

  TestCase $ assertEqual "powFast 1" (powFast 0) (pow 0)
  ,
  TestCase $ assertEqual "powFast 2" (powFast 1) (pow 1)
  ,
  TestCase $ assertEqual "powFast 3" (powFast 16) (pow 16)
  ,
  TestCase $ assertEqual "powFast 4" (map powFast [0..16]) (map pow [0..16])
  
  ]


cases3 = TestLabel "Assignment 2.3" $ TestList [

  TestCase $ assertEqual "fMT 1" (fMT 5 6) (f 5 6)
  ,
  TestCase $ assertEqual "fMT 2" (fMT 9 10) (f 9 10)
  
  ]


cases4 = TestLabel "Assignment 2.4" $ TestList [

  TestCase $ assertEqual "gz 1" (gz 42) (144)
  ,
  TestCase $ assertEqual "gz 1" (gz 402) (400)
  ,
  TestCase $ assertEqual "gzs" (take 2 gzs) ([2,4])
  
  ]


main = runTestTT $ TestList [cases1, cases2, cases3, cases4]
