module AufgabeFFP6_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP6


import Data.List
import Data.Ix
import Array


cases1 = TestLabel "Assignment 6.1" $ TestList [

  TestCase $ assertEqual "eval" (0) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(+)),(2,(-))]))
  ,
  TestCase $ assertEqual "eval" (5) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(*)),(2,(+))]))
  ,
  TestCase $ assertEqual "eval" (-3) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(-)),(2,(*))]))
  
  ]


cases2 = TestLabel "Assignment 6.2" $ TestList [

  TestCase $ assertEqual "import Data.Array.IArray not supported on Hugs'" (True) (False)
  ,
  TestCase $ assertEqual "yield'" ([array (1,2) [(1,Plus),(2,Plus)], array (1,2) [(1,Times),(2,Times)]]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertEqual "yield'" ([]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertEqual "yield'" ([array (1,2) [(1,Plus),(2,Minus)], array (1,2) [(1,Div),(2,Times)], array (1,2) [(1,Div),(2,Div)]]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertEqual "yield" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield" ([0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  ,
  TestCase $ assertEqual "yield_bt" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield_bt" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield_bt" ([0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  ,
  TestCase $ assertEqual "yield_gtf" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield_gtf" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield_gtf" ([0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  
  ]


cases3 = TestLabel "Assignment 6.3" $ TestList [

  TestCase $ assertEqual "TODO" (True) (False)
  ,
  TestCase $ assertEqual "TODO" (False) (True)
  
  ]


main = runTestTT $ TestList [cases1, cases2, cases3]
