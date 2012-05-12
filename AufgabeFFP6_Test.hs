module AufgabeFFP6_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP6

import Control.Monad (unless)


import Data.List
import Data.Ix
import Array


listsUnorderedEqual :: (Eq a, Ord a, Show a) => [a] -> [a] -> Bool
listsUnorderedEqual wanted got = sameLength && equalContent
	where
		sameLength = length wanted == length got
		equalContent = sort wanted == sort got

assertListsUnorderedEqual :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertListsUnorderedEqual preface expected actual =
	unless (listsUnorderedEqual expected actual) (assertFailure msg)
		where msg = (if null preface then "" else preface ++ "\n") ++
			"expected unordered list: " ++ show expected ++ "\n but got: " ++ show actual



cases1 = TestLabel "Assignment 6.1" $ TestList [

  TestCase $ assertEqual "eval" (0) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(+)),(2,(-))]))
  ,
  TestCase $ assertEqual "eval" (5) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(*)),(2,(+))]))
  ,
  TestCase $ assertEqual "eval" (-3) (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(-)),(2,(*))]))
  
  ]


cases2 = TestLabel "Assignment 6.2" $ TestList [

  TestCase $ assertListsUnorderedEqual "yield'" ([array (1,2) [(1,Plus),(2,Plus)], array (1,2) [(1,Times),(2,Times)]]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertListsUnorderedEqual "yield'" ([]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertListsUnorderedEqual "yield'" ([array (1,2) [(1,Plus),(2,Minus)], array (1,2) [(1,Div),(2,Times)], array (1,2) [(1,Times),(2,Div)], array (1,2) [(1,Div),(2,Div)]]) (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertListsUnorderedEqual "yield' div 0" ([array (1,1) [(1,Plus)], array (1,1) [(1,Minus)]]) (yield' (array (1,2) [(1,1),(2,0)]) 1)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_bt" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0) (yield'_bt (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_bt div 0" ([array (1,1) [(1,Plus)], array (1,1) [(1,Minus)]]) (yield'_bt (array (1,2) [(1,1),(2,0)]) 1)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 6) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 6)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 4) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 4)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_gtf" (yield' (array (1,3) [(1,1),(2,2),(3,3)]) 0) (yield'_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 0)
  ,
  TestCase $ assertListsUnorderedEqual "yield'_gtf div 0" ([array (1,1) [(1,Plus)], array (1,1) [(1,Minus)]]) (yield'_gtf (array (1,2) [(1,1),(2,0)]) 1)
  ,
  TestCase $ assertEqual "yield" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield" ([0,0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  ,
  TestCase $ assertEqual "yield_bt" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield_bt" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield_bt" ([0,0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_bt (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  ,
  TestCase $ assertEqual "yield_gtf" ([6,6]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 6))
  ,
  TestCase $ assertEqual "yield_gtf" ([]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 4))
  ,
  TestCase $ assertEqual "yield_gtf" ([0,0,0,0]) (map (eval (array (1,3) [(1,1),(2,2),(3,3)])) (yield_gtf (array (1,3) [(1,1),(2,2),(3,3)]) 0))
  
  ]


cases3 = TestLabel "Assignment 6.3" $ TestList [

  TestCase $ assertEqual "show array of func" ("[array (1,2) [(1,plus),(2,plus)],array (1,2) [(1,times),(2,times)]]") (show ([array (1,2) [(1,(+)),(2,(+))], array (1,2) [(1,(*)),(2,(*))]] :: [Array Int (Int->Int->Int)]))
  ,
  TestCase $ assertEqual "show array of func" ("[array (1,2) [(1,plus),(2,minus)],array (1,2) [(1,div),(2,times)],array (1,2) [(1,times),(2,div)],array (1,2) [(1,div),(2,div)]]") (show ([array (1,2) [(1,(+)),(2,(-))], array (1,2) [(1,(div)),(2,(*))], array (1,2) [(1,(*)),(2,(div))], array (1,2) [(1,(div)),(2,(div))]] :: [Array Int (Int->Int->Int)]))
  
  ]


main = runTestTT $ TestList [cases1, cases2, cases3]
