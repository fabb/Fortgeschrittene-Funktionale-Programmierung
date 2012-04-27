module AufgabeFFP4_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import AufgabeFFP4

import Control.Monad (unless)
import Data.List


type KnapsackWantedSolution = (SolKnp,Value)
type KnapsackWantedSolutions = [KnapsackWantedSolution]
type KnapsackGotSolution = (SolKnp,Value)

knapsackOk :: KnapsackWantedSolutions -> KnapsackGotSolution -> Bool
knapsackOk wanted got = any (equalKnapsack got) wanted
	where
		equalKnapsack (sg,vg) (sw,vw) = vg == vw && equalKnapsackContent sg sw
		equalKnapsackContent sg sw = sort sg == sort sw

assertKnapsackOneOf :: String -> KnapsackWantedSolutions -> KnapsackGotSolution -> Assertion
assertKnapsackOneOf preface expected actual =
	unless (knapsackOk expected actual) (assertFailure msg)
		where msg = (if null preface then "" else preface ++ "\n") ++
			"expected one of: " ++ show expected ++ "\n but got: " ++ show actual


cases1 = TestLabel "Assignment 4.1" $ TestList [

  TestCase $ assertKnapsackOneOf "Fail 1" ([([(2,3),(2,3),(3,4),(3,4)],14)]) (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)
  ,
  TestCase $ assertKnapsackOneOf "Fail 2" ([([],0)]) (knapsack [] 0)
  ,
  TestCase $ assertKnapsackOneOf "Fail 3.1" ([([(2,7),(2,6)],13)]) (knapsack [(5,3),(2,7),(2,6),(10,100)] 5)
  ,
  TestCase $ assertKnapsackOneOf "Fail 3.2" ([([(2,7),(10,100)],107)]) (knapsack [(5,3),(2,7),(2,6),(10,100)] 13)
  ,
  TestCase $ assertKnapsackOneOf "Fail 3.3" ([([],0)]) (knapsack [(5,3),(2,7),(2,6),(10,100)] 1)
  ,
  TestCase $ assertKnapsackOneOf "Fail 3.4" ([([(2,7),(2,6)],13),([(5,13)],13)]) (knapsack [(5,13),(2,7),(2,6),(10,100)] 5)
  
  ]


cases2 = TestLabel "Assignment 4.2" $ TestList [

  TestCase $ assertEqual "Fail 1" (binom (1,1)) (binomDyn (1,1))
  ,
  TestCase $ assertEqual "Fail 2" (binom (10,7)) (binomDyn (10,7))
  
  ]


main = runTestTT $ TestList [cases1, cases2]
