module AufgabeFFP8_Test where

import Test.QuickCheck
import AufgabeFFP8

main = do
    quickCheck prop_allImplsEq_a
    quickCheck prop_allImplsEq_b
