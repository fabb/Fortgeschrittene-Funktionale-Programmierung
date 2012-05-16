module AufgabeFFP7_Test where

import Test.QuickCheck
import AufgabeFFP7

main = do
    quickCheck prop_BufferI_XXX;
	putStrLn "TODO rest of BufferI properties"
	
    quickCheck prop_ssfn_eq_minfree_a
    quickCheck prop_ssfn_eq_minfree_b
