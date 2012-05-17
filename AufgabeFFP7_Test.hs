module AufgabeFFP7_Test where

import Test.QuickCheck
import AufgabeFFP7

main = do
    quickCheck prop_BufferI_empty
    quickCheck prop_BufferI_insert
    quickCheck prop_BufferI_delete
    quickCheck prop_BufferI_left
    quickCheck prop_BufferI_right
    quickCheck prop_BufferI_atLeft
    quickCheck prop_BufferI_atRight
	
    quickCheck prop_ssfn_eq_minfree_a
    quickCheck prop_ssfn_eq_minfree_b
