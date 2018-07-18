{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser (
    pointerTest, swap, swapCycle) where

import Mutation (
    get, set, def, 
    Mutable, Pointer(..), Memory,
    StateOp(..), Value(..),
    returnVal, (>>>), (>~>)
    )

import AList (AList, lookupA, insertA, updateA, existsKey, removeA)

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest n = def 100 (n + 3) >~> \p1 ->
				def 500 (n > 0) >>>
				returnVal (P 100, P 500)

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap p1 p2 = 
	let pointer = get p2
	in
		get p1 >~> \x ->
		get p2 >~> \y ->
		set p2 x >>>
		set p1 y >>>
		returnVal ()

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle [] = returnVal ()
swapCycle (a:[]) = returnVal ()
swapCycle (a:b:as) = swap a b >>> swapCycle (b:as)