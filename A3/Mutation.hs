{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), 
    Value(..), StateOp(..),
    runOp, returnVal, 
    (>>>), (>~>),
    alloc, free
    )
    where

import AList (AList, lookupA, insertA, updateA, existsKey, removeA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

data StateOp a = StateOp (Memory -> (a, Memory))

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Integer where
    get (P val) = StateOp (\mem ->
        ((if existsKey mem val then
            case lookupA mem val of
                IntVal m -> m
            else 
                error "No such key found in memory"), mem))

    set (P pointer) val = StateOp (\mem ->
        ((), (if existsKey mem pointer then
            updateA mem (pointer, IntVal val)
            else
                error "No such key found in memory")))

    def int val = StateOp (\mem ->
        ((if existsKey mem int then
            error "The key already exists in memory"
            else
                ((P int), insertA mem (int, IntVal val)))))

instance Mutable Bool where
    get (P val) = StateOp (\mem ->
        ((if existsKey mem val then
            case lookupA mem val of
                BoolVal m -> m
            else
                error "No such key found in memory"), mem))

    set (P pointer) val = StateOp (\mem ->
        ((), (if existsKey mem pointer then
            updateA mem (pointer, BoolVal val)
            else
                error "No such key found in memory")))

    def int val = StateOp (\mem ->
        ((if existsKey mem int then
            error "The key already exists in memory"
            else
                ((P int), insertA mem (int, BoolVal val)))))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

(>>>) :: StateOp a -> StateOp b -> StateOp b
opA >>> opB = StateOp (\mem ->
    let (_, mem1) = runOp opA mem
    in runOp opB mem1)

(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
a >~> b = StateOp (\mem ->
    let (op, mem1) = runOp a mem
        newStackOp = b op
    in runOp newStackOp mem1)

returnVal :: a -> StateOp a
returnVal x = StateOp (\mem -> (x, mem))

-- helper function to look for and count used memory locations
allocHelper mem count = 
    if existsKey mem count then
        allocHelper mem (count + 1)
    else
        count

alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = StateOp (\mem ->
    runOp (def (allocHelper mem 0) val) mem)

free :: Mutable a => Pointer a -> StateOp ()
free (P pointer) = StateOp (\mem ->
    (if existsKey mem pointer then
        ((), removeA mem pointer)
    else
        error "No such key found in memory"))

