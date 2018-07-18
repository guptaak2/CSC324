{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
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
data Pointer a = P Integer |
                 P2 Integer Integer -- added second constructor
                 deriving Show

data StateOp a = StateOp (Memory -> (a, Memory))

-- A type representing a person with two attributes
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

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

instance Mutable Person where
    get (P2 a b) = StateOp (\mem ->
        let p1 = (P a) :: Pointer Integer
            p2 = (P b) :: Pointer Bool
            (age, mem1) = runOp (get (p1 :: Pointer Integer)) mem
            (bool, mem2) = runOp (get (p2 :: Pointer Bool)) mem
        in ((Person age bool), mem))

    set (P2 a b) person = 
        let (Person age bool) = person
        in set (P a) age >>> set (P b) bool

    def int (Person age student) = StateOp (\mem ->
        let ((P num), val) = runOp (def int age >>> alloc student) mem
        in (P2 int num, val))


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

(@@) :: Pointer p -> Integer -> Pointer q
pointer @@ attr =
    let (P2 attr1 attr2) = pointer in
    if (attr == 0) then
        (P attr1)
    else if (attr == 1) then
        (P attr2)
    else
        error "Attribute does not exist"

isStudent :: Integer
isStudent = 1

age :: Integer
age = 0


personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)
