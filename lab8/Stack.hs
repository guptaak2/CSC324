{- Stack Code -}

module Stack (
    Stack,
    StackOp,
    pop,
    push,
    isEmpty,
    (>>>),
    (>~>))
    where

type Stack = [Integer]
type StackOp a = Stack -> (a, Stack)

-- Equivalent type:
-- pop :: Stack -> (Integer, Stack)
pop :: StackOp Integer
pop (x:xs) = (x, xs)

-- Equivalent type:
-- push :: Integer -> (Stack -> ((), Stack))
push :: Integer -> StackOp ()
push n s = ((), n:s)

isEmpty :: StackOp Bool
isEmpty s = (s == [], s)


-- Do f, throw away result, and then do g
(>>>) :: StackOp a -> StackOp b -> StackOp b
(f >>> g) s =            -- or, f >.> g = \s -> ...
    let (_, s1) = f s
    in  g s1

-- Do f, USE the result to create a new StackOp, and do it
(>~>) :: StackOp a -> (a -> StackOp b) -> StackOp b
(f >~> g) s =
    let (x, s1) = f s
        newStackOp = g x
    in  newStackOp s1

removeSecond :: StackOp ()
removeSecond stack = 
    let (x, s1) = pop stack
        (y, s2) = pop s1
        (_, s3) = push x s2
    in 
        ((), s3)

removeThird :: StackOp ()
removeThird stack = 
    let (x, s1) = pop stack
        (y, s2) = pop s1
        (z, s3) = pop s2
        (_, s4) = push y s3
        (_, s5) = push x s4
    in 
        ((), s5)






