{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
    existsKey,
    removeA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key =
	let (a, b) = head alist
	in 
	if (a == key) then 
		b
	else
		lookupA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA [] (key, val) = [(key, val)]
insertA alist (key, val) = 
	let (a, b) = head alist
	in
	if (a == key) then
		alist
	else
		(a, b) : (insertA (tail alist) (key, val))

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA [(a, b)] (key, val) = 
	if (a == key) then
		[(a, val)]
	else
		[(a, b)]
updateA [] (key, val) = []
updateA alist (key, val) = 
	let (a, b) = head alist
	in
	if (a == key) then
		(a, val) : (tail alist)
	else
		(a, b) : (updateA (tail alist) (key, val))

-- checks if key already exists
existsKey [] key = False
existsKey alist key =
	let (a, b) = head alist
	in
	if (a == key) then
		True
	else
		existsKey (tail alist) key

-- removes a key-value pair
removeA :: Eq a => AList a b -> a -> AList a b
removeA alist key = 
	filter (\k ->
		let (temp_key, value) = k
		in
		temp_key /= key)
	alist