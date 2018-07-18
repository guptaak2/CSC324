% - Exercise 8, due (Dec 1, 11:50pm)
%
% General exercise instructions:
% - Exercises must be done *individually*.
% - You may not import any Haskell libraries, unless explicitly told to.
% - You may write helper functions freely; in fact, you are encouraged
%   to do so to keep your code easy to understand.
% - Your grade will be determined by our automated testing.
%   You can find some sample tests on the course webpage.
% - Submit early and often! MarkUs is rather slow when many people
%   submit at once. It is your responsibility to make sure your work is
%   submitted on time.
% - No late submissions will be accepted!
%
% This line creates a module to allow exporting of functions.
% DON'T CHANGE IT!

:- module(ex8,
          [contains/2,
           preorder/2,
           insert/3]).
		   
% The recursive definition of a binary tree is the following:
% An empty tree is a binary tree
% A tree with a root value, and left and right subtrees 
% (which are binary trees), is a binary tree
% Here's how we might represent one in Prolog.
% The empty tree is represented by the atom empty.
% A non-empty tree is represented by the predicate node(X, Left, Right) 
% where X is an atom (representing the root of the tree), and Left and 
% Right are Prolog tree representations, representing the left and right 
% subtrees.
% Here's an example of how we might represent a binary tree in Prolog. 
% Note that the second and third arguments to node must either be 
% empty or node(...).
% node(10, node(3, empty, node(5, empty, empty))
%          node(20, node(15, empty, empty)
%                   node(30, empty, empty))).
		   
		   
% contains(T, Elem)
% Elem is an atom, T is a valid tree, and Elem is contained in T. 
% You may assume that T is fully instantiated.
contains(node(Elem, _, _), Elem).
contains(node(_, T1, _), Elem) :- contains(T1, Elem).
contains(node(_, _ ,T2), Elem) :- contains(T2, Elem).

% preorder(T, List)
% List is a list containing the elements of T (a tree) in preorder traversal. 
% You may assume that T is fully instantiated.
preorder(empty, []).
preorder(node(E, T1, T2), List) :- preorder(T1, L1), append([E|L1], L2, List), preorder(T2, L2).

% insert(T1, Elem, T2)
% T1 and T2 are valid binary search trees,where T2 is equal to T1, 
% except it has an extra leaf with value Elem. 
% You may assume that Elem does not appear in T1. 
% (This is the most naive implementation of BST insertion, 
% where you traverse the tree down until you reach the bottom, 
% and put the new element there. Don't try anything fancier that you learned
% in CSC263.) You may asume that T1 and Elem are fully instantiated.
insert(T1, Elem, T2) :- preorder(T1, L1), helper(L1, Elem, L2), preorder(T2, L2). 

helper(L, Elem, [Elem|L]).
helper([H|L], Elem, [H|LE]) :- helper(L, Elem, LE).

