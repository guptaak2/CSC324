#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

(define (subsets lst)
    (if (empty? lst) lst
      (if (-< #t #f)
          (append (list (first lst)) (subsets (rest lst)))
          (subsets (rest lst)))
      ))

; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define (sudoku-4 puzzle)
  (?- sudoku-helper (map (λ (lst)
                           (map (λ (square)
                                  (if (string? square)
                                      (-< 1 2 3 4)
                                      square))
                                lst))
                         puzzle)))

; checks sudoku board to make sure that 1,2,3,4 appear in it and abide to sudoku rules
; i.e. 1,2,3,4 appear exactly once in every row and column by sorting every row and column
; there are 12 combinations that need to be checked, hence 12 if-statements

(define (sudoku-helper row)
    (if (equal? '(1 2 3 4) (sort (list (first (first row)) (first (second row)) (
                              first (third row)) (first (fourth row))) < ))
        (if (equal? '(1 2 3 4) (sort (list (second (first row)) (second (second row)) (
                              second (third row)) (second (fourth row))) < ))
            (if (equal? '(1 2 3 4) (sort (list (third (first row)) (third (second row)) (
                              third (third row)) (third (fourth row))) < ))
                (if (equal? '(1 2 3 4) (sort (list (fourth (first row)) (fourth (second row)) (
                              fourth (third row)) (fourth (fourth row))) < ))
                    (if (equal? '(1 2 3 4) (sort (list (first (first row)) (first (second row)) (
                              second (first row)) (second (second row))) < ))
                        (if (equal? '(1 2 3 4) (sort (list (first (third row)) (first (fourth row)) (
                              second (third row)) (second (fourth row))) < ))
                            (if (equal? '(1 2 3 4) (sort (list (third (first row)) (third (second row)) (
                              fourth (first row)) (fourth (second row))) < ))
                                (if (equal? '(1 2 3 4) (sort (list (third (third row)) (third (fourth row)) (
                              fourth (third row)) (fourth (fourth row))) < ))
                                    (if (equal? '(1 2 3 4) (sort (first row) < ))
                                        (if (equal? '(1 2 3 4) (sort (second row) < ))
                                            (if (equal? '(1 2 3 4) (sort (third row) < ))
                                                (if (equal? '(1 2 3 4) (sort (fourth row) < ))
               #t #f) #f) #f) #f) #f) #f) #f) #f) #f) #f) #f) #f))

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#

(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <expr>)
     (foldl <combine> <init> (all <expr>))]
    ))