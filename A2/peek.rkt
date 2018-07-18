; Peek Implementation

; I've implemented peek by basically creating a duplicate
; stack, and every time a choice point is pushed on the choices stack
; I push a quoted-version of that choice point onto the peeks stack
; The functions are implemented at the bottom.


#lang racket
(provide -< next all clear ?-)

#|
(-< <expr> ...)
  Each <expr> is an arbitrary Racket expression.

  Evaluates and returns the first <expr>.
  If there is more than one argument, stores a choice point
  which resumes the program at where the (-< ...) is used,
  but with the remaining choices.
|#
(define-syntax -<
  (syntax-rules ()
    ; When there is only one option, return it.
    [(-< <expr1>) <expr1>]
    ; When there is more than one, return the first and store the rest.
    [(-< <expr1> <expr2> ...)
     (let/cc cont
       ; Push a new choice onto choices and peeks.
       (add-choice! (λ () (cont (-< <expr2> ...))))
       (quoted-push! (λ () '(-< <expr2> ...)))
       <expr1>)]))

(define (next)
  (if (empty? choices)
      "false."
      ((get-choice!))))

; peek the stack 
(define (peek)
  (if (empty? peeks)
      "false."
      ((first peeks))))

#|
(all <expr>)
  <expr> is a Racket expression, possibly containing choice points.
  
  Returns a list of all possible outcomes of evaluating <expr>
  (i.e., making all possible combinations of the choices).
|#
(define-syntax all
  (syntax-rules ()
    [(all <expr>)
     (let* ([all-results '()]
            [all-helper 
             (λ(e)
               (set! all-results (cons e all-results))
               (next)
               (reverse all-results))])
       (all-helper <expr>))]))

#|
(clear)

  Remove all choice points from stack. Used for testing purposes.
|#
(define (clear)
  (set! choices '()))

(define (?- pred expr)
  (if (pred expr)
      expr
      ; If the predicate fails, try the next choice.
      (next)))


;------------------------------------------------------------------------------
; Private values for managing the stack of choices.
;------------------------------------------------------------------------------

; The stack of choice points, represented as a list.
(define choices '())

; The stack of peeks that stores the most recent choice point
(define peeks '())

; "Push": add a choice to the choices stack.
(define (add-choice! choice)
  (set! choices
        (cons choice choices)))

; "Push": add a quoted-choice to the peeks stack
(define (quoted-push! choice)
  (set! peeks
        (cons choice peeks)))

; "Pop": remove and return first choice from
; the choices stack.
(define (get-choice!)
  (let ([choice (first choices)])
    (set! choices (rest choices))
    (set! peeks (rest peeks)) ; pop the peeks and set it to remainder peeks
    choice))
