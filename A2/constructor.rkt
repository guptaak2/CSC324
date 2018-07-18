#lang racket
; Assignment 2 - Classes

; QUESTION 3 (my-class).

(define-syntax my-class
  (syntax-rules ()
    [(my-class <Class>
               (<args> ...)
               [(<attr> <value>) ...]
               [((<method> <param> ...) <value2>)...]
               [(<var> <value3>) ...])
     (define (<Class> <args> ...)
       (let ([<var> <value3>]...)
         (let ([<attr> <value>]...)
           (λ (msg)
             (cond [(equal? msg (id->string <attr>)) <value>]
                   ...
                   [(equal? msg (id->string <method>))
                    (λ (<param> ...) <value2>)]
                   ...
                   [else "Unrecognized message!"])))))]))

; Creating my-class
; (my-class My-Class (x y z) 
;          ([a x] 
;           [b (* y z)]) 
;          ([(f r) (+ r x)]))


; Instantiating my-class
; define instance_my_class (My-Class 32 42 42)

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))


(define (f r) (+ r 5))

; My-Class constructor takes a and b as arguments so my version
; of my class has (<args> ...)
; My-Class constructor takes attributes and their values of the class
; so my version of my class has [(<attr> <value> ...]
; My-Class constructor takes methods of the class as represented by
; [((<method> <param> ...) <value2>) ...]
; My-Class constructor takes local name bindings such as r = f(a)
; so my version of my class has [(<var> <value3>) ...]

; This is the Racket version of the python code in the Assignment
; x = f(a), y = '(b 100 r), z = "you are cool", r = f (a)
; since it does not have any methods, the 3rd field is []

(my-class My-Class (a b)
          [(x (f a))
           (y (list b 100 r))
           (z "you are cool")]
          []
          [(r (f a))])