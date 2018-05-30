#lang hackett

(require (only-in racket/base
           define-syntax for-syntax begin-for-syntax)
         (for-syntax racket/base
                     syntax/parse
                     (only-in hackett/private/prop-case-pattern-expander
                       case-pattern-expander)))

(begin-for-syntax
  (struct group [hash])
  (define (group-ref g x)
    (hash-ref (group-hash g) x)))

(define-syntax group-ref
  (case-pattern-expander
   (syntax-parser
     [(_ {~var G (static group? "group")} x)
      (group-ref (attribute G.value) (syntax-e #'x))])))

(data Result
  (Success Integer)
  (Failure String))

(define-syntax G (group (hash 'good #'Success 'bad #'Failure)))

(main
 (case (Success 5)
   [((group-ref G good) x) (do (pure (Just x)))]
   [((group-ref G bad) y) (do (print y) (pure Nothing))]))

