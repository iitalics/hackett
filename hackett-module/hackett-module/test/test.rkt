#lang racket/base

(require syntax/parse/define
         syntax/macro-testing
         "../sig.rkt"
         "../rep/sig.rkt"
         (for-syntax racket/base
                     rackunit
                     rackunit/text-ui
                     syntax/parse))

(define-signature S
  (sig
   (type X)
   (val x :   X)
   (val y : X)))

(define-for-syntax tests
  (test-suite "tests"
    (test-case "S"
      (define S
        (syntax-parse
            #'(sig
               (type X)
               (val x : X))
          [S:sig (attribute S.expansion)]))
      (println S))
    (check-equal? (internal-definition-context-binding-identifiers
                   (syntax-local-make-definition-context))
                  '())
    ))

(module+ test
  (convert-compile-time-error
   (let-syntax
       ([m
         (λ (stx)
           #`'#,(run-tests tests))])
     (m))))
