#lang racket/base

(require syntax/parse/define
         syntax/macro-testing
         (only-in hackett/private/base Integer)
         "../sig.rkt"
         "../rep/sig.rkt"
         (for-syntax racket/base
                     rackunit
                     rackunit/text-ui
                     syntax/parse
                     "../check/sig-matches.rkt"
                     ))

(define-signature S
  (sig
   (type X)
   (val x :   X)
   (val y : X)))

(define-for-syntax tests
  (test-suite "tests"
    (test-case "S"
      (define S
        (expand-sig
         #'(sig
            (type X)
            (val x : X))))

      (check sig-matches?
             S
             (expand-sig
              #'(sig
                 (type X)
                 (val x : X))))

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type X = Integer)
                 (val x : X)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type Y)
                 (type X = X)
                 (val x : X)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type X = Integer)
                 (val x : Integer)))
             S)

      (check sig-matches?
             (expand-sig
              #'(sig
                 (type Y)
                 (type X = Y)
                 (val x : Y)))
             S))
    ))

(module+ test
  (convert-compile-time-error
   (let-syntax
       ([m
         (λ (stx)
           #`'#,(run-tests tests))])
     (m))))
