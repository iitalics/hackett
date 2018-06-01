#lang hackett-module
(require (only-in racket/base
                  begin-for-syntax
                  for-syntax))
(require (for-syntax racket/base
                     "../namespace/namespace.rkt"
                     "../rep/sig-pretty.rkt"))

(def-signature S
  (sig
    (module Sub :
      (sig
        (type T)))

    (type R = Sub.T)
    ))

(begin-for-syntax
  (define S* (signature-namespace-introduce #'S))
  (displayln
   (sig->string
    ((syntax-local-value S*) S*))))
