#lang racket/base

(provide where)

(require syntax/parse/define
         hackett/private/type-language
         (for-syntax racket/base
                     "rep/sig.rkt"))

(define-syntax-parser where
  #:datum-literals [=]
  [(_ base:sig type-id:id = {~type τ:type})
   (define sym (syntax-e #'type-id))
   (sig-where #'base.expansion sym #'τ.expansion)])

