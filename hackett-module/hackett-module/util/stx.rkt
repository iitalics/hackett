#lang racket/base

(provide hash-literal
         (rename-out [hash-literal hash-lit]))

(require syntax/parse)

(define-syntax-class hash-literal
  [pattern stx
    #:attr value (syntax-e #'stx)
    #:when (hash? (attribute value))
    #:attr keys (hash-keys (attribute value))
    #:attr values (hash-values (attribute value))])

