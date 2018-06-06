#lang racket/base

(require racket/contract/base)
(provide
 (contract-out
  [prop:reintroducible-dot-type
   (struct-type-property/c
    (-> any/c reintroducible-dot-type?))]
  [reintroducible-dot-type?
   (-> any/c boolean?)]
  [reintroducible-dot-type-module
   (-> reintroducible-dot-type?
       identifier?)]
  [reintroducible-dot-type-external-sym
   (-> reintroducible-dot-type?
       symbol?)]

  [attach-reintroducible-dot-type
   (-> identifier?
       reintroducible-dot-type?
       identifier?)])
 reintroducible-dot-type
 reintroducible-dot-type-id ; syntax-class
 )

(require racket/match
         syntax/parse
         struct-like-struct-type-property)

(define stxprop:reintroducible-dot-type
  (gensym 'reintroducible-dot-type))

(define (attach-reintroducible-dot-type stx rdt)
  (syntax-property stx stxprop:reintroducible-dot-type rdt))

(define-struct-like-struct-type-property reintroducible-dot-type
  [module external-sym])

(define-syntax-class (reintroducible-dot-type-id [ctx #f])
  #:attributes [local-value module-id external-sym]
  [pattern x:id
           #:attr local-value (syntax-local-value #'x (λ () #f) ctx)
           #:when (reintroducible-dot-type? (attribute local-value))
           #:do [(match-define (reintroducible-dot-type m s)
                   (attribute local-value))]
           #:with module-id m
           #:with external-sym s]
  [pattern x:id
           #:attr local-value
           (syntax-property #'x stxprop:reintroducible-dot-type)
           #:when (reintroducible-dot-type? (attribute local-value))
           #:do [(match-define (reintroducible-dot-type m s)
                   (attribute local-value))]
           #:with module-id m
           #:with external-sym s])
