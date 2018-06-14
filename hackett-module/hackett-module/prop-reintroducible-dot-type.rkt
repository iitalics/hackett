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
       symbol?)])
 reintroducible-dot-type
 reintroducible-dot-type-id ; syntax-class
 )

(require racket/function
         racket/match
         syntax/parse
         syntax/parse/class/local-value
         (for-syntax racket/base
                     syntax/transformer))

(define-values
  [prop:reintroducible-dot-type
   reintroducible-dot-type?
   reintroducible-dot-type-ref]
  (make-struct-type-property
   'reintroducible-dot-type))

(struct reintroducible-dot-type-struct
  [module external-sym]
  #:property prop:reintroducible-dot-type identity)

(define (normalize-reintroducible-dot-type s)
  (cond
    [(reintroducible-dot-type-struct? s) s]
    [(reintroducible-dot-type? s)
     (normalize-reintroducible-dot-type
      ((reintroducible-dot-type-ref s) s))]))

(define (reintroducible-dot-type-module s)
  (reintroducible-dot-type-struct-module
   (normalize-reintroducible-dot-type s)))

(define (reintroducible-dot-type-external-sym s)
  (reintroducible-dot-type-struct-external-sym
   (normalize-reintroducible-dot-type s)))

(define-match-expander reintroducible-dot-type
  (syntax-rules ()
    [(_ pat-m pat-sym)
     (? reintroducible-dot-type?
        (app normalize-reintroducible-dot-type
             (reintroducible-dot-type-struct
              pat-m
              pat-sym)))])
  (make-variable-like-transformer #'reintroducible-dot-type-struct))

(define-syntax-class reintroducible-dot-type-id
  #:attributes [local-value module-id external-sym]
  [pattern {~var || (local-value reintroducible-dot-type?)}
           #:do [(match-define (reintroducible-dot-type m x)
                   (attribute local-value))]
           #:with module-id m
           #:with external-sym x])
