#lang racket/base

(provide sig
         val
         type
         define-signature)

(require syntax/parse/define
         "rep/sig.rkt"
         (for-syntax racket/base
                     syntax/parse
                     hackett/private/util/stx
                     ))

(define-syntax val #f)
(define-syntax type #f)

(begin-for-syntax
  (define-syntax-class sig-entry
    #:attributes [id norm]
    #:literals [val type]
    #:datum-literals [: =]
    [pattern (val id:id : val-type:expr)
      #:with norm #'(#%val-decl val-type)]
    [pattern (type id:id)
      #:with norm #'(#%type-decl (#%opaque))]
    [pattern (type id:id = alias-type:expr)
      #:with norm #'(#%type-decl (#%alias alias-type))]))

(define-syntax-parser sig
  [(_ entry:sig-entry ...)
   #`(#%sig
      #,(for/hash ([id (in-list (attribute entry.id))])
          (values (syntax-e id) id))
      #,(for/hash ([id (in-list (attribute entry.id))]
                   [norm (in-list (attribute entry.norm))])
          (values (syntax-e id) norm)))])

(define-simple-macro
  (define-signature name sig-expr:sig)
  (begin
    (define-values [] sig-expr.residual)
    (define-syntax name
      (make-variable-like-transformer
       #'sig-expr.expansion))))

