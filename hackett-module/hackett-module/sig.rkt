#lang racket/base

(provide sig
         Π
         val
         type
         data
         def-signature)

(require syntax/parse/define
         "rep/sig-literals.rkt"
         hackett/private/type-language
         (only-in hackett/base type data)
         (for-syntax racket/base
                     syntax/parse
                     hackett/private/util/stx
                     "rep/sig.rkt"
                     ))

(define-syntax val #f)

(begin-for-syntax
  (define-literal-set sig-surface-literals
    #:datum-literals [: =]
    [val type data])

  (define-syntax-class sig-entry
    #:attributes [[id 1] [decl 1]]
    #:literal-sets [sig-surface-literals]
    [pattern (val x:id : {~type val-type:expr})
      #:with [id ...]   #'[x]
      #:with [decl ...] #'[(#%val-decl val-type)]]
    [pattern (data X:id c:id ...)
      #:with [[id decl] ...] #`[[X (#%type-decl (#%alias (#%type:con X)))]
                                [c #,(syntax/loc this-syntax
                                       (#%constructor-decl X))]
                                ...]]
    [pattern (type {~type X:id})
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%opaque))]]
    [pattern (type {~type X:id} = {~type alias-type:expr})
      #:with [id ...]   #'[X]
      #:with [decl ...] #'[(#%type-decl (#%alias alias-type))]])

  (define-syntax-class sig-entries
    #:attributes [[id 1] [decl 1]]
    [pattern [entry:sig-entry ...]
      #:with [id ...] #'[entry.id ... ...]
      #:with [decl ...] #'[entry.decl ... ...]])
  )

(define-syntax-parser sig
  [(_ ent ...)
   #:with entries:sig-entries ((make-syntax-introducer #t) #'[ent ...])
   #`(#%sig
      #,(for/hash ([id (in-list (attribute entries.id))])
          (values (syntax-e id) id))
      #,(for/hash ([id (in-list (attribute entries.id))]
                   [decl (in-list (attribute entries.decl))])
          (values (syntax-e id) decl)))])

(define-syntax-parser Π
  #:datum-literals [:]
  [(_ ([x : A:expr]) B:expr)
   #'(#%pi-sig ([x A]) B)])

(define-simple-macro
  (def-signature name sig-expr:sig)
  (begin
    (define-values [] sig-expr.residual)
    (define-syntax name
      (make-variable-like-transformer
       #'sig-expr.expansion))))
