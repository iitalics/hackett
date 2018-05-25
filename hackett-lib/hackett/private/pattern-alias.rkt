#lang racket/base

(provide pattern list)

(require syntax/parse/define
         (only-in hackett/private/adt defn)
         (only-in hackett/private/prim/type Nil ::)
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     hackett/private/prop-case-pattern-expander
                     hackett/private/util/stx))

(begin-for-syntax
  (struct proc+case-pat-exp [proc case-pat-trans]
    #:property prop:procedure (struct-field-index proc)
    #:property prop:case-pattern-expander
    (λ (self) (proc+case-pat-exp-case-pat-trans self))))

(define-simple-macro
  (pattern (head:id arg:id ...) old:expr)
  #:with head-internal (generate-temporary #'head)
  (begin
    (defn head-internal [[arg ...] old])
    (define-syntax head
      (proc+case-pat-exp
       (make-variable-like-transformer (quote-syntax head-internal))
       (syntax-parser #:disable-colon-notation
         [({~var head} {~var arg} ...) #'old])))))

(define-syntax list
  (let ([trans
         (syntax-parser
           [(list)          #'Nil]
           [(list a bs ...) #'(:: a (list bs ...))])])
    (proc+case-pat-exp trans trans)))
