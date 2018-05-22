#lang racket/base
(provide
 #%dot_τ
 #%dot_e)

(require
 syntax/parse/define
 hackett/private/type-language
 "rep/sig.rkt"
 (for-syntax racket/base
             racket/syntax
             syntax/parse
             (only-in syntax/parse [attribute @])
             hackett/private/typecheck
             hackett/private/util/stx
             "check/module-var.rkt"))

(define-syntax-parser #%dot_e
  #:literal-sets [sig-literals]

  [(_ m:module-binding ~! x:id)
   #:with m- #'m.internal-id
   #:do [(define decl
           (hash-ref (sig-decls #'m.sig)
                     (syntax-e #'x)
                     #f))]
   #:fail-when (and (not (decl-val? decl)) #'x)
   (format "not bound to a value in module ~a" (syntax-e #'m))
   #:with (#%val-decl t) decl
   #:with {~var t_qual (type (@ m.expansion-ctx))} #'t
   (attach-type #'(#%app hash-ref m- 'x)
                #'t_qual.expansion)])

(define-syntax-parser #%dot_τ
  [(_ m x)
   (raise-syntax-error #f "#%dot for types" this-syntax)])
