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
             "check/module-var.rkt"
             (for-syntax racket/base
                         racket/syntax
                         syntax/parse)))

(begin-for-syntax
  (define disappeared-use 'disappeared-use)

  ;; puts it in the whichever namespace modules are in,
  ;; which for now is the value namespace
  (define-syntax ~module
    (pattern-expander
     (syntax-parser
       [(_ pat)
        #:with tmp (generate-temporary 'module-ns-tmp)
        #'{~and tmp {~parse pat (value-namespace-introduce #'tmp)}}]))))

(define-syntax-parser #%dot_e
  #:literal-sets [sig-literals]

  [(_ {~module m:module-binding} ~! x:id)
   #:with m- #'m.internal-id
   #:do [(define decl
           (hash-ref (sig-decls #'m.sig)
                     (syntax-e #'x)
                     #f))]
   #:fail-when (and (not (decl-val? decl)) #'x)
   (format "not bound to a value in module ~a" (syntax-e #'m))
   #:with (#%val-decl t) decl
   #:with {~var t_qual (type (@ m.expansion-ctx))} #'t
   (syntax-property
    (attach-type #'(#%app hash-ref m- 'x)
                 #'t_qual.expansion)
    disappeared-use
    (syntax-local-introduce #'m))])

(define-syntax-parser #%dot_τ
  [(_ {~module m:module-binding} ~! x:id)
   #:do [(define sym (syntax-e #'x))
         (define internal-id
           (hash-ref (sig-internal-ids #'m.sig) sym #f))
         (define decl
           (hash-ref (sig-decls #'m.sig) sym #f))]
   #:fail-when (and (not (decl-type? decl)) #'x)
   (format "not bound to a type in module ~a" (syntax-e #'m))
   #:with {~var t_qual (type (@ m.expansion-ctx))} internal-id
   (syntax-property
    #'t_qual.expansion
    disappeared-use
    (syntax-local-introduce #'m))])
