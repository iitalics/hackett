#lang racket/base
(provide
 #%dot_τ
 #%dot_e
 (for-syntax reintroduce-#%dot))

(require
 syntax/parse/define
 hackett/private/type-language
 "rep/sig-literals.rkt"
 (prefix-in l: "link/mod.rkt")
 (for-syntax racket/base
             racket/match
             racket/syntax
             syntax/parse
             (only-in syntax/parse [attribute @])
             (only-in hackett/private/typecheck attach-type)
             (only-in hackett/private/prop-case-pattern-expander
                      prop:case-pattern-expander)
             hackett/private/util/stx
             "check/module-var.rkt"
             "util/stx-traverse.rkt"
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
        #'{~and tmp {~parse pat (value-namespace-introduce #'tmp)}}])))

  (struct proc+case-pat-exp [proc case-pat-trans]
    #:property prop:procedure (struct-field-index proc)
    #:property prop:case-pattern-expander
    (λ (self) (proc+case-pat-exp-case-pat-trans self)))
  )

(define-syntax #%dot_e
  (proc+case-pat-exp
   ;; as a normal macro
   (syntax-parser
     #:literal-sets [sig-literals]

     [(_ {~module m:module-binding} ~! x:id)
      #:with m- #'m.internal-id
      #:do [(define decl
              (hash-ref (sig-decls #'m.sig)
                        (syntax-e #'x)
                #f))]
      #:fail-when (and (not (or (decl-val? decl)
                                (decl-constructor? decl)))
                       #'x)
      (format "not bound to a value in module ~a" (syntax-e #'m))
      #:with (_ t) decl
      #:with {~var t_qual (type (@ m.expansion-ctx))} #'t
      (syntax-property
       (attach-type #'(#%app l:mod-value-ref m- 'x)
                    #'t_qual.expansion)
       disappeared-use
       (syntax-local-introduce #'m))])

   ;; as a case-pattern expander
   (syntax-parser
     #:literal-sets [sig-literals]
     [(_ {~module m:module-binding} ~! x:id)
      #:do [(define decl
              (hash-ref (sig-decls #'m.sig)
                        (syntax-e #'x)
                #f))]
      #:fail-when (and (not (decl-constructor? decl)) #'x)
      (format "not bound to a constructor in module ~a" (syntax-e #'m))

      #:with ctor-id (hash-ref (@ m.constructor-ids) (syntax-e #'x))
      #'ctor-id])))

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

;; -------------------------------------------

(begin-for-syntax
  ;; the ctx contains a module-binding for m-dots-are-from-id
  ;; ASSUME s-to-reintro is already expanded
  (define (reintroduce-#%dot m-dots-are-from-id m-to-prefix-id s-to-reintro ctx)
    (define m-internal-id
      (module-var-transformer-internal-id
       (syntax-local-value m-dots-are-from-id #f ctx)))

    ;; determine which opaque cons to substitute by comparing
    ;; their mod internal id's with the prefix, to see if we should
    ;; insert prefixes for them.

    (define (traverse stx)
      (syntax-parse stx
        #:literals [#%type:con]
        [(#%type:con x:id)
         (define x-value (syntax-local-value #'x (λ () #f) ctx))
         (match x-value
           [(opaque-type-constructor mod-id external-sym)
            #:when (free-identifier=? mod-id m-internal-id)
            ;; and if it is, actually do the reintroducing
            (quasisyntax/loc stx
              (#%dot_τ #,m-to-prefix-id #,external-sym))]
           [_
            stx])]
        [_
         (traverse-stx/recur stx traverse)]))

    (traverse s-to-reintro)))
