#lang racket/base
(require
 "mod.rkt"
 "rep/sig.rkt"
 racket/pretty
 syntax/parse/define
 hackett/private/type-language
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             racket/syntax
             syntax/parse
             hackett/private/util/stx))

(provide
 def-module
 λₑ
 λₘ)

(begin-for-syntax
  (define (attach-sig stx s)
    (syntax-property stx 'sig: s))

  (define (sig⇒ stx [ctx #f])
    (define m-
      (local-expand stx 'module-begin '() ctx))
    (define sig
      (syntax-local-introduce
       (syntax-property m- 'sig:)))
    (list m- sig))

  ;; Id Sig -> Transformer
  (define (make-module-var-transformer x s)
    (make-variable-like-transformer
     ; Adjust source location information before calling attach-type so that tooltips end up in the
     ; right place.
     (λ (stx) (attach-sig (replace-stx-loc x stx) s))))

  )

(define-syntax-parser def-module
  [(_ name:id m:expr)
   #:with [m- sig] (sig⇒ #'m)
   #:with name- (generate-temporary #'name)
   #'(begin
       (printf "inferred: ")
       (pretty-write 'sig)
       (define-syntax name
         (make-module-var-transformer (quote-syntax name-) (quote-syntax sig)))
       (define name- m-)
       (printf "\nmodule body: ")
       (pretty-write name))])

(define-syntax λₑ (make-rename-transformer #'hkt:λ))

(define-syntax-parser λₘ
  #:datum-literals [:]
  [(_ ([x:id : A:sig]) body:expr)
   #:with x- (generate-temporary #'x)

   ;; create a context where x is bound
   #:do [(define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes
          (list #'x-)
          #f
          ctx)
         (syntax-local-bind-syntaxes
          (list #'x)
          #'(make-module-var-transformer (quote-syntax x-)
                                         (quote-syntax A.expansion))
          ctx)]
   #:with x-- (internal-definition-context-introduce ctx #'x-)

   #:with [body- B] (sig⇒ #'body ctx)
   (attach-sig #'(λ (x--) body-) #'(#%pi-sig ([x-- A.expansion]) B))])

