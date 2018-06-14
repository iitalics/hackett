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
             hackett/private/util/stx
             "check/sig-matches.rkt"))

(provide
 def-module
 λₑ
 λₘ
 appₘ)

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
       (printf "\n---------\nbinding: ~a\n" 'name)
       (printf "inferred: ")
       (pretty-write 'sig)
       (define-syntax name
         (make-module-var-transformer (quote-syntax name-) (quote-syntax sig)))
       (define name- m-)
       (printf "module body: ")
       (pretty-write name))])

(define-syntax λₑ (make-rename-transformer #'hkt:λ))

(define-syntax-parser λₘ
  #:datum-literals [:]
  [(_ ([x:id : A:sig]) body:expr)
   #:with x- (generate-temporary #'x)

   ;; ===========
   ;; TODO:
   ;;   - for every opaque type T in A, generate a
   ;;     temporary T- for it, and create a mapping from
   ;;     the type symbol names to those temporaries.
   ;;
   ;;   - we can make a compile time binding for 'x' that
   ;;     includes:
   ;;       internal id  (x-)
   ;;       signature    (A.expansion)
   ;;       type mapping (T => T-)
   ;;
   ;;   - expand functor body with this compile time binding
   ;;     for 'x'. additionally, each T- will need to be bound
   ;;     as values (so they won't expand).
   ;;
   ;;   - figure out: how to substitute T- back into (#%dot M T)
   ;;     during e.g. application.

   ;; create a context where x is bound
   #:do [(define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (list #'x-)
                                     #f
                                     ctx)
         (syntax-local-bind-syntaxes (list #'x)
                                     #'(make-module-var-transformer
                                        (quote-syntax x-)
                                        (quote-syntax A.expansion))
                                     ctx)]
   #:with x-- (internal-definition-context-introduce ctx #'x-)

   #:with [body- B] (sig⇒ #'body ctx)
   (attach-sig #'(λ (x--) body-) #'(#%pi-sig ([x-- A.expansion]) B))])

(define-syntax-parser appₘ
  #:literals [#%pi-sig]
  [(_ f:expr a:id)
   ;; TODO: allow module paths for `a`, or module expressions if possible

   #:with [f- (#%pi-sig ([x A]) B)] (sig⇒ #'f)
   #:with [a- A*] (sig⇒ #'a)

   #:do [(unless (signature-matches? #'A* #'A)
           (raise-syntax-error #f
             (format "signature mismatch\n  expected: ~a\n  given:    ~a"
                     (sig->string #'A) (sig->string #'A*))
             #'a))]
   #:with B* (signature-substs #'B #'([x a]))
   (attach-sig #'(#%app f- a-)
               #'B*)])
