#lang racket/base
(require
 "rep/sig-literals.rkt"
 "dot.rkt"
 "namespace/reqprov.rkt"
 racket/pretty
 syntax/parse/define
 (prefix-in hkt: hackett/base)
 (prefix-in sig: "sig.rkt")
 (for-syntax racket/base
             racket/syntax
             racket/list
             syntax/parse
             syntax/intdef
             hackett/private/util/stx
             "rep/sig.rkt"
             "rep/sig-pretty.rkt"
             "check/expand-check.rkt"
             "check/module-var.rkt"
             "namespace/namespace.rkt"))

(provide
 def-module
 (module-out seal
             (rename-out
              [λₘ λ]
              [appₘ #%app])))

(define-syntax-parser def-module
  [(_ {~module name:id} {~module m:expr})
   #:with [m- sig] (sig⇒ #'m)
   #:with sig-str (sig->string #'sig 10)

   #:with name- (generate-temporary #'name)
   #:with [([stx-id transformer] ...)
           ([val-id expr] ...)]
   (generate-module-var-bindings #'name #'name- #'sig)

   #'(begin
       (printf "\n---------\nbinding: ~a\n" 'name)
       (printf "inferred: ~a\n" 'sig-str)
       (define name- m-)
       (define-syntaxes [stx-id ...] (values transformer ...))
       (define-values [val-id ...] (values expr ...))
       (printf "module body: ")
       (pretty-write name))])

(define-syntax-parser seal
  #:datum-literals [:>]
  [(_ m:expr :> {~signature s:sig})
   #:with m- (sig⇐ #'m #'s.expansion)
   (attach-sig #'(let-values ([() s.residual])
                   m-)
               #'s.expansion)])

(define-syntax-parser λₘ
  #:datum-literals [:]
  [(_ ([x:id : {~signature A:sig}]) body:expr)
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
   ;;   note: (#%dot x T) will expand into (#%type:con T-)
   ;;
   ;;   - expand functor body with this compile time binding
   ;;     for 'x'. additionally, each T- will need to be bound
   ;;     as values (so that #%type:con is valid, free-id= is not broken etc.).
   ;;
   ;;   - traverse the types in the output signature B, replacing
   ;;     (#%type:con T-) with (#%dot x T). do NOT re-expand B after
   ;;     substituting.
   ;;
   ;; #%pi-sig will have to apply these same steps during expansion.

   ;; create a context where x is bound
   #:do [(define ctx (syntax-local-make-definition-context))
         (define (intro stx)
           (internal-definition-context-introduce ctx stx))

         (syntax-local-bind-syntaxes (list #'x-) #f ctx)
         (define val-ids/exprs
           (syntax-local-bind-module #'x #'x- #'A.expansion ctx))]

   #:with x-- (intro #'x-)
   #:with [body- B] (sig⇒ #'body ctx)
   #:with B* (reintroduce-#%dot (intro #'x) #'x-- #'B ctx)
   #:with [[val-id val-expr] ...] val-ids/exprs
   (internal-definition-context-track
    ctx
    (attach-sig #'(λ (x--)
                    (define-values [val-id ...]
                      (values val-expr ...))
                    body-)
                #'(#%pi-sig ([x-- A.expansion]) B*)))])

(define-syntax-parser appₘ
  #:literals [#%pi-sig]
  [(_ fun:expr arg:id)
   ;; TODO: allow module paths for `a`, or module expressions if possible
   #:with [fun- (#%pi-sig ([x A]) B)] (sig⇒ #'fun)
   #:with arg- (sig⇐ #'arg #'A)
   #:with B*:sig (signature-subst #'B #'x #'arg)
   (attach-sig #'(#%app fun- arg-) #'B*.expansion)])
