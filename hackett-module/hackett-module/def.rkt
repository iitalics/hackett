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
             racket/list
             syntax/parse
             hackett/private/util/stx
             "check/sig-matches.rkt"
             "check/expand-check.rkt"))

(provide
 def-module
 λₑ
 λₘ
 appₘ)

(define-syntax-parser def-module
  [(_ name:id m:expr)
   #:with [m- sig] (sig⇒ #'m)
   #:with name- (generate-temporary #'name)

   #:with [[id transformer] ...]
   (generate-module-var-bindings #'name #'name- #'sig)

   #'(begin
       (printf "\n---------\nbinding: ~a\n" 'name)
       (printf "inferred: ")
       (pretty-write 'sig)
       (define name- m-)
       (define-syntaxes [id ...] (values transformer ...))
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
   #:do [(define bindings
           (generate-module-var-bindings
            #'x
            #'x-
            #'A.expansion))
         (define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (list #'x-) #f ctx)
         (syntax-local-bind-syntaxes
          (map first bindings)
          #`(values #,@(map second bindings))
          ctx)]

   #:with x-- (internal-definition-context-introduce ctx #'x-)

   #:with [body- B] (sig⇒ #'body ctx)
   ;; #:with B* (reintroduce-#%dot #'x #'B ctx)
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
