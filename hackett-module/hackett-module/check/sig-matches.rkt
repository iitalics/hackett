#lang racket/base

(provide signature-matches?
         sig-matches?)

(require (for-template "../rep/sig-literals.rkt")
         "module-var.rkt"
         "../rep/sig.rkt"
         "../util/stx.rkt"
         racket/syntax
         syntax/parse
         syntax/id-set
         (only-in syntax/parse [attribute @])
         threading
         hackett/private/typecheck
         hackett/private/util/stx
         )

;; A Signature is one of:
;;  - Sig
;;  - PiSig

;; ---------------------------------------------------------
;; signature-matches?

;; Signature Signature -> Boolean
;; the signatures should be already expanded.
(define (signature-matches? A B)
  ;; TODO: call either sig-matches or pi-sig-matches depending
  (syntax-parse (list A B)
    #:literal-sets [sig-literals]
    [[(#%sig . _) (#%sig . _)]
     (sig-matches? A B)]
    [[(#%pi-sig . _) (#%pi-sig . _)]
     (pi-sig-matches? A B)]
    [[_ _]
     #f]))

;; ---------------------------------------------------------
;; pi-sig-matches?

;; PiSig PiSig -> Boolean
;; the signatures A and B should already be expanded
(define (pi-sig-matches? A B)
  (define/syntax-parse (_ ([A-x A-in]) A-out) A)
  (define/syntax-parse (_ ([B-x B-in]) B-out) B)
  (and
   ;; the "in" is contravariant
   (signature-matches? #'B-in #'A-in)
   (let ()
     (define/syntax-parse x- (generate-temporary #'B-x))

     ;; Create a context where both `A-x` and `B-x` are bound
     ;; to the same module with the signature `B-in`.
     ;; It's `B-in` because it has the most specific entries
     ;; that both signatures need to be compatible with.
     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (list #'x-) #f ctx)
     (syntax-local-bind-syntaxes
      (list #'A-x)
      #'(make-rename-transformer (quote-syntax B-x))
      ctx)
     (syntax-local-bind-module #'B-x #'x- #'B-in ctx)

     (define A-out* (expand-sig #'A-out ctx))
     (define B-out* (expand-sig #'B-out ctx))

     (signature-matches? A-out* B-out*))))

;; ---------------------------------------------------------
;; sig-matches?

;; Sig Sig -> Boolean
;; the signatures should be already expanded.
(define (sig-matches? A B)
  (define/syntax-parse (_ A-internal-ids:hash-lit A-decls:hash-lit) A)
  (define/syntax-parse (_ B-internal-ids:hash-lit B-decls:hash-lit) B)

  ;; A Common is an Identifier to be bound in the extended common
  ;; environment that will be used for checking the components against
  ;; each other

  ;; sym->common : [Hashof Sym Common]
  (define sym->common
    (let*
        ([acc (hash)]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ A-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-temporary k)))]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ B-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-temporary k)))])
      acc))

  ;; TODO: make an environment that maps the Common ids to
  ;;       transformers that act like their components from A.

  (define common-ctx
    (syntax-local-make-definition-context))
  (define (intro stx)
    (internal-definition-context-introduce common-ctx stx))

  (define A*
    (intro
     (signature-substs
      A
      (for/free-id-table ([(sym id) (in-hash (@ A-internal-ids.value))])
        (values id (hash-ref sym->common sym))))))

  (define B*
    (intro
     (signature-substs
      B
      (for/free-id-table ([(sym id) (in-hash (@ B-internal-ids.value))])
        (values id (hash-ref sym->common sym))))))

  (for ([(sym decl) (in-hash (sig-decls A*))])
    (syntax-local-bind-syntaxes
     (list (hash-ref sym->common sym))
     (decl->transformer-stx decl)
     common-ctx))

  ;; check that all components in B correspond with
  ;; components in A
  (for/and ([(sym B-decl) (in-hash (sig-decls B*))])
    (define A-decl (hash-ref (sig-decls A*) sym #f))
    (and A-decl
         (let ()
           (define A-decl* (expand-decl A-decl common-ctx))
           (define B-decl* (expand-decl B-decl common-ctx))
           (sig-entry-matches?
            A-decl*
            B-decl*)))))

;; Decl -> TransformerStx or #f
(define (decl->transformer-stx decl)
  (syntax-parse decl
    #:literal-sets [sig-literals]
    [(#%val-decl _) #f]
    [(#%constructor-decl _) #f]
    [(#%type-decl (#%opaque)) #f]
    [(#%type-decl (#%data c ...)) #f]
    [(#%type-decl (#%alias t)) #'(make-variable-like-transformer (quote-syntax t))]))

;; Decl Decl -> Bool
(define (sig-entry-matches? A B)
  (syntax-parse (list A B)
    #:literal-sets [sig-literals]
    [[(#%val-decl A) (#%val-decl B)]
     (type-matches? #'A #'B)]
    [[(#%constructor-decl A) (#%val-decl B)]
     (type-matches? #'A #'B)]
    [[(#%constructor-decl A) (#%constructor-decl B)]
     (type-equal? #'A #'B)]

    [[(#%type-decl _) (#%type-decl (#%opaque))]
     #true]
    [[(#%type-decl (#%alias A)) (#%type-decl (#%alias B))]
     (type-equal? #'A #'B)]
    [[(#%type-decl (#%data c-A ...)) (#%type-decl (#%data c-B ...))]
     ; use set comparison because order doesn't matter
     (free-id-set=?
      (immutable-free-id-set (@ c-A))
      (immutable-free-id-set (@ c-B)))]

    [[_ _]
     #false]))

;; Type Type -> Bool
(define (type-equal? A B)
  (and (type-matches? A B)
       (type-matches? B A)))

;; Type Type -> Bool
(define (type-matches? A B)
  (define (exn-type-mismatch? v)
    (and (exn:fail:syntax? v)
         (regexp-match? #rx"typechecker: type mismatch"
                        (exn-message v))))
  (with-handlers ([exn-type-mismatch? (λ (e) #f)])
    ;; TODO: is this the right function to call?
    (type<:! A B #:src (datum->syntax #f #f))))

;; ---------------------------------------------------------

;; make-rename-context : IntDefCtx [Listof [List Id Id]] -> IntDefCtx
(define (make-rename-context parent renamings)
  (define/syntax-parse [[A B] ...] renamings)
  (define ctx (syntax-local-make-definition-context parent))
  (syntax-local-bind-syntaxes
   (@ A)
   ;; create a syntax object that produces multiple values
   #'(values (make-rename-transformer #'B) ...)
   ctx)
  ctx)
