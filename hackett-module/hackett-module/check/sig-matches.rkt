#lang racket/base

(provide sig-matches?)

(require (for-template "../rep/sig.rkt")
         racket/syntax
         syntax/parse
         (only-in syntax/parse [attribute @])
         threading
         hackett/private/typecheck
         "../util/stx.rkt"
         )

;; Sig Sig -> Boolean
(define (sig-matches? A* B*)
  (define/syntax-parse A:sig A*)
  (define/syntax-parse B:sig B*)
  (define/syntax-parse (_ A-internal-ids:hash-lit A-decls:hash-lit)
    #'A.expansion)
  (define/syntax-parse (_ B-internal-ids:hash-lit B-decls:hash-lit)
    #'B.expansion)

  ;; A Common is an Identifier to be bound in the extended common
  ;; environment that will be used for checking the components against
  ;; each other

  ;; sym->common : [Hashof Sym Common]
  ;; common-intdef-ctx : InternalDefinitionContext
  (define-values [sym->common common-intdef-ctx]
    (let*
        ([ctx (syntax-local-make-definition-context)]
         [generate-bound-temporary
          (λ (x)
            (define y (generate-temporary x))
            (syntax-local-bind-syntaxes (list y) #f ctx)
            (internal-definition-context-introduce ctx y))]
         [acc (hash)]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ A-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-bound-temporary k)))]
         [acc
          (for/fold ([acc acc])
                    ([k (in-hash-keys (@ B-internal-ids.value))]
                     #:when (not (hash-has-key? acc k)))
            (hash-set acc k (generate-bound-temporary k)))])
      (values acc ctx)))

  ;; create an internal definition context mapping A ids to Common ids
  (define A->common
    (make-rename-context
     common-intdef-ctx
     (for/list ([(sym A-id) (in-hash (@ A-internal-ids.value))])
       (list A-id (hash-ref sym->common sym)))))

  ;; create an internal definition context mapping A ids to Common ids
  (define B->common
    (make-rename-context
     common-intdef-ctx
     (for/list ([(sym B-id) (in-hash (@ B-internal-ids.value))])
       (list B-id (hash-ref sym->common sym)))))

  ;; IntDefCtx [Hashof Sym Stx] -> [Hashof Sym Stx]
  (define (rename-decl-hash rnm-ctx decl-hash)
    (for/hash ([(k v) (in-hash decl-hash)])
      (values k (expand-decl v rnm-ctx))))

  (define A-decls* (rename-decl-hash A->common (@ A-decls.value)))
  (define B-decls* (rename-decl-hash B->common (@ B-decls.value)))

  ;; TODO: make an environment that maps the Common ids to
  ;;       their components from A.

  ;; check that all components in B correspond with
  ;; components in A
  (for/and ([(sym B-comp) (in-hash B-decls*)])
    (define A-comp (hash-ref A-decls* sym #f))
    (and
     A-comp
     (sig-entry-matches?
      A-comp
      B-comp))))

;; Decl Decl -> Bool
(define (sig-entry-matches? A B)
  (syntax-parse (list A B)
    #:literal-sets [sig-literals]
    [[(#%val-decl A) (#%val-decl B)]
     (type-matches? #'A #'B)]
    [[(#%type-decl (#%alias A)) (#%type-decl (#%alias B))]
     (type-equal? #'A #'B)]
    [[(#%type-decl (#%opaque)) (#%type-decl (#%opaque))]
     #true]
    [[(#%type-decl (#%alias _)) (#%type-decl (#%opaque))]
     #true]
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

