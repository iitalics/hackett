#lang racket/base
(require
 racket/match
 (for-syntax
  racket/base
  syntax/parse))

(provide
 (struct-out mod)
 mod-value-ref
 mod-pattern-ref
 (struct-out pat-info)
 app/pat-info
 make-pat-info)

;; Represents a `mod` form at link-time

; vals : [Hashof Sym Any]
; pats : [Hashof Sym PatInfo]
(struct mod
  [vals pats]
  #:transparent)

;; Represents the information need to reconstruct
;; match-expanders for a data-constructor exported
;; by a module.
; predicate : [Any -> Boolean]
; ->values : [Any -> Any ...]
(struct pat-info
  [predicate
   ->values]
  #:transparent)

(define (mod-value-ref m x)
  (hash-ref (mod-vals m) x))

(define (mod-pattern-ref m x)
  (hash-ref (mod-pats m) x))

(define-match-expander app/pat-info
  (syntax-parser
    [(_ pat-info-expr:id (sub-pat ...))
     #'(? (pat-info-predicate pat-info-expr)
          (app (pat-info-->values pat-info-expr) sub-pat ...))]))

(define-syntax make-pat-info
  (syntax-parser
    [(_ pat:expr (sub-id:id ...))
     #'(pat-info
        (λ (v)
          (match v [pat #true] [_ #false]))
        (λ (v)
          (match v [pat (values sub-id ...)])))]))

;; ------------------------------------------------------------------------------

(module+ test
  (require rackunit racket/contract)

  (define (uncons x)
    (values (car x) (cdr x)))

  (define (list->values l)
    (apply values l))

  (define (->no-values x)
    (values))

  (define (functor M-)
    (define x (mod-value-ref M- 'x))
    (define :: (mod-value-ref M- '::))
    (define ::-pat (mod-pattern-ref M- '::))
    (define Nil (mod-value-ref M- 'Nil))
    (define Nil-pat (mod-pattern-ref M- 'Nil))

    (check-equal? x 4)
    (check-equal? (match (:: 3 4)
                    [(app/pat-info ::-pat (a b)) (+ a b)])
                  7)
    (check-equal? (match Nil
                    [(app/pat-info Nil-pat ()) 'yes]
                    [_ 'no])
                  'yes))

  (functor
   (mod
     (hash 'x 4
           ':: cons
           'Nil '())
     (hash ':: (make-pat-info (cons a b) [a b])
           'Nil (make-pat-info '() []))))

  (functor
   (mod
     (hash 'x 4
           ':: list
           'Nil 'nil)
     (hash ':: (make-pat-info (list a b) [a b])
           'Nil (make-pat-info 'nil []))))

  )
