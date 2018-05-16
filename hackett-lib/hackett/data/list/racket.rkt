#lang racket/base

(module untyped racket/base
  (require
   (only-in hackett/private/util/require postfix-in)
   (only-in hackett/private/prim/type-provide typed-out)
   (only-in hackett/private/type-reqprov for-type only-types-in unmangle-types-in)

   hackett/private/base
   (unmangle-types-in #:no-introduce (only-types-in hackett/private/prim/type))
   (only-in (unmangle-types-in #:no-introduce (only-types-in hackett/private/kernel)) forall)

   racket/promise
   racket/match)

  ; has kind * -> *
  ; e.g. (RacketList Integer)
  (define-base-type RacketList)

  (provide (for-type RacketList)
           (typed-out
            [racket-cons : (forall [a] {a -> (RacketList a) -> (RacketList a)})]
            [racket-nil : (forall [a] (RacketList a))]
            ))

  (define ((racket-cons x) xs) (cons x xs))
  (define racket-nil '())

  )

(module typed hackett
  (require hackett
           (only-in racket/base submod))
  (require (submod ".." untyped))

  (provide list->racket-list)

  (def list->racket-list : (forall [a] {(List a) -> (RacketList a)})
    (foldr racket-cons racket-nil))

  )


(require
 (only-in hackett/private/type-reqprov for-type only-types-in unmangle-types-in)
 (unmangle-types-in #:no-introduce 'untyped)
 'typed)

(provide (for-type RacketList)
         racket-cons
         racket-nil
         list->racket-list)
