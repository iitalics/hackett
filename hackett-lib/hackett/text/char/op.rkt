#lang racket/base
(require
 (only-in hackett/private/util/require postfix-in)
 (only-in hackett/private/prim/type-provide typed-out)
 (only-in hackett/private/type-reqprov for-type only-types-in unmangle-types-in)

 hackett/private/base
 (unmangle-types-in #:no-introduce (only-types-in hackett/private/prim/type))
 (unmangle-types-in #:no-introduce (only-types-in hackett/data/list/racket))
 (only-in hackett/private/prim/type
          [Unit MkUnit] [Tuple MkTuple] [IO MkIO])

 (postfix-in - (combine-in racket/base
                           racket/promise))
 (prefix-in lazy: lazy)
 (for-syntax racket/base syntax/parse hackett/private/typecheck))

(define-base-type Char)

(define-syntax ch
  (syntax-parser
    [(_ c:char)
     (attach-type #'(quote- c) (expand-type #'Char))]))

(provide (for-type Char)
         (typed-out
          [equal?/Char : {Char -> Char -> Bool}]
          [show/Char : {Char -> String}]
          [string-ref : {String -> Integer -> Char}]
          [racket-list->string : {(RacketList Char) -> String}]
          )
         ch)

(define ((string-ref s) i)
  (string-ref- (force- s) (force- i)))

(define ((equal?/Char c) d)
  (char=? (force- c) (force- d)))

(define (show/Char c)
  (format "~v" (force- c)))

(define (racket-list->string l)
  (list->string- (lazy:!!list l)))
