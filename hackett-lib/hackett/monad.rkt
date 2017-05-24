#lang hackett/private/kernel

(require (only-in racket/base define-syntax for-syntax)
         (for-syntax racket/base
                     syntax/parse/class/paren-shape)
         hackett/applicative
         hackett/function
         hackett/functor
         hackett/private/provide
         syntax/parse/define)

(provide (class Monad) =<< >>= <- do ap)

(class (Monad m)
  [join : (∀ [a] {(m (m a)) -> (m a)})])

(def =<< : (∀ [m a b] (=> [(Functor m) (Monad m)] {{a -> (m b)} -> (m a) -> (m b)}))
  (λ [f x] (join (map f x))))

(def >>= : (∀ [m a b] (=> [(Functor m) (Monad m)] {(m a) -> {a -> (m b)} -> (m b)}))
  (flip =<<))

(define-syntax (<- stx) (raise-syntax-error #f "cannot be used as an expression" stx))
(define-syntax-parser do
  #:literals [: <- def]
  [(_ {~and clause [~brackets ~! x:id <- e:expr]} rest ...+)
   (syntax/loc #'clause
     (>>= e (λ [x] (do rest ...))))]
  [(_ e:expr)
   #'e]
  [(_ e:expr rest ...+)
   (syntax/loc #'e
     (>>= e (λ [x] (do rest ...))))])

(def ap : (∀ [m a b] (=> [(Functor m) (Applicative m) (Monad m)] {(m {a -> b}) -> (m a) -> (m b)}))
  (λ [mf mx] (do [f <- mf]
                 [x <- mx]
                 (pure (f x)))))