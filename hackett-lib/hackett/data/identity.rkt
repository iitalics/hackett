#lang hackett

(require hackett/record)
(provide (data Identity) run-identity)

(def-record (Identity a)
  run-identity : a)

(instance (forall [a] (Show a) => (Show (Identity a)))
  [show (λ [(Identity x)] {"(Identity " ++ (show x) ++ ")"})])

(instance (forall [a] (Eq a) => (Eq (Identity a)))
  [== (λ [(Identity x) (Identity y)] {x == y})])

(instance (forall [a] (Semigroup a) => (Semigroup (Identity a)))
  [++ (λ [(Identity x) (Identity y)] (Identity {x ++ y}))])

(instance (forall [a] (Monoid a) => (Monoid (Identity a)))
  [mempty (Identity mempty)])

(instance (Functor Identity)
  [map (λ [f (Identity x)] (Identity (f x)))])

(instance (Applicative Identity)
  [pure Identity]
  [<*> (λ [(Identity f) (Identity x)] (Identity (f x)))])

(instance (Monad Identity)
  [join (λ [(Identity (Identity x))] (Identity x))])
