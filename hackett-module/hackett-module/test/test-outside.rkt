#lang hackett-module/outside

(type X String)
(def u : X "hello")

(def-module M
  (mod
   (type X Integer)

   (defn foo : (∀ [a] {a -> (Tuple a X)})
     [[x] (Tuple (: x a) 5)])

   {1 + 1}
   (foo 3)

   ))
