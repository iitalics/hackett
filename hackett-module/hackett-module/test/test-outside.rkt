#lang hackett-module/outside

(def-module M
  (mod
   (type X Integer)

   (defn foo : (∀ [a] {a -> (Tuple a X)})
     [[x] (Tuple (: x a) 5)])

   (def x 2)

   {1 + 1}
   (foo 3)

   ))
