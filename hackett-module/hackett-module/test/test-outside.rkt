#lang hackett-module/outside

(def-module M
  (mod
   (type X Integer)

   (defn foo : (∀ [a] {a -> (Tuple a X)})
     [[x] (Tuple (: x a) 5)])

   {1 + 1}
   (foo 3)

   ))

(def-module M* M)

(type Y M.X)
(def y : Y 8)

#;
(def-module F
  (λₘ ([m : (sig (type X))]) m))
