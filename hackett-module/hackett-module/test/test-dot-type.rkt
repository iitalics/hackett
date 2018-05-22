#lang hackett-module/outside

(def-module M
  (mod (type X Integer)
       (type Y String)
       (type T Integer)
       (def v : T 6)))

(def-module Seal-M
  (λₘ ([M : (sig (type X = Integer)
                 (type Y = String)
                 (type T)
                 (val v : T))])
    M))

(def-module M* (appₘ Seal-M M))

(def x : M*.X 3)
(def y : M*.Y "aedifico")
(def z : M*.T M*.v)

