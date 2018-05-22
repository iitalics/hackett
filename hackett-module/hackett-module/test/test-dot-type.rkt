#lang hackett-module/outside

(def-module M
  (mod (type X Integer)
       (type Y String)
       (type T Integer)
       (def v : T 6)))

(def-module M*
  (seal M
        :>
        (sig (type X = Integer)
             (type Y = String)
             (type T)
             (val v : T))))

(def x : M*.X 3)
(def y : M*.Y "aedifico")
(def z : M*.T M*.v)

