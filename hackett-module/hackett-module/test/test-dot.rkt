#lang hackett-module/outside

(def-module M
  (mod
    (type X Integer)
    (def simple : X 5)
    ))

(def x M.simple)


(def-module F
  (λₘ ([A : (sig (type X)
                 (val simple : X))])
    A))

(def-module M-sealed (appₘ F M))

(def x-sealed M-sealed.simple)
