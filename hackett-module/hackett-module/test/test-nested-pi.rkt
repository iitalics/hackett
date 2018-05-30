#lang hackett-module/outside

(def-signature F
  (Π ([M : (sig (type T))])
    (Π ([N : (sig)])
      (sig (type X = M.T)))))


(def-module G
  (λₘ ([M : (sig (type T))])
    (mod
      (type U M.T))))
