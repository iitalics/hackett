#lang hackett-module/outside

(def-signature EMPTY
  (sig))

(def-signature NUMBERS
  (sig
    (val x : Integer)
    (val y : Integer)
    (val z : Integer)))

(def-module Empty (mod))
(def-module Numbers
  (mod (def x 3) (def y x) (def z {x + 1})))

(def-module Reorder
  (λₘ ([N : NUMBERS])
    (mod
      (def x N.y)
      (def y N.x)
      (def z N.z))))

;; ======================

(def-signature TYPES
  (sig
    (type T)
    (type U)))

(def-module ReorderTypes
  (λₘ ([M : TYPES])
    (mod
      (type T M.U)
      (type U M.T))))
