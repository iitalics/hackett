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
  (mod (def x 3)
       (def y x)
       (def z {x + 1})))

(def-module Reorder
  (λₘ ([N : NUMBERS])
    (mod
      (def x (: "im not an int" String))
      (def y N.x)
      (def z N.z))))

(def-module Numbers+
  (appₘ Reorder Numbers))

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
;; should be shown as:
;inferred:
;(#%pi-sig
; ((M33
;   (#%sig
;    #hash((T . T) (U . U))
;    #hash((T . (#%type-decl (#%opaque))) (U . (#%type-decl (#%opaque)))))))
; (#%sig
;  #hash((T . T) (U . U))
;  #hash((T . (#%type-decl (#%alias (#%dot M33 U))))
;        (U . (#%type-decl (#%alias (#%dot M33 T)))))))

;; If it doesn't say `#%dot` here and says
;; `(#%type:con opaque:M.U34)` or something it's wrong!

(def-module ReorderTypes*
  (seal ReorderTypes
        :>
        (Π ([M : TYPES])
          (sig (type T = M.U)
               (type U = M.T))))
