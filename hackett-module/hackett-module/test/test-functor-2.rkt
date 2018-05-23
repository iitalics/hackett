#lang hackett-module/outside

(def-signature A (sig (type X) (val x : X) (val f : (-> X Integer))))

(def-module M
  (seal (mod (type X Integer)
             (def x : X 5)
             (def f : (-> X Integer) (λ (x) x)))
        :>
        A))

;; the M18 in the error message come from this definition:
(def-module F
  (λₘ ([M : A])
    (seal
     (mod (type X M.X)
          (type Y (-> Integer X))
          (def y : Y (λ (dummy) M.x))
          (def f : (-> Y X) (λ (y) (y 0))))
     :>
     (sig (type X = M.X) (type Y) (val y : Y) (val f : (-> Y X))))))

(def-module N (appₘ F M))

