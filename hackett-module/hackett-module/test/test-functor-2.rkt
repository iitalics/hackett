#lang hackett-module/outside

(def-signature A (sig (type X) (val x : X) (val f : (-> X Integer))))

(def-module M
  (seal (mod (type X Integer)
             (def x : X 5)
             (def f : (-> X Integer) (λ (x) x)))
        :>
        A))

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

;; -------------------------------------
#|

;; the M55 in the error message comes from this functor:
(def-module G
  (λₘ ([M : A])
    (λₘ ([N : (sig (type X = M.X) (type Y) (val y : Y) (val f : (-> Y X)))])
      (seal
       (mod (type X M.X)
            (def ffy : Integer (M.f (N.f N.y))))
       :>
       (sig (type X = M.X) (val ffy : Integer))))))


(def-module O (appₘ (appₘ G M) N))
|#

; -------------------

(def-module H
  (λₘ ([A : (sig (type X))])
    (λₘ ([B : (sig (type X = A.X))])
      (mod))))

(def-module H-M (appₘ H M))
;; should infer:
;;   (Π ([B- (sig (type X = opaque:M.X-))]) (sig))
;;
;; instead it incorrectly infers:
;;   (Π ([B- (sig (type X = opaque:A55.X-))]) (sig))
;;                                 ^
;;                      comes from A in the defn. of H
