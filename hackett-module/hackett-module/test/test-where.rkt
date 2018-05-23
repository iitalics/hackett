#lang hackett-module/outside

(def-signature A (sig (type X) (val x : X) (val f : (-> X Integer))))
(def-signature B (sig (type X) (type Y) (val y : Y) (val f : (-> Y X))))
(def-signature C (sig (type X) (val ffy : Integer)))

(def-module A*
  (seal (mod (type X Integer)
             (def x : X 5)
             (def f : (-> X Integer) (λ (x) x)))
        :>
        A))

(def-module B*/A
  (λₘ ([A : A])
    (seal
     (mod (type X A.X)
          (type Y (-> Integer X))
          (def y : Y (λ (dummy) A.x))
          (def f : (-> Y X) (λ (y) (y 0))))
     :>
     (where B X = A.X))))

(def-module B* (appₘ B*/A A*))  ; (where B X = A*.X)


(def-module C*/AB
  (λₘ ([A : A])
    (λₘ ([B : (where B X = A.X)])
      (seal
       (mod (type X A.X)
            (def ffy : Integer (A.f (B.f B.y))))
       :>
       (where C X = A.X)))))


(def-module C* (seal (appₘ (appₘ C*/AB A*) B*) :> C))


