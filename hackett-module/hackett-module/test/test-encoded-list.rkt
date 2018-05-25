#lang hackett-module/outside

(require hackett/private/test)

(defn quo/rem
  [[x N] (if {{0 <= x} && {x < N}}  (Tuple 0 x)
          (if {{0 <= x} && {0 < N}} (case (quo/rem {x - N} N)
                                      [(Tuple q-1 r)  (Tuple {q-1 + 1} r)])
           (if {{x < 0} && {0 < N}} (case (quo/rem {x + N} N)
                                      [(Tuple q+1 r)  (Tuple {q+1 - 1} r)])
            #|else|#                (error! "quo/rem: N must be positive"))))])

(defn quo [[x N] (fst (quo/rem x N))])
(defn rem [[x N] (snd (quo/rem x N))])

(def-signature LIST
  (sig (type T)
       (val empty : T)
       (val cons : {Integer -> T -> T})
       (val empty? : {T -> Bool})
       (val first : {T -> Integer})
       (val rest : {T -> T})))

(def-module TestPair
  (λₘ ([L : LIST])
    (mod (def list1 : L.T (L.cons 2 L.empty))
         (def list2 : L.T (L.cons 1 list1))
         (def l==!
           (λ (x y) (do {(L.empty? x) ==! (L.empty? y)}
                         (if (L.empty? x)
                             (do {(L.first x) ==! (L.first y)}
                                  {(L.rest x) l==! (L.rest y)})
                             (do (pure Unit))))))
         (def test
           (do {(L.empty? L.empty) ==! True}
               {(L.empty? list1) ==! False}
               {(L.first list1) ==! 2}
               {(L.rest list2) l==! list1})))))


(def-module PrimeExpt
  (seal
   (mod (type T Integer)
        (def empty : T 0)
        (def cons : {Integer -> T -> T}
          (λ (x y)
            (if (== x 0)
                (if (== y 0) 1
                    (* 2 (cons x (- y 1))))
                (* 3 (cons (- x 1) y)))))

        (def empty? : {T -> Bool}
          (λ (n) (== n 0)))

        (def first : {T -> Integer}
          (λ (n) (if (== (rem n 3) 0)
                     (+ 1 (first (quo n 3)))
                     0)))

        (def rest : {T -> T}
          (λ (n) (if (== (rem n 2) 0)
                     (+ 1 (rest (quo n 2)))
                     0))))
   :> LIST))

(def-module TestPrimeExpt (appₘ TestPair PrimeExpt))

(test TestPrimeExpt.test)

