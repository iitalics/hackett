#lang hackett-module/outside

(def-signature S (sig (type X) (val x : X)))

(def-module F
  (seal
   (λₘ ([dummy : (sig)])       ; removing this dummy layer here
     (λₘ ([M : (sig (type T))])
       (mod (type U M.T))))
   :>
   (Π ([dummy : (sig)])        ; and this dummy layer here gets rid of the error
     (Π ([M : (sig (type T))])
       (sig (type U = M.T))))))


(def-module G
  (seal
   (λₘ ([dummy : (sig)])  ; removing this dummy layer here
     (λₘ ([M : S])
       (mod
        (def v : M.X M.x))))
   :>
   (Π ([dummy : (sig)])   ; and this dummy layer here gets rid of the error
     (Π ([M : S])
       (sig
        (val v : M.X))))))

