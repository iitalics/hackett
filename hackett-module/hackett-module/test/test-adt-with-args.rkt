#lang hackett-module/outside

(def-module M
  (mod
    (data Posn (MkPosn Integer Integer))))

(def three-four
  (M.MkPosn 3 4))

(def three
  (case three-four
    [(M.MkPosn x y) x]))
