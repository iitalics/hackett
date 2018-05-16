#lang hackett
(require hackett/record
         hackett/private/test)

(def-record Posn
  posn-x : Integer
  posn-y : Integer)

(derive-instance Show Posn)

(test {(show (Posn 2 3)) ==! "(Posn 2 3)"})
(test {(posn-x (Posn 2 3)) ==! 2})
(test {(posn-y (Posn 2 3)) ==! 3})
