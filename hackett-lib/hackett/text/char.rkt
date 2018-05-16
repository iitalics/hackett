#lang hackett
(require
 hackett/text/char/op
 hackett/data/list/racket
 (only-in hackett/private/type-reqprov for-type))

(provide
 ch
 (for-type Char)
 string-ref
 string->list
 )

(instance (Eq Char)
  [== equal?/Char])

(instance (Show Char)
  [show show/Char])

(defn string->list : {String -> (List Char)}
  [[s]
   (letrec
       ([f (λ [i]
             (if {i < (string-length s)}
                 {(string-ref s i) :: (f {i + 1})}
                 Nil))])
     (f 0))])

(def list->string : {(List Char) -> String}
  {racket-list->string . list->racket-list})
