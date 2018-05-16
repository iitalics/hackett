#lang hackett/private/kernel

(require hackett/record
         hackett/private/provide)

(provide (data Unit) (data Bool) (data Tuple) (data Maybe) (data Either) (data List)
         (data IO) (data Real-World)
         fst snd)

(data Unit Unit)
(data Bool True False)
(data (Maybe a) (Just a) Nothing)
(data (Either a b) (Left a) (Right b))
(data (List a)
  {a :: (List a)} #:fixity right
  Nil)

(def-record (Tuple a b)
  fst : a
  snd : b)

(data Real-World Real-World)
(data (IO a) (IO (-> Real-World (Tuple Real-World a))))
