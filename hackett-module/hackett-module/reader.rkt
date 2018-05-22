#lang racket/base
(provide (all-defined-out))
(require hackett/private/reader)

(define (call-with-hackett-module-reading-parameterization f)
  ;; TODO:
  ;;   hackett's reader parameterization prevents `read-cdot` from activating;
  ;;   we may need to reimplement hackett's dot reader in order for it to
  ;;   cooperate... for now, #%dot is more important than `.` as a symbol.
  (parameterize ([read-cdot #t])
    (f)))
