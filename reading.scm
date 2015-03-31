(define-module (scsh reading))
(export reading-error)

(define (reading-error port message . irritants)
  (apply error message (append irritants (list port))))
