(define-module (scsh primitives))
(export unspecific)

(define (unspecific) (if #f #f))
