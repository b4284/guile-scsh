(define-module (scsh features))
(export immutable? make-immutable!)

(define (make-immutable! thing) thing)

(define (immutable? thing) #f)
