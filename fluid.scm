;; implementation of scheme48 fluid variables using libguile fluids.

(define-module (scsh fluid))

(begin-deprecated
 ;; Prevent `export' from re-exporting core bindings.  This behaviour
 ;; of `export' is deprecated and will disappear in one of the next
 ;; releases.
 (define make-fluid #f))

(export make-fluid set-fluid! fluid let-fluid)

(define guile-make-fluid
  (module-ref (resolve-module '(guile)) 'make-fluid))

(define (make-fluid value)
  (let ((result (guile-make-fluid)))
    (fluid-set! result value)
    result))

(define set-fluid! fluid-set!)

(define fluid fluid-ref)

(define (let-fluid fluid value thunk)
  (with-fluids* (list fluid) (list value) thunk))
