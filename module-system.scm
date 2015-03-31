;;; dummy definitions for Guile.

(define-module (scsh module-system))
(export-syntax define-structure structure-ref)

;; pick out the begin forms.
(defmacro define-structure (name interface . body)
  (let loop ((rest body)
	     (result '(begin)))
    (if (null? rest)
	(reverse result)
	(loop (cdr rest)
	      (if (eq? (caar rest) 'begin)
		  (cons (car rest) result)
		  result)))))

(defmacro structure-ref (structure symb)
  symb)
