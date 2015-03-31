;;; Regular expression matching for scsh
;;; Copyright (c) 1998 by Olin Shivers.

(define-module (scsh rx re-high)
  :use-module (ice-9 receive)
  :use-module (scsh let-opt)
  :use-module (scsh rx cond-package)
  :use-module (scsh rx re-low)
  :use-module (scsh rx re)
  :use-module (scsh rx posixstr)
)
(export compile-regexp regexp-search regexp-search?)

;;; Translates the re to a Posix string, and returns a CRE record,
;;; but doesn't actually compile the Posix string into a C regex_t struct.
;;; Uses the :POSIX field to cache the CRE record.

(define (compile-regexp re)
  (let* ((compile (lambda () (receive (s lev pcount tvec)
				 (regexp->posix-string re)
			       (new-cre s tvec))))

	 (check-cache (lambda (fetch set)
			(or (fetch re)		; Already cached.
			    (let ((cre (compile)))	; Compile it,
			      (set re cre)		; cache it,
			      cre)))))			; and return it.

    (? ((re-seq? re)
	(check-cache re-seq:posix set-re-seq:posix))
       ((re-choice? re)
	(check-cache re-choice:posix set-re-choice:posix))
       ((re-repeat? re)
	(check-cache re-repeat:posix set-re-repeat:posix))
       ((re-char-set? re)
	(check-cache re-char-set:posix set-re-char-set:posix))
       ((re-string? re)
	(check-cache re-string:posix set-re-string:posix))
       ((re-submatch? re)
	(check-cache re-submatch:posix set-re-submatch:posix))
       ((re-dsm? re)
	(check-cache re-dsm:posix set-re-dsm:posix))

       ((re-bos? re) (or bos-cre (set! bos-cre (compile))))
       ((re-eos? re) (or eos-cre (set! eos-cre (compile))))

       ((re-bol? re) (error "BOL regexp not supported in this implementation."))
       ((re-eol? re) (error "EOL regexp not supported in this implementation."))

       ((re-bow? re) (or bow-cre (set! bow-cre (compile))))
       ((re-eow? re) (or eow-cre (set! eow-cre (compile))))

       (else (error "compile-regexp -- not a regexp" re)))))

(define bos-cre #f)
(define eos-cre #f)
(define bow-cre #f)
(define eow-cre #f)



(define (regexp-search re str . maybe-start)
  (let* ((tsm (re-tsm re))
	 (svec (make-vector (+ 1 tsm) #f))
	 (evec (make-vector (+ 1 tsm) #f))
	 (cre (compile-regexp re)))
    (cre-search cre svec evec str (:optional maybe-start 0))))
	

(define (regexp-search? re str . maybe-start)
  (cre-search? (compile-regexp re) str (:optional maybe-start 0)))
