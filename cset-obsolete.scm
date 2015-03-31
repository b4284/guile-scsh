;;; Support for obsolete, deprecated 0.5.2 char-set procedures.
;;; Will go away in a future release.

(define-module (scsh cset-obsolete)
  :use-module (srfi srfi-14)
  :use-module (scsh module-system)
  :use-module (scsh utilities))

;;(define-interface obsolete-char-set-interface
  (export char-set-members		; char-set->list
	  chars->char-set		; list->char-set
	  ascii-range->char-set		; ucs-range->char-set (not exact)
	  predicate->char-set		; char-set-filter (not exact)
	  ;->char-set			; no longer handles a predicate
	  char-set-every?		; char-set-every
	  char-set-any?			; char-set-any

	  char-set-invert		; char-set-complement
	  char-set-invert!		; char-set-complement!

	  reduce-char-set		; char-set-fold

	  char-set:alphabetic		; char-set:letter
	  char-set:numeric		; char-set:digit
	  char-set:alphanumeric		; char-set:letter+digit
	  char-set:control)		; char-set:iso-control


(define-structure obsolete-char-set-lib obsolete-char-set-interface
  (open scsh-utilities char-set-lib scheme)
  (begin
    
    (define char-set-members
      (deprecated-proc char-set->list 'char-set-members
		       "Use CHAR-SET->LIST instead."))
    (define chars->char-set
      (deprecated-proc list->char-set 'chars->char-set
		       "Use LIST->CHAR-SET instead."))
    (define ascii-range->char-set
      (deprecated-proc (lambda (lower upper) (ucs-range->char-set lower upper #t))
		       'ascii-range->char-set
		       "Use UCS-RANGE->CHAR-SET instead."))
    (define predicate->char-set
      (deprecated-proc (lambda (pred) (char-set-filter pred char-set:full))
		       'predicate->char-set
		       "Change code to use CHAR-SET-FILTER."))
    (define char-set-every?
      (deprecated-proc char-set-every 'char-set-every?
		       "Use CHAR-SET-EVERY instead."))
    (define char-set-any?
      (deprecated-proc char-set-every 'char-set-any?
		       "Use CHAR-SET-ANY instead."))
    (define char-set-invert
      (deprecated-proc char-set-complement 'char-set-invert
		       "Use CHAR-SET-COMPLEMENT instead."))
    (define char-set-invert!
      (deprecated-proc char-set-complement! 'char-set-invert!
		       "Use CHAR-SET-COMPLEMENT! instead."))

    (define reduce-char-set (deprecated-proc char-set-fold 'char-set-fold
					 "Use char-set-fold instead."))

    (define char-set:alphabetic		char-set:letter)
    (define char-set:numeric		char-set:digit)
    (define char-set:alphanumeric	char-set:letter+digit)
    (define char-set:control		char-set:iso-control)))
