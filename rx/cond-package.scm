(define-module (scsh rx cond-package)
  :use-module (scsh module-system)
  :use-module (scsh alt-syntax))
(export-syntax define-simple-syntax when unless ? switchq switch prog0 land*)
(export %switch %switchq)

(define-structure conditionals
  (export (define-simple-syntax :syntax)
	  (when    :syntax)
	  (unless  :syntax)
	  (?       :syntax)
	  (switchq :syntax)
	  (switch  :syntax)
	  (prog0   :syntax)
	  (land*   :syntax))
  (open scheme)
  (begin

;;; (define-simple-syntax (name subforms ...) expansion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-simple-syntax
  (syntax-rules ()
    ((define-simple-syntax (name subforms ...) expansion)
     (define-syntax name (syntax-rules () ((name subforms ...) expansion))))))


;;; ? = COND
;;; (WHEN test body ...)		(SWITCHQ = key clause ...)
;;; (UNLESS test body ...)		(SWITCH  = key clause ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handy conditional forms. ? is so short that it renders WHEN pretty
;;; much useless.

(define-simple-syntax (when test body ...)
  (if test (begin body ...)))

(define-simple-syntax (unless test body ...)
  (if (not test) (begin body ...)))

;;; ? is synonym for COND.
(define-simple-syntax (? clause ...) (cond clause ...))


;;; (PROG0 val-exp exp ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-syntax (prog0 val-exp exp ...)
  (let ((v val-exp)) exp ... v))


;;; (land* (clause ...) body ...)				-*- Scheme -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluate each clause. If any clause returns false, land* stops and
;;; returns false. If all the clauses evaluate to a true value, return
;;; the value of the body.
;;;
;;; The difference between LAND* and AND is that LAND* binds names to
;;; the values of its clauses, which may be used by subsequent clauses.
;;; Clauses are of the form
;;;     (var exp)	; binds VAR to the value of EXP.
;;;     (exp)		; No binding.
;;;     var		; Reference -- no binding. 
;;;
;;; Example:
;;; (land* ((probe (assq key alist)))
;;;   (cdr probe))
;;;
;;; LAND* is due to Oleg Kiselyov (http://pobox.com/~oleg); I wrote this
;;; simple implementation as a high-level R5RS DEFINE-SYNTAX macro.
;;;     Olin 98/9/29

(define-syntax land*
  (syntax-rules ()
    ((land* () body ...) (begin body ...))

    ((land* ((var exp) clause ...) body ...)
     (let ((var exp)) (and var (land* (clause ...) body ...))))

    ((land* ((#f exp) clause ...) body ...)
     (and exp (land* (clause ...) body ...)))

    ((land* ((exp) clause ...) body ...)
     (and exp (land* (clause ...) body ...)))

    ((land* (var clause ...) body ...)
     (and var (land* (clause ...) body ...)))))



;;; Like CASE, but you specify the key-comparison procedure.
;;; SWITCH evaluates its keys each time through the conditional.
;;; SWITCHQ keys are not evaluated -- are simply constants.
;;; (switchq string=? (vector-ref vec i)
;;;   (("plus" "minus") ...)
;;;   (("times" "div")  ...)
;;;   (else ...))

(define-simple-syntax (switchq compare key clause ...)
  (let ((k key)				; Eval KEY and COMPARE
	(c compare))			; just once, then call %switch.
    (%switchq c k clause ...)))		; C, K are vars, hence replicable.

(define-syntax %switchq
  (syntax-rules (else)
    ((%switchq compare key ((key1 ...) body1 body2 ...) rest ...)
     (if (or (compare key 'key1) ...)
	 (begin body1 body2 ...)
	 (%switchq compare key rest ...)))

    ((%switchq compare key ((key1 ...)) rest ...)	; Null body.
     (if (not (or (compare key 'key1) ...))
	 (%switchq compare key rest ...)))
    
    ((%switchq compare key (else body ...))
     (begin body ...))

    ((%switchq compare key) '#f)))


(define-simple-syntax (switch compare key clause ...)
  (let ((k key)				; Eval KEY and COMPARE
	(c compare))			; just once, then call %switch.
    (%switch c k clause ...)))		; C, K are vars, hence replicable.

(define-syntax %switch
  (syntax-rules (else)
    ((%switch compare key ((key1 ...) body1 body2 ...) rest ...)
     (if (or (compare key key1) ...)
	 (begin body1 body2 ...)
	 (%switch compare key rest ...)))

    ((%switch compare key ((key1 ...)) rest ...)	; Null body.
     (if (not (or (compare key key1) ...))
	 (%switch compare key rest ...)))
    
    ((%switch compare key (else body ...))
     (begin body ...))

    ((%switch compare key) '#f)))

;;; I can't get this to work -- S48 complains "too many ...'s".
;(define-syntax switchq
;  (syntax-rules (else)
;    ((switchq compare key clause ...)
;     (letrec-syntax ((%switchq (syntax-rules (else)
;			         ((%switchq compare key
;					   ((key1 ...) body1 body2 ...) rest ...)
;				  (if (or (compare key 'key1) ...)
;				      (begin body1 body2 ...)
;				      (%switchq compare key rest ...)))
;
;				 ; Null body.
;				 ((%switchq compare key ((key1 ...)) rest ...)
;				  (if (not (or (compare key 'key1) ...))
;				      (%switchq compare key rest ...)))
;    
;			         ((%switchq compare key (else body ...))
;				  (begin body ...))
;
;				 ((%switchq compare key) '#f))))
;
;        (let ((k key)			 ; Eval KEY and COMPARE
;	      (c compare))		 ; just once, then call %switch.
;	   (%switchq c k clause ...)))))); C, K are vars, hence replicable.
))
