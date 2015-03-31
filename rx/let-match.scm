;;; These are some macros to support using regexp matching.

;; this file has been renamed to re-match-syntax.scm in scsh.
(define-module (scsh rx let-match)
  :use-module (scsh module-system)
  :use-module (scsh alt-syntax))
(export-syntax let-match if-match match-cond)

;;; (let-match m mvars body ...)
;;; Bind the vars in MVARS to the match & submatch strings of match data M,
;;; and eval the body forms. #F is allowed in the MVARS list, as a don't-care 
;;; parameter.
;;;
;;; (if-match m mvars conseq alt)
;;; The same as LET-MATCH -- eval the CONSEQ form in the scope of the
;;; bound MVARS. However, if the match data M evaluates to false, instead
;;; of blowing up, we execute the ALT form instead.

(define-syntax let-match
  (lambda (exp r c)
    (if (< (length exp) 3)
	(error "No match-vars list in LET-MATCH" exp))
    (let ((m (cadr exp))		; The match expression
	  (mvars (caddr exp))		; The match vars
	  (body (cdddr exp))		; The expression's body forms

	  (%begin           (r 'begin))
	  (%match:substring (r 'match:substring))
	  (%let*            (r 'let*)))

      (cond ((null? mvars) `(,%begin ,@body))

	    ((pair? mvars)
	     (let* ((msv (or (car mvars) (r 'match-val))) ; "match-struct var"
		    (sm-bindings (let recur ((i 0) (vars (cdr mvars)))
				   (if (pair? vars)
				       (let ((var (car vars))
					     (bindings (recur (+ i 1) (cdr vars))))
					 (if var
					     (cons `(,var (,%match:substring ,msv ,i))
						   bindings)
					     bindings))
				       '()))))
	       `(,%let* ((,msv ,m) ,@sm-bindings) ,@body)))


	    (else (error "Illegal match-vars list in LET-MATCH" mvars exp))))))

(define-syntax if-match
  (syntax-rules ()
    ((if-match match-exp mvars on-match no-match)
     (cond (match-exp => (lambda (m) (let-match m mvars on-match)))
	   (else no-match)))))

;;; (MATCH-COND (<match-exp> <match-vars> <body> ...)
;;;             (TEST <exp> <body> ...)
;;;             (TEST <exp> => <proc>)
;;;             (ELSE <body> ...))
;;;
;;; The first clause is as-in IF-MATCH; the next three clauses are as-in COND.
;;;
;;; It would be slicker if we could *add* extra clauses to the syntax
;;; of COND, but Scheme macros aren't extensible this way.
 
;;; Two defs. The other expander produces prettier output -- one COND
;;; rather than a mess of nested IF's.
;(define-syntax match-cond
;  (syntax-rules (else test =>)
;    ((match-cond (else body ...) clause2 ...) (begin body ...))
;
;    ((match-cond) (cond))
;
;    ((match-cond (test exp => proc) clause2 ...)
;     (let ((v exp)) (if v (proc v) (match-cond clause2 ...))))
;
;    ((match-cond (test exp body ...) clause2 ...)
;     (if exp (begin body ...) (match-cond clause2 ...)))
;
;    ((match-cond (test exp) clause2 ...)
;     (or exp (match-cond clause2 ...)))
;
;    ((match-cond (match-exp mvars body ...) clause2 ...)
;     (if-match match-exp mvars (begin body ...)
;	       (match-cond clause2 ...)))))

(define-syntax match-cond
  (syntax-rules ()
    ((match-cond clause ...) (match-cond-aux () clause ...))))

(define-syntax match-cond-aux
  (syntax-rules (test else)

   ;; No more clauses.
   ((match-cond-aux (cond-clause ...))
    (cond cond-clause ...))

   ;; (TEST . <cond-clause>)
   ((match-cond-aux (cond-clause ...)
		    (test . another-cond-clause) clause2 ...)
    (match-cond-aux (cond-clause ... another-cond-clause)
		    clause2 ...))
   
   ;; (ELSE <body> ...)
   ((match-cond-aux (cond-clause ...)
		    (else body ...) clause2 ...)
    (match-cond-aux (cond-clause ... (else body ...))))

   ;; (<match-exp> <mvars> <body> ...)
   ((match-cond-aux (cond-clause ...)
		    (match-exp mvars body ...) clause2 ...)
    (match-cond-aux (cond-clause ... (match-exp => (lambda (m)
						     (let-match m mvars
						       body ...))))
		    clause2 ...))))
