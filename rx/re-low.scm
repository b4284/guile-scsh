;;; Regular expression matching for scsh
;;; Copyright (c) 1994 by Olin Shivers.

(define-module (scsh rx re-low)
  :use-module (regex spencer)
  :use-module (scsh define-foreign-syntax)
  :use-module (scsh defrec)
  :use-module (scsh let-opt)
  :use-module (scsh utilities)
)
(export match:start match:end match:substring)
(export regexp-match:start regexp-match:end regexp-match:string)
(export new-cre cre-search cre-search? cre:string cre:tvec)

(foreign-source
  "/* Make sure foreign-function stubs interface to the C funs correctly: */"
  "#include <sys/types.h>"
  "#include \"../regexp/regex.h\""
  "#include \"re1.h\""
  "" ""
  )

;;; Match data for regexp matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record regexp-match
  string	; The string against which we matched
  start		; vector of starting indices
  end)		; vector of ending indices

(define (match:start match . maybe-index)
  (vector-ref (regexp-match:start match)
	      (:optional maybe-index 0)))

(define (match:end match . maybe-index)
  (vector-ref (regexp-match:end match)
	      (:optional maybe-index 0)))

(define (match:substring match . maybe-index)
  (let* ((i (:optional maybe-index 0))
	 (start (vector-ref (regexp-match:start match) i)))
    (and start (substring (regexp-match:string match)
			  start
			  (vector-ref (regexp-match:end match) i)))))

;;; Compiling regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; There's no legal Posix string expressing the empty match (e.g., (|))
;;; that will never match anything. So when we have one of these, we set
;;; the STRING field to #f. The matchers will spot this case and handle it
;;; specially.

;;; We compile the string two ways, on demand -- one for cre-search, and
;;; one for cre-search?.

(define-record cre	; A compiled regular expression
  string		; The Posix string form of the regexp or #F.
  max-paren		; Max paren in STRING needed for submatches.
  (bytes    #f)		; Pointer to the compiled form, in the C heap, or #F.
  (bytes/nm #f)		; Same as BYTES, but compiled with no-submatch.
  tvec			; Translation vector for the submatches
  ((disclose self) (list "cre" (cre:string self))))

(define (new-cre str tvec) (make-cre str (max-live-posix-submatch tvec) tvec))

(define (max-live-posix-submatch tvec)
  (vfold (lambda (sm mlpsm) (if sm (max mlpsm sm) mlpsm)) 0 tvec))

; (define (compile-posix-re->c-struct re-string sm?)
;   (receive (errcode c-struct) (%compile-re re-string sm?)
;     (if (zero? errcode) c-struct
; 	(error errcode (%regerror-msg errcode c-struct)
; 	       compile-posix-re->c-struct re-string sm?))))

(define (compile-posix-re->c-struct re-string sm?)
  (regcomp re-string (logior REG_EXTENDED REG_PEND (if sm? 0 REG_NOSUB))))

(define-foreign %compile-re (compile_re (string-desc pattern) (bool submatches?))
  integer ; 0 or error code
  (C regex_t*))


;;; Searching with compiled regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cre-search returns match info; cre-search? is just a predicate.

; (define (cre-search cre start-vec end-vec str start)
;   (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
;     (and re-str
; 	 (let* ((C-bytes (or (cre:bytes cre)
; 			     (let ((C-bytes (compile-posix-re->c-struct re-str #t)))
; 			       (set-cre:bytes cre C-bytes)
; 			       (register-re-c-struct cre C-bytes)
; 			       C-bytes)))
; 		(retcode (%cre-search C-bytes str start
; 				      (cre:tvec cre)
; 				      (cre:max-paren cre)
; 				      start-vec end-vec)))
; 	   (if (integer? retcode)
; 	       (error retcode (%regerror-msg retcode C-bytes)
; 		      cre-search cre start-vec end-vec str start)
; 	       (and retcode (make-regexp-match str start-vec end-vec)))))))

(define (cre-search cre start-vec end-vec str start)
  (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
    (and re-str
 	 (let* ((C-bytes (or (cre:bytes cre)
 			     (let ((C-bytes (compile-posix-re->c-struct
					     re-str #t)))
 			       (set-cre:bytes cre C-bytes)
 			       C-bytes)))
		(retcode (regexec C-bytes str REG_STARTEND -1 start))
		(tvec (cre:tvec cre)))
	   (cond (retcode
		  (vector-set! start-vec 0 (regmatch:start retcode 0))
		  (vector-set! end-vec 0 (regmatch:end retcode 0))
		  (do ((i (- (vector-length start-vec) 2) (- i 1)))
		      ((< i 0))
		    (let ((j-scm (vector-ref tvec i)))
		      (cond (j-scm
			     (let ((k (regmatch:start retcode j-scm))
				   (l (regmatch:end retcode j-scm)))
			       (vector-set! start-vec (+ i 1)
					    (if (= k -1) #f k))
			       (vector-set! end-vec (+ i 1)
					    (if (= l -1) #f l)))))))))
	   (and retcode (make-regexp-match str start-vec end-vec))))))

; (define (cre-search? cre str start)
;   (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
;     (and re-str
; 	 (let* ((C-bytes (or (cre:bytes/nm cre)
; 			     (let ((C-bytes (compile-posix-re->c-struct re-str #f)))
; 			       (set-cre:bytes/nm cre C-bytes)
; 			       (register-re-c-struct cre C-bytes)
; 			       C-bytes)))
; 		(retcode (%cre-search C-bytes str start '#() -1 '#() '#())))
; 	   (if (integer? retcode)
; 	       (error retcode (%regerror-msg retcode C-bytes)
; 		      cre-search? cre str start)
;	       retcode)))))

(define (cre-search? cre str start)
  (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
    (and re-str
 	 (let* ((C-bytes (or (cre:bytes/nm cre)
 			     (let ((C-bytes (compile-posix-re->c-struct
					     re-str #f)))
 			       (set-cre:bytes/nm cre C-bytes)
 			       C-bytes))))
	   (regexec C-bytes str REG_STARTEND 0 start)))))

(define-foreign %cre-search
  (re_search ((C "const regex_t *~a") compiled-regexp)
	      (string-desc str)
	      (integer start)
	      (vector-desc tvec) (integer max-psm)
	      (vector-desc svec) (vector-desc evec))
  desc)	; 0 success, #f no-match, or non-zero int error code.


;;; Generate an error msg from an error code.

(define-foreign %regerror-msg (re_errint2str (integer errcode)
					     ((C "const regex_t *~a") re))
  string)


;;; Reclaiming compiled regexp storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avert your eyes from the unsightly crock.
;;;
;;; S48 0.36 doesn't have finalizers, so we don't have a way to free
;;; the C regexp_t structure when its CRE record is gc'd. So our current
;;; lame approximation is to keep track of all the CRE's with a list of
;;;     (cre-weak-pointer . regex_t*)
;;; pairs. From time to time, we should walk the list. If we deref the
;;; weak pointer and discover the CRE's been GC'd, we free the regex_t
;;; struct.
;;;
;;; Note this code is completely thread unsafe.

;;; Free the space used by a compiled regexp.
(define-foreign %free-re (free_re ((C regex_t*) re)) ignore)

;(define *master-cre-list* '())

;;; Whenever we make a new CRE, use this proc to add it to the master list.
;(define (register-re-c-struct cre c-bytes)
;  (set! *master-cre-list* (cons (cons (make-weak-pointer cre) c-bytes)
;				*master-cre-list*)))

(define (clean-up-cres) #t)

; (define (clean-up-cres)
;   (set! *master-cre-list*
; 	(fold (lambda (elt lis)
; 		(if (weak-pointer-ref (car elt)) ; Still alive
; 		    (cons elt lis)
; 		    (begin (%free-re (cdr elt))
; 			   lis)))
; 	      '()
; 	      *master-cre-list*)))
