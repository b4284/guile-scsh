;;; Regexp support for Scheme
;;;     Olin Shivers, January 1997, May 1998.

;;; Todo:
;;; - Better unparsers for (word ...) and (word+ ...).
;;; - Unparse char-sets into set-diff SREs -- find a char set that's a
;;;   tight bound, then get the difference.  This would really pretty up
;;;   things like (- alpha "aeiou")

;;; Exports:
;;; (sre->regexp sre)		SRE->ADT parser
;;; (regexp->sre re)		ADT->SRE unparser
;;;
;;; Procedures that parse sexp regexps and translate ADTs for low-level macros:
;;; (parse-sre  sre  rename compare)
;;; (parse-sres sres rename compare)
;;; (regexp->scheme re rename)
;;;
;;; (char-set->in-pair cset)	Char-set unparsing utility

;;; Character-set dependencies:
;;; The only stuff in here dependent on the implementation's character type
;;; is the char-set parsing and unparsing, which deal with ranges of 
;;; characters. We assume an 8-bit ASCII superset.

;;; Imports:
;;; ? for COND, and SWITCHQ conditional form.
;;; every

;;; This code is much hairier than it would otherwise be because of the
;;; the presence of ,<exp> forms, which put a static/dynamic duality over
;;; a lot of the processing -- we have to be prepared to handle either
;;; re's or Scheme epressions that produce re's; char-sets or Scheme 
;;; expressions that produce char-sets. It's a pain.
;;;
;;; See comments in re.scm ADT code about building regexp trees that have
;;; code in the record fields instead of values.
;;;
;;; The macro expander works by parsing the regexp form into an re record,
;;; and simplifying it. If the record is completely static, it is then 
;;; translated, at macro-expand time, into a Posix regex string. If the
;;; regexp needs runtime values -- e.g, the computed from and to fields in 
;;;     (** "ha, " (- min 1) (+ max 1))
;;; -- the expander instead produces Scheme ADT constructors to build
;;; the regexp at run-time.

(define-module (scsh rx parse)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-14)
  :use-module (ice-9 receive)
  :use-module (scsh ascii)
  :use-module (scsh rx re-low)
  :use-module (scsh rx re)
  :use-module (scsh rx cond-package)
  :use-module (scsh rx spencer)
)
(export static-regexp? sre->regexp parse-sre parse-sres regexp->scheme
	char-set->in-pair)

;;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Is a parsed regexp completely determined statically, or does it
;;; have dynamic components (e.g., a ,@<pattern> or a computed char-set)
;;; in the form of embedded code in some of the regexp's fields?

(define (static-regexp? re)
  (? ((re-seq?    re)   (every static-regexp? (re-seq:elts    re)))
     ((re-choice? re)   (every static-regexp? (re-choice:elts re)))

     ((re-char-set? re) (char-set? (re-char-set:cset re)))    ; Might be code.

     ((re-repeat? re)		; FROM & TO fields might be code.
      (let ((to (re-repeat:to re)))
	(and (integer? (re-repeat:from re))
	     (or (not to) (integer? to))
	     (static-regexp? (re-repeat:body re)))))

     ((re-dsm? re)      (static-regexp? (re-dsm:body re)))
     ((re-submatch? re) (static-regexp? (re-submatch:body re)))

     (else (or (re-bos? re) (re-eos? re)	; Otw, if it's not 
	       (re-bol? re) (re-eol? re)	; one of these,
	       (re-bow? re) (re-eow? re)	; then it's Scheme code.
	       (re-string? re))))) 
               

;;; Two useful standard char sets
(define nonl-chars (char-set-complement (char-set #\newline)))
(define word-chars (char-set-union (char-set #\_) char-set:letter+digit))

;;; Little utility that should be moved to scsh's utilities.scm
(define (partition pred lis)
  (let recur ((in '()) (out '()) (lis lis))
    (if (pair? lis)
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (recur (cons head in) out             tail)
	      (recur in             (cons head out) tail)))
	(values in out))))


(define (sre->regexp sre)
  (parse-sre sre (lambda (x) x) equal?))


;;; Parse a sexp regexp into a regexp value, which may be "dynamic" --
;;; i.e., some slots may be filled with the Scheme code that will produce
;;; their true vaues.
;;;
;;; R & C are rename and compare functions for low-level macro expanders.

;;; These two guys are little front-ends for the main routine.

(define (parse-sre sre r c) (parse-sre/context sre #t #f r c))

(define (parse-sres sres r c)
  (re-seq (map (lambda (sre) (parse-sre sre r c)) sres)))


;;; (parse-sre/context sre case-sensitive? cset? r c)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the main entry point. Parse SRE, given the lexical case-sensitivity
;;; flag CASE-SENSITIVE?. If CSET? is true, SRE *must* be parseable as a
;;; char-class SRE, and this function returns a character set, *not* a
;;; regexp value. If CSET? is false, SRE can be any SRE, and this function
;;; returns a regexp value. R and C are low-level macro rename and compare
;;; functions.

(define (parse-sre/context sre case-sensitive? cset? r c)
  (let ((%bos (r 'bos))		(%eos (r 'eos))
	(%bol (r 'bol))		(%eol (r 'eol))
	(%bow (r 'bow))		(%eow (r 'eow))

	(%word (r 'word))

	(%flush-submatches       (r 'flush-submatches))
	(%coerce-dynamic-charset (r 'coerce-dynamic-charset))
	(%coerce-dynamic-regexp  (r 'coerce-dynamic-regexp)))

    (let recur ((sre             sre)
		(case-sensitive? case-sensitive?)
		(cset?           cset?))

      ;; Parse the sequence of regexp expressions SEQ with a lexical
      ;; case-sensitivity context of CS?.
      (define (parse-seq/context seq cs?)
	(if cset?
	    (if (= 1 (length seq))
		(recur (car sre) cs? #t)
		(error "Non-singleton sequence not allowed in char-class context."
		       seq))
	    (re-seq (map (lambda (sre) (recur sre cs? cset?))
			 seq))))
	  
      (define (parse-seq seq) (parse-seq/context seq case-sensitive?))
      (define (parse-char-class sre) (recur sre case-sensitive? #t))

      (define (non-cset)	; Blow up if cset? is true.
	(if cset? (error "Illegal SRE in char-class context." sre)))

      (? ((char? sre)   (parse-char-re   sre case-sensitive? cset?))
	 ((string? sre) (parse-string-re sre case-sensitive? cset?))

	 ((c sre %bos) (non-cset) re-bos)
	 ((c sre %eos) (non-cset) re-eos)

	 ((c sre %bol) (non-cset) re-bol)
	 ((c sre %eol) (non-cset) re-eol)

	 ((c sre %bow)  (non-cset) re-bow)
	 ((c sre %eow)  (non-cset) re-eow)
	 ((c sre %word) (non-cset) re-word)

	 ((pair? sre)
	  (case (car sre)
	    ((*)  (non-cset) (re-repeat 0 #f (parse-seq (cdr sre))))
	    ((+)  (non-cset) (re-repeat 1 #f (parse-seq (cdr sre))))
	    ((?)  (non-cset) (re-repeat 0  1 (parse-seq (cdr sre))))
	    ((=)  (non-cset) (let ((n (cadr sre)))
			       (re-repeat n n (parse-seq (cddr sre)))))
	    ((>=) (non-cset) (re-repeat (cadr sre) #f (parse-seq (cddr sre))))
	    ((**) (non-cset) (re-repeat (cadr sre) (caddr sre)
					(parse-seq (cdddr sre))))

	    ;; Choice is special wrt cset? because it's "polymorphic".
	    ;; Note that RE-CHOICE guarantees to construct a char-set
	    ;; or single-char string regexp if all of its args are char 
	    ;; classes.
	    ((| or) (let ((elts (map (lambda (sre)
				       (recur sre case-sensitive? cset?))
				     (cdr sre))))
		      (if cset?
			  (assoc-cset-op char-set-union 'char-set-union elts r)
			  (re-choice elts))))

	    ((: seq) (non-cset) (parse-seq (cdr sre)))

	    ((word)  (non-cset) (parse-seq `(,%bow ,@(cdr sre) ,%eow)))
	    ((word+)
	     (recur `(,(r 'word) (,(r '+) (,(r '&) (,(r '|) ,(r 'alphanum) "_")
						   (,(r '|) . ,(cdr sre)))))
		    case-sensitive?
		    cset?))
	    
	    ((submatch) (non-cset) (re-submatch (parse-seq (cdr sre))))
	    ((dsm)      (non-cset) (re-dsm (parse-seq (cdddr sre))
					   (cadr sre)
					   (caddr sre)))

	    ;; We could be more aggressive and push the uncase op down into
	    ;; partially-static regexps, but enough is enough.
	    ((uncase)
	     (let ((re-or-cset (parse-seq (cdr sre))))	; Depending on CSET?.
	       (if cset?

		   (if (re-char-set? re-or-cset)	; A char set or code
		       (uncase-char-set re-or-cset)	; producing a char set.
		       `(,(r 'uncase) ,re-or-cset))

		   (if (static-regexp? re-or-cset)	; A regexp or code
		       (uncase re-or-cset)		; producing a regexp.
		       `(,(r 'uncase)
			 ,(regexp->scheme (simplify-regexp re-or-cset) r))))))

	    ;; These just change the lexical case-sensitivity context.
	    ((w/nocase) (parse-seq/context (cdr sre) #f))
	    ((w/case)   (parse-seq/context (cdr sre) #t))

	    ;; ,<exp> and ,@<exp>
	    ((unquote)
	     (let ((exp (cadr sre)))
	       (if cset?
		   `(,%coerce-dynamic-charset ,exp)
		   `(,%flush-submatches (,%coerce-dynamic-regexp ,exp)))))
	    ((unquote-splicing)
	     (let ((exp (cadr sre)))
	       (if cset?
		   `(,%coerce-dynamic-charset ,exp)
		   `(,%coerce-dynamic-regexp ,exp))))

	    ((~) (let* ((cs (assoc-cset-op char-set-union 'char-set-union
					   (map parse-char-class (cdr sre))
					   r))
			(cs (if (char-set? cs)
				(char-set-complement cs)
				`(,(r 'char-set-complement) ,cs))))
		   (if cset? cs (make-re-char-set cs))))

	    ((&) (let ((cs (assoc-cset-op char-set-intersection 'char-set-intersection
					  (map parse-char-class (cdr sre))
					  r)))
		   (if cset? cs (make-re-char-set cs))))

	    ((-) (if (pair? (cdr sre))
		     (let* ((cs1 (parse-char-class (cadr sre)))
			    (cs2 (assoc-cset-op char-set-union 'char-set-union
						(map parse-char-class (cddr sre))
						r))
			    (cs (if (and (char-set? cs1) (char-set? cs2))
				    (char-set-difference cs1 cs2)
				    `(,(r 'char-set-difference)
				      ,(if (char-set? cs1)
					   (char-set->scheme cs1 r)
					   cs1)
				      . ,(if (char-set? cs2)
					     (list (char-set->scheme cs2 r))
					     (cdr cs2))))))
		       (if cset? cs (make-re-char-set cs)))
		     (error "SRE set-difference operator (- ...) requires at least one argument")))

	    ((/) (let ((cset (range-class->char-set (cdr sre) case-sensitive?)))
		   (if cset? cset (make-re-char-set cset))))

	    ((posix-string)
	     (if (and (= 1 (length (cdr sre)))
		      (string? (cadr sre)))
		 (posix-string->regexp (cadr sre))
		 (error "Illegal (posix-string ...) SRE body." sre)))

	    (else (if (every string? sre)	; A set spec -- ("wxyz").
		      (let* ((cs (apply char-set-union
					(map string->char-set sre)))
			     (cs (if case-sensitive? cs (uncase-char-set cs))))
			(if cset? cs (make-re-char-set cs)))

		      (error "Illegal SRE" sre)))))

	 ;; It must be a char-class name (ANY, ALPHABETIC, etc.)
	 (else (let ((cs (case sre
			   ((any)			char-set:full)
			   ((nonl)			nonl-chars)
			   ((lower-case lower)		char-set:lower-case)
			   ((upper-case upper)		char-set:upper-case)
			   ((alphabetic alpha)		char-set:letter)
			   ((numeric digit num)	char-set:digit)
			   ((alphanumeric alnum alphanum) char-set:letter+digit)
			   ((punctuation punct)	char-set:punctuation)
			   ((graphic graph)		char-set:graphic)
			   ((blank)			char-set:blank)
			   ((whitespace space white)	char-set:whitespace)
			   ((printing print)		char-set:printing)
			   ((control cntrl)		char-set:iso-control)
			   ((hex-digit xdigit hex)	char-set:hex-digit)
			   ((ascii)			char-set:ascii)
			   (else (error "Illegal regular expression" sre)))))
		 (if cset? cs (make-re-char-set cs))))))))


;;; In a CSET? true context, S must be a 1-char string; convert to a char set
;;;     according to CASE-SENSITIVE? setting.
;;; In a CSET? false context, convert S to a string re (CASE-SENSITIVE? true),
;;;     or a sequence of char-sets (CASE-SENSITIVE? false).

(define (parse-string-re s case-sensitive? cset?)
  (if (= 1 (string-length s))
      (parse-char-re (string-ref s 0) case-sensitive? cset?)
      (if cset?
	  (error "Non-singleton string not allowed in char-class context." s)
	  ((if case-sensitive? make-re-string uncase-string) s))))

(define (parse-char-re c case-sensitive? cset?)
  (if case-sensitive?
      (if cset? (char-set c) (make-re-string (string c)))
      (let ((cset (char-set (char-upcase c) (char-downcase c))))
	(if cset? cset (make-re-char-set cset)))))


;;; "Apply" the associative char-set function OP to the char-sets ELTS.
;;; If any of the ELTS is Scheme code instead of a real char set, then
;;; we instead produce Scheme code for the op, using OP-NAME as the name
;;; of the function, and R for the macro renamer function.

(define (assoc-cset-op op op-name elts r)
  (receive (csets code-chunks) (partition char-set? elts)
    (if (pair? code-chunks)
	(? ((pair? csets)
	    `(,(r op-name) ,(char-set->scheme (apply op csets) r)
			   . ,code-chunks))
	   ((pair? (cdr code-chunks)) `(,(r op-name) . ,code-chunks))
	   (else (car code-chunks))) ; Just one.
	(apply op csets))))

;;; Parse a (/ <range-spec> ...) char-class into a character set in
;;; case-sensitivity context CS?.
;;; Each <range-spec> can be a character or a string of characters.

(define (range-class->char-set range-specs cs?)
  (let* ((specs (apply string-append
		       (map (lambda (spec) (if (char? spec) (string spec) spec))
			    range-specs)))
	 (len (string-length specs))
	 (cset (char-set-copy char-set:empty)))
    (if (odd? len)
	(error "Unmatched range specifier" range-specs)
	(let lp ((i (- len 1)) (cset cset))
	  (if (< i 0)
	      (if cs? cset (uncase-char-set cset)) ; Case fold if necessary.
	      (lp (- i 2)
		  (ucs-range->char-set! (char->ascii (string-ref specs (- i 1)))
					(+ 1 (char->ascii (string-ref specs i)))
					#f cset)))))))

;;; (regexp->scheme re r)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translate a regexp value RE into raw Scheme code that will create it, with
;;; calls to the regexp ADT constructor functions. R is a renaming function
;;; provided by low-level macro expanders.

(define (regexp->scheme re r)
  (let ((%re-bos (r 're-bos))	(%re-eos (r 're-eos))
	(%re-bol (r 're-bol))	(%re-eol (r 're-eol))
	(%re-bow (r 're-bow))	(%re-eow (r 're-eow))
	(%list   (r 'list)))

  (let recur ((re re))
    ;; If (fetch-posix re) = #f, produce (OP . ARGS);
    ;; Otherwise, produce (OP/POSIX ,@ARGS '<posix-translation>).
    (define (doit op op/posix args fetch-posix)
      (? ((fetch-posix re) =>
	  (lambda (psx) `(,(r op/posix) ,@args
					',(cre:string psx) ',(cre:tvec psx))))
					
	 (else `(,(r op) . ,args))))

    (? ((re-string? re)   (if (re-trivial? re) (r 're-trivial) ; Special hack
			      (doit 'make-re-string 'make-re-string/posix
				    `(,(re-string:chars re))
				    re-string:posix)))

       ((re-seq? re)      (doit '%make-re-seq '%make-re-seq/posix
				`((,%list . ,(map recur (re-seq:elts re)))
				  ,(re-seq:tsm re))
				re-seq:posix))

       ((re-choice? re)   (doit '%make-re-choice '%make-re-choice/posix
				`((,%list . ,(map recur (re-choice:elts re)))
				  ,(re-choice:tsm re))
				re-choice:posix))

       ((re-char-set? re) (if (re-any? re) (r 're-any) ; Special hack for ANY.
			      (doit 'make-re-char-set 'make-re-char-set/posix
				    `(,(char-set->scheme (re-char-set:cset re) r))
				    re-char-set:posix)))

       ((re-repeat? re)   (doit '%make-re-repeat '%make-re-repeat/posix
				`(,(re-repeat:from re)
				  ,(re-repeat:to re)
				  ,(recur (re-repeat:body re))
				  ,(re-repeat:tsm re))
				re-repeat:posix))

       ((re-dsm? re)      (doit '%make-re-dsm '%make-re-dsm/posix
				`(,(recur (re-dsm:body re))
				  ,(re-dsm:pre-dsm  re)
				  ,(re-dsm:tsm re))
				re-dsm:posix))

       ((re-submatch? re) (doit '%make-re-submatch '%make-re-submatch/posix
				`(,(recur (re-submatch:body re))
				  ,(re-submatch:pre-dsm re)
				  ,(re-submatch:tsm re))
				re-submatch:posix))

       ((re-bos? re) %re-bos)
       ((re-eos? re) %re-eos)
       ((re-bol? re) %re-bol)
       ((re-eol? re) %re-eol)
       ((re-bow? re) %re-bow)
       ((re-eow? re) %re-eow)

       (else re)))))



;;; Classify a character set.
;;; We pass in a char set CS and 15 parameters, one for each of the
;;; standard char sets. If we can classify CS as any of these char
;;; sets, we return the corresponding parameter's value, otw #f.
;;;
;;; This is gratuitously optimised by probing cset with a couple of
;;; witness chars (a,A,1,space), and doing an initial filter based
;;; on these witnesses.

(define (try-classify-char-set cs
			       full nonl lower upper alpha num alphanum
			       punct graph white print ctl hex blank ascii)
  (let ((a     (char-set-contains? cs #\a))
	(biga  (char-set-contains? cs #\A))
	(one   (char-set-contains? cs #\1))
	(space (char-set-contains? cs #\space)))

    (if a
	(if biga
	    (if space 
		(and one (switch char-set= cs
			   ((char-set:full)		full)
			   ((nonl-chars)		nonl)
			   ((char-set:printing)		print)
			   ((char-set:ascii)		ascii)
			   (else #f)))
		(if one
		    (switch char-set= cs
		      ((char-set:letter+digit)	alphanum)
		      ((char-set:graphic)	graph)
		      ((char-set:hex-digit)	hex)
		      (else #f))
		    (and (char-set= cs char-set:letter) alpha)))
	    (and (char-set= cs char-set:lower-case) lower)) ; a, not A

	(if biga
	    (and (not space) (char-set= cs char-set:upper-case) upper)
	    (if one
		(and (not space) (char-set= cs char-set:digit) num)
		(if space
		    (switch char-set= cs
		      ((char-set:whitespace) white)
		      ((char-set:blank)      blank)
		      (else #f))
		    (switch char-set= cs
		      ((char-set:punctuation)	punct)
		      ((char-set:iso-control)	ctl)
		      (else #f))))))))
		

(define (char-set->scheme cs r)
  (let ((try (lambda (cs)
	       (try-classify-char-set cs
				      'char-set:full         'nonl-chars
				      'char-set:lower-case   'char-set:upper-case
				      'char-set:letter       'char-set:digit
				      'char-set:letter+digit 'char-set:punctuation
				      'char-set:graphic      'char-set:whitespace
				      'char-set:printing     'char-set:iso-control
				      'char-set:hex-digit    'char-set:blank
				      'char-set:ascii))))
    (? ((not (char-set? cs)) cs) ; Dynamic -- *already* Scheme code.
       ((char-set-empty? cs) (r 'char-set:empty))
       ((try cs) => r)
       ((try (char-set-complement cs)) =>
	(lambda (name) `(,(r 'char-set-complement) ,name)))

       (else
	(receive (loose+ ranges+) (char-set->in-pair cs)
	  (receive (loose- ranges-) (char-set->in-pair (char-set-complement cs))
	    (let ((makeit (r 'spec->char-set)))
	      (if (< (+ (length loose-) (* 12 (length ranges-)))
		     (+ (length loose+) (* 12 (length ranges+))))
		  `(,makeit #f ,(list->string loose-) ',ranges-)
		  `(,makeit #t ,(list->string loose+) ',ranges+)))))))))



;;; This code needs work.

(define (char-set->sre cs r)
  (if (char-set? cs)
      (let ((try (lambda (cs)
		   (try-classify-char-set cs
					  'any          'nonl
					  'lower-case   'upper-case
					  'alphabetic   'numeric
					  'alphanumeric 'punctuation
					  'graphic      'whitespace
					  'printing     'control
					  'hex-digit    'blank
					  'ascii)))
	    (nchars (char-set-size cs)))
	(? ((zero? nchars) `(,(r '|)))
	   ((= 1 nchars) (apply string (char-set->list cs)))
	   ((try cs) => r)
	   ((try (char-set-complement cs)) =>
	    (lambda (name) `(,(r '~) ,name)))
	   (else (receive (cs rp comp?) (char-set->in-sexp-spec cs)
		   (let ((args (append (? ((string=? cs "") '())
					  ((= 1 (string-length cs)) `(,cs))
					  (else `((,cs))))
				       (if (string=? rp "") '()
					   (list `(,(r '/) ,rp))))))
		     (if (and (= 1 (length args)) (not comp?))
			 (car args)
			 `(,(r (if comp? '~ '|)) . ,args)))))))

      `(,(r 'unquote) ,cs))) ; dynamic -- ,<cset-exp>


;;; Unparse an re into a *list* of SREs (representing a sequence).
;;; This is for rendering the bodies of DSM, SUBMATCH, **, *, =, >=, and &'s,
;;; that is, forms whose body is an implicit sequence.

(define (regexp->sres/renamer re r)
    (if (re-seq? re)
	(let ((elts (re-seq:elts re)))
	  (if (pair? elts)
	      (map (lambda (re) (regexp->sre/renamer re r)) elts)
	      (let ((tsm  (re-seq:tsm  re))
		    (%dsm (r 'dsm)))
		(if (zero? tsm) '() `((,%dsm ,tsm 0))))))   ; Empty sequence
	(list (regexp->sre/renamer re r))))		    ; Not a seq


(define (regexp->sre/renamer re r)
  (let recur ((re re))
    (? ((re-string? re) (re-string:chars re))

       ((re-seq? re)    `(,(r ':) . ,(regexp->sres/renamer re r)))
	 
       ((re-choice? re)
	(let ((elts (re-choice:elts re))
	      (%| (r '|)))
	  (if (pair? elts)
	      `(,%| . ,(map recur elts))
	      (let ((tsm  (re-choice:tsm  re)))
		(if (zero? tsm) `(,%|) `(,(r 'dsm) ,tsm 0 (,%|)))))))

       ((re-char-set? re) (char-set->sre (re-char-set:cset re) r))

       ((re-repeat? re)
	(let ((from (re-repeat:from re))
	      (to (re-repeat:to re))
	      (bodies (regexp->sres/renamer (re-repeat:body re) r)))
	  (? ((and (eqv? from 0) (not to))    `(,(r '*) . ,bodies))
	     ((and (eqv? from 0) (eqv? to 1)) `(,(r '?) . ,bodies))
	     ((and (eqv? from 1) (not to))    `(,(r '+) . ,bodies))
	     ((eqv? from to)		      `(,(r '=) ,to . bodies))
	     (to                           `(,(r '**) ,from ,to . ,bodies))
	     (else                         `(,(r '>=) ,from . ,bodies)))))

       ((re-dsm? re)
	`(,(r 'dsm) ,(re-dsm:pre-dsm re) ,(re-dsm:post-dsm re)
		    . ,(regexp->sres/renamer (re-dsm:body re) r)))

       ((re-submatch? re)
	`(,(r 'submatch) . ,(regexp->sres/renamer (re-submatch:body re) r)))

       ((re-bos? re) (r 'bos))
       ((re-eos? re) (r 'eos))
       ((re-bol? re) (r 'bol))
       ((re-eol? re) (r 'eol))
       ((re-bow? re) (r 'bow))
       ((re-eow? re) (r 'eow))

       (else re))))			; Presumably it's code.

(define (regexp->sre re) (regexp->sre/renamer re (lambda (x) x)))

;;; Character class unparsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the code that takes char-sets and converts them into forms suitable
;;; for char-class SRE's or [...] Posix strings.

;;; Map a char-set to an (| ("...") (/"...")) or (~ ("...") (/"...")) SRE.
;;; We try it both ways, and return whichever is shortest.
;;; We return three values: 
;;; - a string of chars that are members in the set;
;;; - a string of chars that, taken in pairs specifying ranges,
;;;   give the rest of the members of the set.
;;; - A boolean COMP?, which says whether the set should be complemented
;;;   (~ ...) or taken as-is (| ...).
;;;
;;; E.g., ["!?.", "AZaz09", #t]

(define (char-set->in-sexp-spec cset)
  (let ((->sexp-pair (lambda (cset)
		       (receive (loose ranges) (char-set->in-pair cset)
			 (values (apply string loose)
				 (apply string
					(fold-right (lambda (r lis)
						      `(,(car r) ,(cdr r) . ,lis))
						    '() ranges)))))))
    (receive (cs+ rp+) (->sexp-pair cset)
      (receive (cs- rp-) (->sexp-pair (char-set-complement cset))
	(if (< (+ (string-length cs-) (string-length rp-))
	       (+ (string-length cs+) (string-length rp+)))
	    (values cs- rp- #t)
	    (values cs+ rp+ #f))))))

;;; Return 2 values characterizing the char set in a run-length encoding:
;;; - LOOSE		List of singleton chars -- elts of the set.
;;; - RANGES		List of (from . to) char ranges.
;;;
;;; E.g., [(#\! #\? #\.) 
;;;        ((#\A . #\Z) (#\a . #\z) (#\0 . #\9))]

(define (char-set->in-pair cset)
  (let ((add-range (lambda (from to loose ranges)
		     (if from (case (- to from)
				((0) (values (cons (ascii->char from) loose)
					     ranges))
				((1) (values `(,(ascii->char from)
					       ,(ascii->char to)
					       . ,loose)
					     ranges))
				((2) (values `(,(ascii->char from)
					       ,(ascii->char (+ from 1))
					       ,(ascii->char to)
					       . ,loose)
					     ranges))
				(else (values loose
					      `((,(ascii->char from) .
						 ,(ascii->char to))
						. ,ranges))))
			 (values loose ranges)))))

    (let lp ((i 127) (from #f) (to #f) (loose '()) (ranges '()))
      (if (< i 0)
	  (add-range from to loose ranges)

	  (let ((i-1 (- i 1)))
	    (if (char-set-contains? cset (ascii->char i))
		(if from
		    (lp i-1 i to loose ranges)	; Continue the run.
		    (lp i-1 i i  loose ranges))	; Start a new run.

		;; If there's a run going, finish it off.
		(receive (loose ranges) (add-range from to loose ranges)
		  (lp i-1 #f #f loose ranges))))))))
