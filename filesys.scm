;;; Ops that create objects in the file system:
;;; create-{directory,fifo,hard-link,symlink}
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

(define-module (scsh filesys)
  :use-module (ice-9 stack-catch)
  :use-module (scsh scsh-condition)
  :use-module (scsh syscalls)
  :use-module (scsh let-opt)
  :use-module (scsh fileinfo)
  :use-module (scsh errno)
  :use-module (scsh scsh)
)

(begin-deprecated
 ;; Prevent `export' from re-exporting core bindings.  This behaviour
 ;; of `export' is deprecated and will disappear in one of the next
 ;; releases.
 (define rename-file #f))

(export delete-filesys-object create-directory create-fifo create-hard-link
	create-symlink rename-file)

;;; This procedure nukes FNAME, whatever it may be: directory, file, fifo,
;;; symlink.
;;;
;;; We can't probe FNAME to find out what it is and then do the right
;;; delete operation because there's a window in-between the probe and the
;;; delete where the file system can be altered -- the probe and delete
;;; aren't atomic. In order to deliver on our contract, we have to spin
;;; in a funny loop until we win. In practice, the loop will probably never
;;; execute more than once.

(define (delete-filesys-object fname)
  (let loop ()
    (or (with-errno-handler ; Assume it's a file and try.
	    ((err data)
	     ((errno/isdir) #f) ; Return #f if directory
	     ((errno/perm) #f)
	     ((errno/noent) #t))
	    (delete-file fname)
	    #t)

	(with-errno-handler ; Assume it's a directory and try.
	    ((err data)
	     ((errno/notdir) #f) ; Return #f if fname is not a directory.
	     ((errno/noent) #t))
	    (delete-directory fname)
	    #t)

	(loop)))) ; Strange things are happening. Try again.


;;; For similar reasons, all of these ops must loop.

;;; Abstract out common code for create-{directory,fifo,hard-link,symlink}:

(define (create-file-thing fname makeit override? op-name syscall)
  (let ((query (lambda ()
		 (y-or-n? (string-append op-name ": " fname
					 " already exists. Delete")))))
    (let loop ((override? override?))
      (stack-catch 'system-error
		   (lambda () (makeit fname))
		   (lambda (tag proc msg msg-args rest)
		     (let ((errno (car rest)))
		       (if (= errno errno/exist)
			   ;; FNAME exists. Nuke it and retry?
			   (cond ((if (eq? override? 'query)
				      (query)
				      override?)
				  (delete-filesys-object fname)
				  (loop #t))
				 (else
				  (throw tag proc msg msg-args rest)))
			   (throw tag proc msg msg-args rest))))))))

;;;;;;;

(define (create-directory dir . rest)
  (let ((perms (if (null? rest) #o777 (car rest)))
	(override? (if (or (null? rest) (null? (cdr rest))) #f
		       (cadr rest))))
    (create-file-thing dir
		       (lambda (dir) (mkdir dir perms))
		       override?
		       "create-directory"
		       create-directory)))

(define (create-fifo fifo . rest)
  (let ((perms (if (null? rest) #o777 (car rest)))
	(override? (if (or (null? rest) (null? (cdr rest))) #f
		       (cadr rest))))
    (create-file-thing fifo
		       (lambda (fifo) (mknod fifo 'fifo perms 0))
		       override?
		       "create-fifo"
		       create-fifo)))

(define (create-hard-link old-fname new-fname . maybe-override?)
  (create-file-thing new-fname
		     (lambda (new-fname)
		       (link old-fname new-fname))
		     (:optional maybe-override? #f)
		     "create-hard-link"
		     create-hard-link))

(define (create-symlink old-fname new-fname . maybe-override?)
  (create-file-thing new-fname
		     (lambda (new-fname)
		       (symlink old-fname new-fname))
		     (:optional maybe-override? #f)
		     "create-symlink"
		     create-symlink))

;;; Unix rename() works backwards from mkdir(), mkfifo(), link(), and 
;;; symlink() -- it overrides by default, (though insisting on a type
;;; match between the old and new object). So we can't use create-file-thing.
;;; Note that this loop has a tiny atomicity problem -- if someone
;;; creates a file *after* we do our existence check, but *before* we 
;;; do the rename, we could end up overriding it, when the user asked
;;; us not to. That's life in the food chain.

(define guile-rename-file
  (module-ref (resolve-module '(guile)) 'rename-file))

(define (rename-file old-fname new-fname . maybe-override?)
  (let ((override? (:optional maybe-override? #f)))
    (if (or (and override? (not (eq? override? 'query)))
	    (file-not-exists? new-fname)
	    (and override?
		 (y-or-n? (string-append "rename-file:" new-fname
					 " already exists. Delete"))))
	(guile-rename-file old-fname new-fname))))
