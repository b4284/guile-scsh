;;; Networking for the Scheme Shell
;;; Copyright (c) 1994-1995 by Brian D. Carlstrom.
;;; Copyright (c) 1994 by Olin Shivers.
;;; See file COPYING.

(define-module (scsh network)
  :use-module (ice-9 receive)
  :use-module (scsh define-foreign-syntax)
  :use-module (scsh netconst)
  :use-module (scsh let-opt)
  :use-module (scsh errno)
  :use-module (scsh rw)
  :use-module (scsh defrec)
)
(export socket-connect bind-listen-accept-loop
	socket? socket:family socket:inport socket:outport
	socket-address? socket-address:address socket-address:family
	internet-address->socket-address socket-address->internet-address
	create-socket close-socket bind-socket connect-socket
	listen-socket accept-connection socket-remote-address
	socket-local-address shutdown-socket create-socket-pair
	receive-message receive-message!
	receive-message/partial receive-message!/partial
	send-message send-message/partial
	socket-option set-socket-option
	host-info host-info?
	host-info:name host-info:aliases host-info:addresses
	network-info network-info?
	network-info:name network-info:aliases network-info:net
	service-info service-info?
	service-info:name service-info:aliases service-info:port
	service-info:protocol
	protocol-info protocol-info?
	protocol-info:name protocol-info:aliases protocol-info:number
	host-to-net-16 net-to-host-16 host-to-net-32 net-to-host-32
)

;; noop for Guile.
(defmacro define-errno-syscall args #f)

;;; Scheme48 implementation.

(foreign-source
 "#include <sys/types.h>"
 "#include <sys/socket.h>"
 "#include <errno.h>"
 ""
 "/* Make sure foreign-function stubs interface to the C funs correctly: */"
 "#include \"network1.h\""
 ""
 "extern int h_errno;"
 ""
 "#define errno_on_zero_or_false(x) ((x) ? SCHFALSE : ENTER_FIXNUM(errno))"
 "#define errno_or_false(x) (((x) == -1) ? ENTER_FIXNUM(errno) : SCHFALSE)"
 "#define False_on_zero(x) ((x) ? ENTER_FIXNUM(x) : SCHFALSE)"
 "" )

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; High Level Prototypes
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (socket-connect protocol-family socket-type . args)
  (let* ((sock (create-socket protocol-family
			      socket-type))
	 (addr (cond ((= protocol-family
			 protocol-family/internet)
		      (let* ((host (car  args))
			     (port (cadr args))
			     (host (car (host-info:addresses 
					 (name->host-info host))))
			     (port (cond ((integer? port) port)
					 ((string? port)
					  (service-info:port 
					   (service-info (cadr args) "tcp")))
					 (else
					  (error
					   "socket-connect: bad arg ~s"
					   args)))))
			(internet-address->socket-address host port)))
		     ((= protocol-family
			 protocol-family/unix)
		      (unix-address->socket-address (car args)))
		     (else 
		      (error "socket-connect: unsupported protocol-family ~s"
			     protocol-family)))))
    ;; Close the socket and free the file-descriptors
    ;; if the connect fails:
    (let ((connected #f))
      (dynamic-wind
       (lambda () #f)
       (lambda () (connect-socket sock addr) (set! connected #t))
       (lambda ()
         (if (not connected)
             (close-socket sock))))
      (if connected
          sock
          #f))))

(define (bind-listen-accept-loop protocol-family proc arg)
  (let ((sock (create-socket protocol-family socket-type/stream))
	(addr (cond ((= protocol-family protocol-family/internet)
		     (internet-address->socket-address internet-address/any
		       (cond ((integer? arg) arg)
			     ((string? arg)
			      (service-info:port (service-info arg "tcp")))
			     (else (error "socket-connect: bad arg ~s" arg)))))

		    ((= protocol-family protocol-family/unix)
		     (unix-address->socket-address arg))

		    (else
		     (error "bind-listen-accept-loop: unsupported protocol-family ~s"
			    protocol-family)))))

    (set-socket-option sock level/socket socket/reuse-address #t)
    (bind-socket sock addr)
    (listen-socket sock 5)
    (let loop ()
      (call-with-values (lambda () (accept-connection sock)) proc)
      (loop))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Socket Record Structure
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record socket
  family				; protocol family
  inport				; input port 
  outport)				; output port

(define-record socket-address
  family				; address family
  address)				; address

;;; returns the port of a socket
;;; not exported
(define (socket->port sock)
  (socket:inport sock))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Socket Address Routines
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (internet-address->socket-address address32 port16)
  (cond ((not (<= 0 address32 #xffffffff))
	 (error "internet-address->socket-address: address out of range ~s"
		address32))
	((not (<= 0 port16 #xffff))
	 (error "internet-address->socket-address: port out of range ~s"
		port16))
	(else 
	 (make-socket-address address-family/internet
			      (cons address32 port16)))))
  
(define (socket-address->internet-address sockaddr)
  (if (or (not (socket-address? sockaddr))
	  (not (= (socket-address:family sockaddr) 
		  address-family/internet)))
      (error "socket-address->internet-address: internet socket expected ~s"
	     sockaddr)
      (values (car (socket-address:address sockaddr))
	      (cdr (socket-address:address sockaddr)))))

(define (unix-address->socket-address path)
  (if (> (string-length path) 108)
      (error "unix-address->socket-address: path too long ~s" path)
      (make-socket-address address-family/unix path)))

(define (socket-address->unix-address sockaddr)
  (if (or (not (socket-address? sockaddr))
	  (not (= (socket-address:family sockaddr) 
		  address-family/unix)))
      (error "socket-address->unix-address expects an unix socket ~s" sockaddr)
      (socket-address:address sockaddr)))

(define (address-vector->socket-address vec)
  (if (eq? vec #f)
      #f
      (let ((family (vector-ref vec 0)))
	(cond ((= family address-family/internet)
	       (internet-address->socket-address (vector-ref vec 1)
						 (vector-ref vec 2)))
	      ((= family address-family/unix)
	       (unix-address->socket-address (vector-ref vec 1)))
	      (else
	       (error "Unrecognised socket address type" family))))))

(define (socket-address->list addr)
  (let ((family (socket-address:family addr)))
    (cond ((= family address-family/unix)
	   (list (socket-address->unix-address addr)))
	  ((= family address-family/internet)
	   (receive (address port)
		    (socket-address->internet-address addr)
		    (list address port)))
	  (else
	   (error "Unrecognised address family: " family)))))

;(define (make-addr af)
;  (make-string (cond ((= af address-family/unix) 108)
;		     ((= af address-family/internet) 8)
;		     (else 
;		      (error "make-addr: unknown address-family ~s" af)))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socket syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (create-socket pf type . maybe-protocol)
  (let ((protocol (:optional maybe-protocol 0)))
    (if (not (and (integer? pf)
		  (integer? type)
		  (integer? protocol)))
	(error "create-socket: integer arguments expected ~s ~s ~s" 
	       pf type protocol)
	(let* ((sock  (socket pf type protocol)))
	  (make-socket pf sock sock)))))

(define-foreign %socket/errno
  (socket (integer pf)
	  (integer type)
	  (integer protocol))
  (multi-rep (to-scheme integer errno_or_false)
             integer))

(define-errno-syscall (%socket pf type protocol) %socket/errno
  sockfd)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; close syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (close-socket sock)
  (close-port (socket:inport sock)))

(define (socket-address->list address)
  (let ((family (socket-address:family address)))
    (cond ((= family address-family/unix)
	   (list family (socket-address:address address)))
	  ((= family address-family/internet)
	   (list family
		 (car (socket-address:address address))
		 (cdr (socket-address:address address))))
	  (else
	   (error 
	    "unrecognised socket family" family)))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; bind syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (bind-socket sock name)
  (cond ((not (socket? sock))
	 (error "bind-socket: socket expected ~s" sock))
	((not (socket-address? name))
	 (error "bind-socket: socket-address expected ~s" name))
	(else
	 (let ((family (socket:family sock)))
	   (if (not (= family (socket-address:family name)))
	       (error 
		"bind-socket: trying to bind incompatible address to socket ~s"
		name)
	       (apply bind 
		      (socket->port sock)
		      (socket-address->list name)))))))

(define-foreign %bind/errno
  (scheme_bind (integer     sockfd)	; socket fdes
	       (integer     family)	; address family
	       (string-desc name))	; scheme descriptor
  (to-scheme integer errno_or_false))

(define-errno-syscall (%bind sockfd family name) %bind/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; connect syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (connect-socket sock name)
  (cond ((not (socket? sock))
	 (error "connect-socket: socket expected ~s" sock))
	((not (socket-address? name))
	 (error "connect-socket: socket-address expected ~s" name))
	(else
	 (let ((family (socket:family sock)))
	   (cond ((not (= family (socket-address:family name)))
		  (error 
	   "connect: trying to connect socket to incompatible address ~s"
	   name))
		 (else
		  (apply connect (socket->port sock)
			 (socket-address->list name))))))))

(define-foreign %connect/errno
  (scheme_connect (integer sockfd)	; socket fdes
		  (integer family)	; address family
		  (desc    name))	; scheme descriptor
  (to-scheme integer errno_or_false))

(define-errno-syscall (%connect sockfd family name) %connect/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; listen syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (listen-socket sock backlog)
  (cond ((not (socket? sock))
	 (error "listen-socket: socket expected ~s" sock))
	((not (integer? backlog))
	 (error "listen-socket: integer expected ~s" backlog))
	(else
	 (listen (socket->port sock) backlog))))
	 
(define-foreign %listen/errno
  (listen (integer sockfd)	; socket fdes
	  (integer backlog))	; backlog
	no-declare ; for Linux
  (to-scheme integer errno_or_false))

(define-errno-syscall (%listen sockfd backlog) %listen/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; accept syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (accept-connection sock)
  (if (not (socket? sock))
      (error "accept-connection: socket expected ~s" sock)
      (let* ((family (socket:family sock))
	     (rv (accept (socket->port sock)))
	     (new-socket (car rv))
	     (address-vector (cdr rv)))
	(values (make-socket family new-socket new-socket)
		(address-vector->socket-address address-vector)))))

(define-foreign %accept/errno
  (scheme_accept (integer     sockfd)
		 (integer     family)
		 (string-desc name))
  (multi-rep (to-scheme integer errno_or_false)
             integer))

(define-errno-syscall (%accept sock family name) %accept/errno
  sockfd)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getpeername syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (socket-remote-address sock)
  (if (or (not (socket? sock))
	  (not (= (socket:family sock) address-family/internet)))
      (error "socket-remote-address: internet socket expected ~s" sock)
      (let* ((family (socket:family sock))
	     (peer (getpeername (socket->port sock))))
	(address-vector->socket-address peer))))

(define-foreign %peer-name/errno
  (scheme_peer_name (integer     sockfd)
		    (integer     family)
		    (string-desc name))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%peer-name sock family name) %peer-name/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getsockname syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (socket-local-address sock)
  (if (or (not (socket? sock))
	  (not (= (socket:family sock) address-family/internet)))
      (error "socket-local-address: internet socket expected ~s" sock)
      (let* ((family (socket:family sock))
	     (name (getsockname (socket->port sock))))
	(address-vector->socket-address name))))

(define-foreign %socket-name/errno
  (scheme_socket_name (integer     sockfd)
		      (integer     family)
		      (string-desc name))
  (to-scheme integer "False_on_zero"))

(define-errno-syscall 
  (%socket-name sock family name) %socket-name/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; shutdown syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (shutdown-socket sock how)
  (cond ((not (socket? sock))
	 (error "shutdown-socket: socket expected ~s" sock))
	((not (integer? how))
	 (error "shutdown-socket: integer expected ~s" how))
	(else
	 (shutdown (socket->port sock) how))))

(define-foreign %shutdown/errno
  (shutdown (integer sockfd)
	    (integer how))
  (to-scheme integer errno_or_false))

(define-errno-syscall 
  (%shutdown sock how) %shutdown/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socketpair syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (create-socket-pair type)
  (if (not (integer? type))
      (error "create-socket-pair: integer argument expected ~s" type)
        (let* ((pair (socketpair protocol-family/unix type 0)))
	  (values (make-socket protocol-family/unix (car pair) (car pair))
		  (make-socket protocol-family/unix (cdr pair) (cdr pair))))))

;; based on pipe in syscalls.scm
(define-foreign %socket-pair/errno
  (scheme_socket_pair (integer type))
  (to-scheme integer errno_or_false)
  integer
  integer)

(define-errno-syscall 
  (%socket-pair type) %socket-pair/errno
  sockfd1
  sockfd2)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; recv syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (receive-message socket len . maybe-flags) 
  (let ((flags (:optional maybe-flags 0)))
    (cond ((not (socket? socket))
	   (error "receive-message: socket expected ~s" socket))
	  ((or (not (integer? flags))
	       (not (integer? len)))
	   (error "receive-message: integer expected ~s ~s" flags len))
	  (else 
	   (let ((s (make-string len)))
	     (receive (nread from)
		      (receive-message! socket s 0 len flags)
               (values
		(cond ((not nread) #f)	; EOF
		      ((= nread len) s)
		      (else (substring s 0 nread)))
		from)))))))

(define (receive-message! socket s . args)
  (if (not (string? s))
      (error "receive-message!: string expected ~s" s)
      (let-optionals args ((start 0) (end (string-length s)) (flags 0))
        (cond ((not (socket? socket))
	       (error "receive-message!: socket expected ~s" socket))
	      ((not (or (integer? flags)
			(integer? start)
			(integer? end)))
	       (error "receive-message!: integer expected ~s ~s ~s"
		      flags start end))
	      (else 
	       (generic-receive-message! (socket->port socket) flags
					 s start end 
					 'dummy
					 (socket:family socket)))))))

(define (generic-receive-message! sockfd flags s start end reader from)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" s start end))
  (let loop ((i start)
	     (addr #f))
    (if (>= i end) (values (- i start)
			   (make-socket-address from addr))
	(let* ((rv (recvfrom! sockfd s flags i end))
	       (nread (car rv))
	       (addr (cdr rv)))
	  (cond
	   ((zero? nread)	; EOF
	    (values
	     (let ((result (- i start)))
	       (and (not (zero? result)) result))
	     (make-socket-address from addr)))
	   (else (loop (+ i nread) addr)))))))

(define (receive-message/partial socket len . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (cond ((not (socket? socket))
	   (error "receive-message/partial: socket expected ~s" socket))
	  ((or (not (integer? flags))
	       (not (integer? len)))
	   (error "receive-message/partial: integer expected ~s ~s" flags len))
	  (else 
	   (let ((s (make-string len)))
	     (receive (nread addr)
		      (receive-message!/partial socket s 0 len flags)
		      (values 
		       (cond ((not nread) #f)	; EOF
			     ((= nread len) s)
			     (else (substring s 0 nread)))
		       addr)))))))

(define (receive-message!/partial socket s . args)
  (if (not (string? s))
      (error "receive-message!/partial: string expected ~s" s)
      (let-optionals args ((start 0) (end (string-length s)) (flags 0))
        (cond ((not (socket? socket))
	       (error "receive-message!/partial: socket expected ~s"
		      socket))
	      ((not (integer? flags))
	       (error "receive-message!/partial: integer expected ~s"
		      flags))
	      (else 
	       (generic-receive-message!/partial (socket->port socket)
						 flags 
						 s start end
						 'dummy
						 (socket:family socket)))))))

(define (generic-receive-message!/partial sockfd flags s start end reader from)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" reader s start end))

  (if (= start end) 0 ; Vacuous request.
      (catch 'system-error
	     (lambda ()
	       (let* ((rv (recvfrom! sockfd s flags start end))
		      (nread (car rv))
		      (addr (cdr rv)))
		 (values (and (not (zero? nread)) nread)
			 (make-socket-address from addr))))
	     (lambda args
	       (let ((err (car (list-ref args 4))))
		 (cond ;; ((= err errno/intr) (loop)) ; handled by primitive.
		  ((or (= err errno/wouldblock); No forward-progess here.
		       (= err errno/again))
		   0)
		  (else (apply scm-error args))))))))


(define-foreign recv-substring!/errno
  (recv_substring (integer sockfd)
		  (integer flags)
		  (string-desc buf)
		  (integer start)
		  (integer end)
		  (string-desc name))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; send syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (send-message socket s . args)
  (let-optionals args ((start 0) (end (string-length s)) (flags 0) (addr #f))
    (cond ((not (socket? socket))
	   (error "send-message: socket expected ~s" socket))
	  ((not (integer? flags))
	   (error "send-message: integer expected ~s" flags))
	  ((not (string? s))
	   (error "send-message: string expected ~s" s))
	  (else 
	   (generic-send-message (socket->port socket) flags
				 s start end
				 'dummy
				 (if addr (socket-address:family addr) 0)
				 (and addr (socket-address:address addr)))))))

(define (generic-send-message sockfd flags s start end writer family addr)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" 
	     sockfd flags family addr
	     s start end writer))
  (let loop ((i start))
    (if (< i end)
	(let ((nwritten
	       (if (eq? family 0)
		   (send sockfd (substring s i end) flags)
		   (apply sendto (append (list sockfd (substring s i end)
					       family)
					 (socket-address->list addr)
					 (list flags))))))
	  (loop (+ i nwritten))))))

(define (send-message/partial socket s . args)
  (let-optionals args ((start 0) (end (string-length s)) (flags 0) (addr #f))
    (cond ((not (socket? socket))
	   (error "send-message/partial: socket expected ~s" socket))
	  ((not (integer? flags))
	   (error "send-message/partial: integer expected ~s" flags))
	  ((not (string? s))
	   (error "send-message/partial: string expected ~s" s))
	  (else 
           (generic-send-message/partial (socket->port socket) flags
					 s start end
					 'dummy
					 (if addr (socket-address:family addr) 0)
					 (if addr (socket-address:address addr)))))))

(define (generic-send-message/partial sockfd flags s start end writer family addr)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" 
	     sockfd flags family addr
	     s start end writer))
  (if (= start end) 0			; Vacuous request.
      (if (eq? family 0)
	  (send sockfd (substring s start end) flags)
	  (apply sendto (append (list sockfd (substring s start end) family)
				(socket-address->list addr)
				(list flags))))))

(define-foreign send-substring/errno
  (send_substring (integer sockfd)
		  (integer flags)
		  (string-desc buf)
		  (integer start)
		  (integer end)
		  (integer family)
		  (string-desc name))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getsockopt syscall 
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (socket-option sock level option)
  (cond ((not (socket? sock))
	 (error "socket-option: socket expected ~s" sock))
	((or (not (integer? level))(not (integer? option)))
	 (error "socket-option: integer expected ~s ~s" level option))
	((boolean-option? option)
	 (let ((result (getsockopt (socket->port sock) level option)))
	   (not (= result 0))))
	((value-option? option)
	 (getsockopt (socket->port sock) level option))
	((linger-option? option)
	 (let ((result (getsockopt (socket->port sock) level option)))
	   (if (= (car result) 0)
	       #f
	       (cdr result))))
	;; ((timeout-option? option)
	;; (receive (result/secs usecs)
	;;	  (%getsockopt-linger (socket->fdes sock) level option)
	;;   (cond ((= result/secs -1) 
	;;	  (error "socket-option ~s ~s ~s" sock level option))
	;;	 (else (+ result/secs (/ usecs 1000))))))
	(else
	 "socket-option: unknown option type ~s" option)))

(define-foreign %getsockopt/errno
  (scheme_getsockopt (integer sockfd)
		     (integer level)
		     (integer optname))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-errno-syscall (%getsockopt sock level option) %getsockopt/errno 
  value)

(define-foreign %getsockopt-linger/errno
  (scheme_getsockopt_linger (integer sockfd)
			    (integer level)
			    (integer optname))
  (multi-rep (to-scheme integer errno_or_false)
	     integer) ; error/on-off
  integer) ; linger time

(define-errno-syscall 
  (%getsockopt-linger sock level option) %getsockopt-linger/errno 
  on-off
  linger)

(define-foreign %getsockopt-timeout/errno
  (scheme_getsockopt_timeout (integer sockfd)
			     (integer level)
			     (integer optname))
  (multi-rep (to-scheme integer errno_or_false)
	     integer) ; error/secs
  integer) ; usecs

(define-errno-syscall 
  (%getsockopt-timeout sock level option) %getsockopt-timeout/errno
  secs
  usecs)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; setsockopt syscall 
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (set-socket-option sock level option value)
  (cond ((not (socket? sock))
	 (error "set-socket-option: socket expected ~s" sock))
	((or (not (integer? level)) (not (integer? option)))
	 (error "set-socket-option: integer expected ~s ~s" level option))
	((boolean-option? option)
	 (setsockopt (socket->port sock) level option (if value 1 0)))
	((value-option? option)
	 (setsockopt (socket->port sock) level option value))
	((linger-option? option)
	 (setsockopt(socket->port sock) level option 
		    (if value (cons 1 value) (cons 0 0))))
	;;((timeout-option? option)
	;; (let ((secs (truncate value)))
	;;   (%setsockopt-timeout (socket->fdes sock) level option 
	;;			secs
	;;			(truncate (* (- value secs) 1000)))))
	(else 
	 "set-socket-option: unknown option type")))

(define-foreign %setsockopt/errno
  (scheme_setsockopt (integer sockfd)
		     (integer level)
		     (integer optname)
		     (integer optval))
  (to-scheme integer errno_or_false))

(define-errno-syscall 
  (%setsockopt sock level option value) %setsockopt/errno)


(define-foreign %setsockopt-linger/errno
  (scheme_setsockopt_linger (integer sockfd)
			    (integer level)
			    (integer optname)
			    (integer on-off)
			    (integer time))
  (to-scheme integer errno_or_false))

(define-errno-syscall 
  (%setsockopt-linger sock level option on-off time) %setsockopt-linger/errno)

(define-foreign %setsockopt-timeout/errno
  (scheme_setsockopt_timeout (integer sockfd)
			     (integer level)
			     (integer optname)
			     (integer secs)
			     (integer usecs))
  (to-scheme integer errno_or_false))

(define-errno-syscall 
  (%setsockopt-timeout sock level option secs usecs) %setsockopt-timeout/errno)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socket-option routines
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (boolean-option? opt)
  (member opt options/boolean))

(define (value-option? opt)
  (member opt options/value))

(define (linger-option? opt)
  (member opt options/linger))

(define (timeout-option? opt)
  (member opt options/timeout))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; host lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record host-info
  name					; Host name
  aliases				; Alternative names
  addresses				; Host addresses

  ((disclose hi)			; Make host-info records print like
   (list "host" (host-info:name hi))))	; #{host clark.lcs.mit.edu}.

(define (host-info arg)
  (cond ((string? arg) (name->host-info arg))
	((socket-address? arg) (address->host-info arg))
	(else (error "host-info: string or socket-address expected ~s" arg))))

(define (address->host-info name)
  (if (or (not (socket-address? name)) 
	  (not (= (socket-address:family name) address-family/internet)))
      (error "address->host-info: internet address expected ~s" name)
      (let* ((vec (gethostbyaddr (car (socket-address:address name))))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (addresses (vector-ref vec 4)))
	(make-host-info name aliases addresses))))

(define-foreign %host-address->host-info/h-errno
  (scheme_host_address2host_info (string-desc name))
  (to-scheme integer "False_on_zero")
  static-string	; host name
  (C char**)    ; alias list
  (C char**))   ; address list
  
(define (name->host-info name)
  (if (not (string? name))
      (error "name->host-info: string expected ~s" name)
      (let* ((vec (gethostbyname name))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (addresses (vector-ref vec 4)))
	(make-host-info name aliases addresses))))

(define-foreign %host-name->host-info/h-errno
  (scheme_host_name2host_info (string name))
  (to-scheme integer "False_on_zero")
  static-string	; host name
  (C char**)    ; alias list
  (C char**))   ; address list
  

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; network lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record network-info
  name					; Network name
  aliases				; Alternative names
  net)					; Network number

(define (network-info arg)
  (cond ((string? arg) (name->network-info arg))
	((socket-address? arg) (address->network-info arg))
	(else 
	 (error "network-info: string or socket-address expected ~s" arg))))

(define (address->network-info name)
  (if (not (socket-address? name))
      (error "address->network-info: socket-address expected ~s" name)
      (let* ((vec (getnetbyaddr (car (socket-address:address name))))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (net (vector-ref vec 3)))
	(make-network-info name aliases net))))
		  
(define-foreign %net-address->network-info
  (scheme_net_address2net_info (string-desc name) (string-desc net))
  (to-scheme integer "False_on_zero")
  static-string	; net name
  (C char**))   ; alias list

  
(define (name->network-info name)
  (if (not (string? name))
      (error "name->network-info: string expected ~s" name)
      (let* ((vec (getnetbyname name))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (net (vector-ref vec 3)))
	(make-network-info name aliases net))))
		  
(define-foreign %net-name->network-info
  (scheme_net_name2net_info (string name) (string-desc net))
  (to-scheme integer "False_on_zero")
  static-string	 ; net name
  (C char**))    ; alias list

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; service lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record service-info
  name					; Service name
  aliases				; Alternative names
  port					; Port number
  protocol)				; Protocol name

(define (service-info . args)
  (apply (cond ((string?  (car args)) name->service-info)
	       ((integer? (car args)) port->service-info)
	       (else (error "service-info: string or integer expected ~s" args)))
	 args))

(define (port->service-info name . maybe-proto)
  (let ((proto (:optional maybe-proto "")))
    (cond ((not (integer? name))
	   (error "port->service-info: integer expected ~s" name))
	  ((not (string? proto))
	   (error "port->service-info: string expected ~s" proto))
	  (else
	   (let* ((vec (getservbyport name proto))
		  (name (vector-ref vec 0))
		  (aliases (vector-ref vec 1))
		  (port (vector-ref vec 2))
		  (proto (vector-ref vec 3)))
	     (make-service-info name aliases port proto))))))
		  
(define-foreign %service-port->service-info
  (scheme_serv_port2serv_info (integer name) (string  proto))
  (to-scheme integer "False_on_zero")
  static-string	 ; service name
  (C char**)     ; alias list
  integer        ; port number
  static-string) ; protocol name
  
  
(define (name->service-info name . maybe-proto)
  (let* ((vec (getservbyname name (:optional maybe-proto "")))
	 (name (vector-ref vec 0))
	 (aliases (vector-ref vec 1))
	 (port (vector-ref vec 2))
	 (proto (vector-ref vec 3)))
    (make-service-info name aliases port proto)))
		  
(define-foreign %service-name->service-info
  (scheme_serv_name2serv_info (string name) (string proto))
  (to-scheme integer "False_on_zero")
  static-string	 ; service name
  (C char**)     ; alias list
  integer        ; port number
  static-string) ; protocol name

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; protocol lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record protocol-info
  name					; Protocol name
  aliases				; Alternative names
  number)				; Protocol number

(define (protocol-info arg)
  (cond ((string? arg)  (name->protocol-info arg))
	((integer? arg) (number->protocol-info arg))
	(else (error "protocol-info: string or integer expected ~s" arg))))

(define (number->protocol-info name) 
  (if (not (integer? name))
      (error "number->protocol-info: integer expected ~s" name)
      (let* ((vec (getprotobynumber name))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (number (vector-ref vec 2)))
	(make-protocol-info name aliases number))))

(define-foreign %protocol-port->protocol-info
  (scheme_proto_num2proto_info (integer name))
  (to-scheme integer "False_on_zero")
  static-string	; protocol name
  (C char**)    ; alias list
  integer)      ; protocol number
  
(define (name->protocol-info name)
  (if (not (string? name))
      (error "name->protocol-info: string expected ~s" name)
      (let* ((vec (getprotobyname name))
	     (name (vector-ref vec 0))
	     (aliases (vector-ref vec 1))
	     (number (vector-ref vec 2)))
	(make-protocol-info name aliases number))))
		  
(define-foreign %protocol-name->protocol-info
  (scheme_proto_name2proto_info (string name))
  (to-scheme integer "False_on_zero")
  static-string ; protocol name
  (C char**)    ; alias list
  integer)      ; protocol number

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Lowlevel junk 
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Used to pull address list back
;; based on C-string-vec->Scheme from cig/libcig.scm
;(define (C-long-vec->Scheme cvec veclen) ; No free.
; (let ((vec (make-vector (or veclen (%c-veclen-or-false cvec) 0))))
;    (mapv! (lambda (ignore) (make-string 4)) vec)
;    (%set-long-vector-carriers! vec cvec)
;    (mapv! string->integer vec)))

;(define (integer->string num32)
;  (let* ((str   (make-string 4))
;	 (num24 (arithmetic-shift num32 -8))
;	 (num16 (arithmetic-shift num24 -8))
;	 (num08 (arithmetic-shift num16 -8))
;	 (byte0 (bitwise-and #b11111111 num08))
;	 (byte1 (bitwise-and #b11111111 num16))
;	 (byte2 (bitwise-and #b11111111 num24))
;	 (byte3 (bitwise-and #b11111111 num32)))
;    (string-set! str 0 (ascii->char byte0))
;    (string-set! str 1 (ascii->char byte1))
;    (string-set! str 2 (ascii->char byte2))
;    (string-set! str 3 (ascii->char byte3))
;    str))

;(define (string->integer str)
;  (+ (arithmetic-shift(char->ascii(string-ref str 0))24)
;     (arithmetic-shift(char->ascii(string-ref str 1))16)
;     (arithmetic-shift(char->ascii(string-ref str 2)) 8)
;     (char->ascii(string-ref str 3))))

;; also from cig/libcig.scm
(define-foreign %c-veclen-or-false
  (veclen ((C "const long * ~a") c-vec)); redefining can we open cig-aux?
  desc) ; integer or #f if arg is NULL.

;; also from cig/libcig.scm
(define-foreign %set-long-vector-carriers!
  (set_longvec_carriers (vector-desc svec)
			((C "long const * const * ~a") cvec))
  ignore)

;; also from cig/libcig.scm
(define (mapv! f v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i len) v)
      (vector-set! v i (f (vector-ref v i))))))

;; from scsh/endian.scm.in.

(define host-to-net-16 htons)
(define net-to-host-16 ntohs)
(define host-to-net-32 htonl)
(define net-to-host-32 ntohl)
