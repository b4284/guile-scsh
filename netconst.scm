(define-module (scsh netconst))
(export options/boolean options/value options/linger
	shutdown/receives shutdown/sends shutdown/sends+receives)
;; more generated exports below.

(defmacro maybe-define (name value)
  `(cond ((defined? ',value)
	  (define ,name ,value)
	  (export ,name))))

(defmacro maybe-define-so (name value type)
  (let ((type-var (string->symbol (string-append "options/"
						 (symbol->string type)))))
    `(cond ((defined? ',value)
	    (define ,name ,value)
	    (export ,name)
	    (set! ,type-var (cons ,value ,type-var))))))

(maybe-define address-family/unspecified	AF_UNSPEC)
(maybe-define address-family/unix 		AF_UNIX)
(maybe-define address-family/internet		AF_INET)

(maybe-define protocol-family/unspecified	PF_UNSPEC)
(maybe-define protocol-family/unix 		PF_UNIX)
(maybe-define protocol-family/internet		PF_INET)

(maybe-define socket-type/stream	SOCK_STREAM)
(maybe-define socket-type/datagram	SOCK_DGRAM)
(maybe-define socket-type/raw		SOCK_RAW)

(maybe-define internet-address/any		INADDR_ANY)
(maybe-define internet-address/broadcast	INADDR_BROADCAST)
(maybe-define internet-address/loopback		INADDR_LOOPBACK)

(maybe-define level/socket	SOL_SOCKET)

(define options/boolean '())
(define options/value '())
(define options/linger '())

(maybe-define-so socket/debug		SO_DEBUG	boolean)
(maybe-define-so socket/reuse-address	SO_REUSEADDR	boolean)
(maybe-define-so socket/type		SO_TYPE		value)
(maybe-define-so socket/error		SO_ERROR	value)
(maybe-define-so socket/dont-route	SO_DONTROUTE	boolean)
(maybe-define-so socket/broadcast	SO_BROADCAST	boolean)
(maybe-define-so socket/send-buffer	SO_SNDBUF	value)
(maybe-define-so socket/receive-buffer	SO_RCVBUF	value)
(maybe-define-so socket/keep-alive	SO_KEEPALIVE	boolean)
(maybe-define-so socket/linger		SO_LINGER	linger)

(maybe-define message/out-of-band	MSG_OOB)
(maybe-define message/peek		MSG_PEEK)
(maybe-define message/dont-route	MSG_DONTROUTE)

(define shutdown/receives	0)
(define shutdown/sends		1)
(define shutdown/sends+receives	2)
