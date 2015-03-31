;; scsh/lib/char-package.scm in scsh 0.5.3.

(define-module (scsh char-p)
  :use-module (srfi srfi-14))

(export
 ;; R5RS
 ;; char-lower-case? char-upper-case? char-alphabetic? char-numeric?
 ;; char-whitespace?

 char-letter? char-digit? char-letter+digit? char-graphic?
 char-printing? char-blank? char-iso-control? char-punctuation?
 char-symbol? char-hex-digit? char-ascii?

 ;; Obsolete.
 char-alphanumeric? char-control?
)

;; These are scsh extensions to R5RS.
(define (char-letter?       c) (char-set-contains? char-set:letter       c))
(define (char-digit?        c) (char-set-contains? char-set:digit        c))
(define (char-letter+digit? c) (char-set-contains? char-set:letter+digit c))
(define (char-graphic?      c) (char-set-contains? char-set:graphic      c))
(define (char-printing?     c) (char-set-contains? char-set:printing     c))
(define (char-blank?        c) (char-set-contains? char-set:blank        c))
(define (char-iso-control?  c) (char-set-contains? char-set:iso-control  c))
(define (char-punctuation?  c) (char-set-contains? char-set:punctuation  c))
(define (char-symbol?       c) (char-set-contains? char-set:symbol       c))
(define (char-hex-digit?    c) (char-set-contains? char-set:hex-digit    c))
(define (char-ascii?        c) (char-set-contains? char-set:ascii        c))

;; Obsolete scsh.
(begin-deprecated
 (define (char-alphanumeric? c)
   (issue-deprecation-warning
    "char-alphanumeric? is deprecated.  Use char-letter+digit? instead")
   (char-letter+digit? c))

 (define (char-control? c)
   (issue-deprecation-warning
    "char-control? is deprecated.  Use char-iso-control? instead")
   (char-iso-control? c)))
