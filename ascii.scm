(define-module (scsh ascii))
(export ascii->char char->ascii)

(define ascii->char integer->char)
(define char->ascii char->integer)   ;; also in glob.
