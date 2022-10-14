(coalton-library/utils:defstdlib-package #:coalton-library/char-stream-pure
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/monad/io)
  (:local-nicknames (#:cs #:coalton-library/char-stream))
  (:export
    #:pure-open?
    #:close

    #:read-char
    #:read-line

    #:write-char
    #:write-string
    #:write-line
    #:flush-output

    #:read-line-std
    #:write-line-std))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(cl:in-package #:coalton-library/char-stream-pure)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun safe-symbol (unsafe-symbol)
    "Given the symbol for an unsafe function, produce a symbol for a safe version.

     If the unsafe symbol ends in '!' it strips it. Otherwise it prepends 'pure-'."
     (cl:let ((name (cl:symbol-name unsafe-symbol)))
       (cl:intern
         (cl:if (cl:char=
                  #\!
                  (cl:elt name (cl:1- (cl:length name))))
           (cl:subseq name 0 (cl:1- (cl:length name)))
           (cl:concatenate 'cl:string "pure-" name))))))

(cl:defmacro define-safe (arity unsafe-symbol)
  (cl:let ((arg-syms (cl:loop for n from 1 to arity collect (cl:gensym))))
    `(coalton-toplevel
       (define (,(safe-symbol unsafe-symbol) ,@arg-syms)
         (IO
           (fn ()
             (,unsafe-symbol ,@arg-syms)))))))

(define-safe 1 cs:open?)
(define-safe 1 cs:close!)

(define-safe 1 cs:read-char!)
(define-safe 1 cs:read-line!)

(define-safe 2 cs:write-char!)
(define-safe 2 cs:write-string!)
(define-safe 2 cs:write-line!)
(define-safe 2 cs:flush-output!)

(define-safe 0 cs:read-line-std!)
(define-safe 1 cs:write-line-std!)
