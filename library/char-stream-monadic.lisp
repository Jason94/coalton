(coalton-library/utils:defstdlib-package #:coalton-library/char-stream-monadic
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/monad/io)
  (:local-nicknames (#:cs #:coalton-library/char-stream))
  (:export
    #:open?
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

(cl:in-package #:coalton-library/char-stream-monadic)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun safe-symbol (unbound-symbol)
    "Given the symbol for an unbound function, produce a symbol for an IO-bound version.

     If the unbound symbol ends in '!' it strips it. Otherwise uses the same name."
     (cl:let ((name (cl:symbol-name unbound-symbol)))
       (cl:intern
         (cl:if (cl:char=
                  #\!
                  (cl:elt name (cl:1- (cl:length name))))
           (cl:subseq name 0 (cl:1- (cl:length name)))
           name)))))

(cl:defmacro define-monadic (arity unsafe-symbol)
  (cl:let ((arg-syms (cl:loop for n from 1 to arity collect (cl:gensym))))
    `(coalton-toplevel
       (define (,(safe-symbol unsafe-symbol) ,@arg-syms)
         (IO
           (fn ()
             (,unsafe-symbol ,@arg-syms)))))))

(define-monadic 1 cs:open?)
(define-monadic 1 cs:close!)

(define-monadic 1 cs:read-char!)
(define-monadic 1 cs:read-line!)

(define-monadic 2 cs:write-char!)
(define-monadic 2 cs:write-string!)
(define-monadic 2 cs:write-line!)
(define-monadic 2 cs:flush-output!)

(define-monadic 0 cs:read-line-std!)
(define-monadic 1 cs:write-line-std!)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR-STREAM-MONADIC")
