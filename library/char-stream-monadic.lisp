(coalton-library/utils:defstdlib-package #:coalton-library/char-stream-monadic
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/monad/io)
  (:local-nicknames (#:cs #:coalton-library/char-stream))
  (:import-from #:coalton-library/char-stream
    #:StreamError
    #:StreamErrorSimple
    #:StreamErrorClosed
    #:StreamErrorEndOfFile
    #:StreamErrorDecoding
    #:StreamErrorEncoding
    #:StreamErrorTimeout
    #:StreamErrorReader

    #:Stream

    #:Input
    #:Output

    #:standard-input
    #:standard-output

    #:FlushOperation
    #:FlushBlocking
    #:FlushAsync

    #:Path

    #:Encoding
    #:ASCII
    #:UTF-8
    #:UTF-16
    #:LATIN-1
    #:default-encoding

    #:IfExists
    #:IfExistsError
    #:IfExistsRename
    #:IfExistsAppend
    #:IfExistsOverwrite
    #:IfExistsSupersede

    #:IfDoesNotExist
    #:IfDoesNotExistError
    #:IfDoesNotExistCreate

    #:FileOptions
    #:default-file-options
    #:with-encoding
    #:if-exists
    #:if-does-not-exist

    #:FileError

    #:wrap-input-stream-for-lisp
    #:wrap-output-stream-for-lisp
    #:wrap-two-way-stream-for-lisp

    #:coalton-stream-error
    #:coalton-stream-error-simple
    #:coalton-stream-error-simple-message
    #:coalton-stream-error-closed
    #:coalton-stream-error-end-of-file
    #:coalton-stream-error-decoding
    #:coalton-stream-error-encoding
    #:coalton-stream-error-timeout
    #:coalton-stream-error-reader)
  (:export
    ;; We don't want users to mix unbound and monadic IO, so we provide the
    ;; types and access to standard input/output that don't need to be wrapped.
    #:StreamError
    #:StreamErrorSimple
    #:StreamErrorClosed
    #:StreamErrorEndOfFile
    #:StreamErrorDecoding
    #:StreamErrorEncoding
    #:StreamErrorTimeout
    #:StreamErrorReader

    #:Stream

    #:Input
    #:Output

    #:standard-input
    #:standard-output

    #:FlushOperation
    #:FlushBlocking
    #:FlushAsync

    #:Path

    #:Encoding
    #:ASCII
    #:UTF-8
    #:UTF-16
    #:LATIN-1
    #:default-encoding

    #:IfExists
    #:IfExistsError
    #:IfExistsRename
    #:IfExistsAppend
    #:IfExistsOverwrite
    #:IfExistsSupersede

    #:IfDoesNotExist
    #:IfDoesNotExistError
    #:IfDoesNotExistCreate

    #:FileOptions
    #:default-file-options
    #:with-encoding
    #:if-exists
    #:if-does-not-exist

    #:FileError

    #:wrap-input-stream-for-lisp
    #:wrap-output-stream-for-lisp
    #:wrap-two-way-stream-for-lisp

    #:coalton-stream-error
    #:coalton-stream-error-simple
    #:coalton-stream-error-simple-message
    #:coalton-stream-error-closed
    #:coalton-stream-error-end-of-file
    #:coalton-stream-error-decoding
    #:coalton-stream-error-encoding
    #:coalton-stream-error-timeout
    #:coalton-stream-error-reader

    ;; Monadic IO interface
    #:open?
    #:close

    #:read-char
    #:read-line
    #:input-chars
    #:input-lines

    #:write-char
    #:write-string
    #:flush-output

    #:newline
    #:write-line
    #:write-chars
    #:write-lines
    #:write-strings
    #:finish-output
    #:force-output

    #:open-input-file
    #:open-output-file
    #:open-two-way-file

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
(define-monadic 1 cs:input-chars!)
(define-monadic 1 cs:input-lines!)

(define-monadic 2 cs:write-char!)
(define-monadic 2 cs:write-string!)
(define-monadic 2 cs:flush-output!)

(define-monadic 1 cs:newline!)
(define-monadic 2 cs:write-line!)
(define-monadic 2 cs:write-chars!)
(define-monadic 2 cs:write-lines!)
(define-monadic 2 cs:write-strings!)
(define-monadic 1 cs:finish-output!)
(define-monadic 1 cs:force-output!)

(define-monadic 2 cs:open-input-file!)
(define-monadic 2 cs:open-output-file!)
(define-monadic 2 cs:open-two-way-file!)

(define-monadic 0 cs:read-line-std!)
(define-monadic 1 cs:write-line-std!)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR-STREAM-MONADIC")
