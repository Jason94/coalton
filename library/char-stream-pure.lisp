(coalton-library/utils:defstdlib-package #:coalton-library/char-stream-pure
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/monad/io)
  (:local-nicknames (#:cs #:coalton-library/char-stream))
  (:export
    #:write-line))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(cl:in-package #:coalton-library/char-stream-pure)

(cl:defun safe-symbol (unsafe-symbol)
  "Given the symbol for an unsafe function, produce a symbol for a safe version.

   If the unsafe symbol ends in '!' it strips it. Otherwise the safe symbol
   will look the same."
   (cl:let ((name (cl:symbol-name unsafe-symbol)))
     (cl:intern
       (cl:if (cl:char=
                #\!
                (cl:elt name (cl:1- (cl:length name))))
         (cl:subseq name 0 (cl:1- (cl:length name)))
         name))))

(coalton-toplevel

  ; (declare write-line (cs:Output :stream => :stream -> String -> IO (Result cs:StreamError Unit)))
  (define (write-line stream string)
    (IO
      (fn ()
        (cs:write-line! stream string)))))
