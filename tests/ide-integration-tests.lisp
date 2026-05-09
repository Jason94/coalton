(in-package #:coalton-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (uiop:define-package #:coalton-tests/ide-integration-test-package
    (:use #:coalton #:coalton-prelude)))

(in-package #:coalton-tests)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *infos-name-table*
  (make-hash-table :test #'equal)
  "Store info results by display name in a hashtable")

(defun push-symbol-info (symbol-info)
  "Store a symbol-info in the *infos-name-table*"
  (setf (gethash (ide:symbol-info-display-name symbol-info)
                       *infos-name-table*)
           symbol-info))

(defun get-info (display-name)
  "Lookup a symbol info by its display name"
  (gethash display-name *infos-name-table*))

(defun printed-type (symbol-info)
  "Get the printed type string from symbol-info."
  (format nil "~a" (ide:symbol-info-type symbol-info)))

(defun docstring (symbol-info)
  "Get the docstring form symbol-info."
  (ide:symbol-info-docstring symbol-info))

(defun test-setup ()
  "Clear the cached infos and load into the symbol hook."
  (clrhash *infos-name-table*)
  (setf ide:*symbol-hook* #'push-symbol-info))

(defun test-cleanup ()
  "Remove the symbol hook."
  (setf ide:*symbol-hook* nil))

(deftest test-ide-variable-type-at ()
  (test-setup)

  (with-coalton-compilation (:package #:coalton-tests/ide-integration-test-package)
    (coalton-toplevel
      (declare test-variable-type-at (Void -> Void))
      (define (test-variable-type-at)
        (let _x = (the Integer 5))
        (values))))

  (let ((x (get-info "_X")))
    (test-cleanup)

    (is (string= "INTEGER"
                 (printed-type x)))))

(defun test-ide-function-docstring ()
  (test-setup)

  (with-coalton-compilation (:package #:coalton-tests/ide-integration-test-package)
    (coalton-toplevel
      (declare test-function-docstring (Void -> Void))
      (define (test-function-docstring)
        "Test Docstring"
        (values))))

  (let ((test-funtion-docstring (get-info "TEST-FUNCTION-DOCSTRING")))
    (test-cleanup)

    (is (string= "Test Docstring"
                 (docstring test-funtion-docstring)))))
