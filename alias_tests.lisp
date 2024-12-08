(in-package :coalton-user)
(cl:import 'cl:inspect)

(coalton-toplevel
  (define-alias Foo-Bar Integer)

  (define-alias (MyList :a) (List :a))

  (define-alias Foo-Bar String)

  (define-type Foo Biscuit)
)

  ; (define-type Foo Biscuit)

  ; (declare foo (List Integer -> Integer))
  ; (define (foo _)
  ;   1)
  
  ; (declare bar (MyList Integer -> Integer))
  ; (define (bar _)
  ;   1)
  
  ; (define *foo-nil* (foo Nil)))

; (coalton-toplevel

;   (define x 2)

;   (declare foo (:a -> (Optional :a)))
;   (define foo Some)

;   (declare bar ((Optional :a) -> :a -> :a))
;   (define (bar _ a)
;     a)

;   (declare fooint (Integer -> (Optional Integer)))
;   (define fooint Some))

; (coalton-toplevel
;   (declare fizz ((:a -> (Optional :a)) -> :a -> (Optional :a)))
;   (define (fizz _ _)
;     None))

; (cl:defparameter *x-ty-scheme* (type-of 'x))
; (cl:defparameter *x-tapp*
;   (coalton-impl/typechecker/predicate:qualified-ty-type
;    (coalton-impl/typechecker/scheme:ty-scheme-type *x-ty-scheme*)))

; (coalton
;   (foo 2))

; (coalton (fizz foo 1))

; (cl:defparameter *fenv*
;                  (coalton-impl/typechecker/environment:environment-function-environment
;                    coalton-impl/entry:*global-environment*))

; (cl:defparameter *fdata*
;                  (coalton-impl/algorithm/immutable-map:immutable-map-data *fenv*))

; (cl:defparameter *foo-typ* (type-of 'foo))

; (cl:defparameter *foo-key* 'foo)

; (cl:defparameter *foo-val* (fset:@ *fdata* 'foo))

; (cl:defparameter *foo-tapp*
;   (coalton-impl/typechecker/predicate:qualified-ty-type
;    (coalton-impl/typechecker/scheme:ty-scheme-type *foo-typ*)))

; (cl:defparameter *bar-typ* (type-of 'bar))

; (cl:defparameter *bar-key* 'bar)

; (cl:defparameter *bar-val* (fset:@ *fdata* 'bar))

; (cl:defparameter *bar-tapp*
;   (coalton-impl/typechecker/predicate:qualified-ty-type
;    (coalton-impl/typechecker/scheme:ty-scheme-type *bar-typ*)))

; (cl:defparameter *fooint-typ* (type-of 'fooint))

; (cl:defparameter *fooint-key* 'fooint)

; (cl:defparameter *fooint-val* (fset:@ *fdata* 'fooint))

; (cl:defparameter *fooint-tapp*
;   (coalton-impl/typechecker/predicate:qualified-ty-type
;    (coalton-impl/typechecker/scheme:ty-scheme-type *fooint-typ*)))

; ;(coalton-impl/typechecker/unify:unify (cl:list) *foo-tapp* *bar-tapp*)

; ;(coalton-impl/typechecker/unify:unify (cl:list) *x-tapp* *foo-tapp*)

; ;; (coalton-impl/typechecker/unify:unify (cl:list) *foo-tapp* *x-tapp*)

; (coalton-impl/typechecker/unify:unify (cl:list) *foo-tapp* *fooint-tapp*)

; (cl:type-of
;  (coalton-impl/typechecker/predicate:qualified-ty-type
;   (coalton-impl/typechecker/scheme:ty-scheme-type *foo-typ*)))

; (cl:defun discover ()
;   (cl:dolist (p (cl:list-all-packages))
;     (cl:do-external-symbols (s p)
;       (cl:ignore-errors
;         (cl:format cl:t "~a: ~a~%" s (type-of s))))))
