(coalton-library/utils:defstdlib-package #:coalton-library/monad/optionalt
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/monad/state)
  (:export
   #:OptionalT
   #:run-optionalT
   #:map-optionalT
   ))

(in-package #:coalton-library/monad/optionalt)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; OptionalT
;;;

(coalton-toplevel
  (define-type (OptionalT :m :a)
    "A monadic computation that returns an Optional."
    (OptionalT (:m (Optional :a))))
  
  (declare run-optionalT (OptionalT :m :a -> :m (Optional :a)))
  (define (run-optionalT (OptionalT m))
    m)
  
  (declare map-optionalT ((:m (Optional :a) -> :n (Optional :b))
                          -> OptionalT :m :a
                          -> OptionalT :n :b))
  (define (map-optionalT f (OptionalT m))
    (OptionalT (f m))))

;;;
;;; Instances
;;;

(coalton-toplevel
  (define-instance (Functor :m => Functor (OptionalT :m))
    (define (map fa->b (OptionalT m))
      (OptionalT (map (map fa->b) m))))
  
  (define-instance (Monad :m => Applicative (OptionalT :m))
    (define (pure a)
      (OptionalT (pure (Some a))))
    (define (liftA2 fa->b->c (OptionalT ma) (OptionalT mb))
      (OptionalT
        (do
          (opta <- ma)
          (match opta
            ((None) (pure None))
            ((Some a)
             (do
               (optb <- mb)
               (match optb
                 ((None) (pure None))
                 ((Some b)
                  (pure (Some (fa->b->c a b))))))))))))
  
  (define-instance (Monad :m => Monad (OptionalT :m))
    (define (>>= (OptionalT ma) fa->optb)
      (OptionalT
        (do
          (opta <- ma)
          (match opta
            ((None) (pure None))
            ((Some a)
             (run-optionalT (fa->optb a))))))))
  
  (define-instance (MonadTransformer OptionalT)
    (define lift (compose OptionalT (map Some))))
  
  (define-instance (MonadState :s :m => MonadState :s (OptionalT :m))
    (define put (compose lift put))
    (define get (lift get)))
  )

;; #+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/OPTIONALT")
