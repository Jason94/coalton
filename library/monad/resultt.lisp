(coalton-library/utils:defstdlib-package #:coalton-library/monad/resultt
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/result
   #:coalton-library/monad/state)
  (:export
   #:ResultT
   #:run-resultT
   #:map-resultT
   #:map-errT
   ))

(in-package #:coalton-library/monad/resultt)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;;  ResultT
;;;
(coalton-toplevel
  (define-type (ResultT :err :m :ok)
    "A monadic computation that returns a Result."
    (ResultT (:m (Result :err :ok))))

  (declare run-resultT (ResultT :err :m :ok -> :m (Result :err :ok)))
  (define (run-resultT (ResultT m))
    m)

  (declare map-resultT ((:m (Result :e1 :a) -> :n (Result :e2 :b))
                        -> ResultT :e1 :m :a
                        -> ResultT :e2 :n :b))
  (define (map-resultT f (ResultT m))
    (ResultT (f m)))

  (declare map-errT (Functor :m => (:a -> :b) -> ResultT :a :m :c -> ResultT :b :m :c))
  (define (map-errT ferr) (map-resultT (map (map-err ferr)))))

;;;
;;; Instances
;;;

(coalton-toplevel
  (define-instance (Functor :m => Functor (ResultT :err :m))
    (define (map fa->b (ResultT m))
      (ResultT (map (map fa->b) m))))

  (define-instance (Monad :m => Applicative (ResultT :err :m))
    (define (pure a)
      (ResultT (pure (Ok a))))
    (define (liftA2 fa->b->c (ResultT ma) (ResultT mb))
      (ResultT
       (do
        (resa <- ma)
        (match resa
          ((Err er) (pure (Err er)))
          ((Ok a)
           (do
            (resb <- mb)
            (match resb
              ((Err er) (pure (Err er)))
              ((Ok b)
               (pure (Ok (fa->b->c a b))))))))))))

  (define-instance (Monad :m => Monad (ResultT :err :m))
    (define (>>= (ResultT ma) fa->resmb)
      (ResultT
       (do
        (resa <- ma)
        (match resa
          ((Err er) (pure (Err er)))
          ((Ok a)
           (run-resultT (fa->resmb a))))))))

  (define-instance (MonadTransformer (ResultT :err))
    (define lift (compose ResultT (map Ok))))

  (define-instance (MonadState :s :m => MonadState :s (ResultT :err :m))
    (define put (compose lift put))
    (define get (lift get))))

;; #+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/RESULTT")
