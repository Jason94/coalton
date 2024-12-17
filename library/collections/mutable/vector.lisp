(coalton-library/utils:defstdlib-package #:coalton-library/collections/mutable/vector
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:cln #:coalton-library/collections/classes)
   (#:opt #:coalton-library/optional)
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:ram #:coalton-library/randomaccess))
  (:export
   #:Vector
   #:new
   #:with-capacity
   #:with-initial-element
   #:singleton
   #:length
   #:capacity
   #:empty?
   #:singleton?
   #:copy
   #:set-capacity!
   #:clear!
   #:push!
   #:pop!
   #:pop-unsafe!
   #:index
   #:index-unsafe
   #:set!
   #:head
   #:head-unsafe
   #:last
   #:last-unsafe
   #:extend!
   #:find-elem
   #:append
   #:swap-remove!
   #:swap-remove-unsafe!
   #:sort!
   #:sort-by!
   #:make))

(in-package #:coalton-library/collections/mutable/vector)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; Vector
  ;;

  (repr :native (cl:and (cl:vector cl:t) (cl:not cl:simple-vector)))
  (define-type (Vector :a))

  (declare new (Unit -> Vector :a))
  (define (new _)
    "Create a new empty vector"
    (with-capacity 0))

  (declare with-capacity (UFix -> Vector :a))
  (define (with-capacity n)
    "Create a new vector with `n` elements preallocated."
    (lisp (Vector :a) (n)
      (cl:make-array n :fill-pointer 0 :adjustable cl:t :element-type cl:t)))

  (declare with-initial-element (UFix -> :a -> Vector :a))
  (define (with-initial-element n x)
    "Create a new vector with `n` elements equal to `x`."
    (let v = (with-capacity n))
    (extend! v (iter:repeat-for x n))
    v)

  (declare singleton (:a -> Vector :a))
  (define (singleton x)
    "Create a new vector with a single element equal to `x`"
    (with-initial-element 1 x))

  (declare length (Vector :a -> UFix))
  (define (length v)
    "Returns the length of `v`."
    (lisp UFix (v)
      (cl:length v)))

  (declare capacity (Vector :a -> UFix))
  (define (capacity v)
    "Returns the number of elements that `v` can store without resizing."
    (lisp UFix (v)
      (cl:array-dimension v 0)))

  (declare empty? (Vector :a -> Boolean))
  (define (empty? v)
    "Is `v` empty?"
    (== 0 (length v)))

  (declare singleton? (Vector :a -> Boolean))
  (define (singleton? v)
    "Is `v` a singleton?"
    (== 1 (length v)))

  (declare copy (Vector :a -> Vector :a))
  (define (copy v)
    "Return a new vector containing the same elements as `v`."
    (lisp (Vector :a) (v)
      ;; We use COPY-ARRAY and not COPY-SEQ to get identical
      ;; adjustable properties.
      (alexandria:copy-array v)))

  (declare set-capacity! (UFix -> Vector :a -> Unit))
  (define (set-capacity! new-capacity v)
    "Set the capacity of `v` to `new-capacity`. Setting the capacity to lower then the length will remove elements from the end."
    (let shrinking = (< new-capacity (length v)))
    (lisp Unit (v shrinking new-capacity)
      ;; If the array is getting larger then don't change the
      ;; fill pointer
      (cl:adjust-array v new-capacity :fill-pointer shrinking)
      Unit))

  (declare clear! (Vector :a -> Unit))
  (define (clear! v)
    "Set the capacity of `v` to `0`."
    (set-capacity! 0 v))

  (declare push! (:a -> Vector :a -> UFix))
  (define (push! item v)
    "Append `item` to `v` and resize `v` if necessary, returning the index of the new item."
    (lisp UFix (item v)
      (cl:vector-push-extend item v)))

  (declare pop! (Vector :a -> Optional :a))
  (define (pop! v)
    "Remove and return the last item of `v`."
    (if (empty? v)
        None
        (Some (pop-unsafe! v))))

  (declare pop-unsafe! (Vector :a -> :a))
  (define (pop-unsafe! v)
    "Remove and return the last item of `v` without checking if the vector is empty."
    (lisp :a (v)
      (cl:vector-pop v)))

  (declare index (UFix -> Vector :a -> Optional :a))
  (define (index index v)
    "Return the `index`th element of `v`."
    (if (>= index (length v))
        None
        (Some (index-unsafe index v))))

  (declare index-unsafe (UFix -> Vector :a -> :a))
  (define (index-unsafe idx v)
    "Return the `idx`th element of `v` without checking if the element exists."
    (lisp :a (idx v)
      (cl:aref v idx)))

  (declare set! (UFix -> :a -> Vector :a -> Unit))
  (define (set! idx item v)
    "Set the `idx`th element of `v` to `item`. This function left intentionally unsafe because it does not have a return value to check."
    (lisp Void (idx item v)
      (cl:setf (cl:aref v idx) item))
    Unit)

  (declare head (Vector :a -> Optional :a))
  (define (head v)
    "Return the first item of `v`."
    (index 0 v))

  (declare head-unsafe (Vector :a -> :a))
  (define (head-unsafe v)
    "Return the first item of `v` without first checking if `v` is empty."
    (index-unsafe 0 v))

  (declare last (Vector :a -> Optional :a))
  (define (last v)
    "Return the last element of `v`."
    (index (- (length v) 1) v))

  (declare last-unsafe (Vector :a -> :a))
  (define (last-unsafe v)
    "Return the last element of `v` without first checking if `v` is empty."
    (index-unsafe (- (length v) 1) v))

  (declare find-elem (Eq :a => :a -> Vector :a -> Optional UFix))
  (define (find-elem e v)
    "Find the index of element `e` in `v`."
    (let ((test (fn (elem)
                  (== elem e))))

      (lisp (Optional UFix) (v test)
        (cl:let ((pos (cl:position-if
                       (cl:lambda (x)
                         (cl:eq cl:t (call-coalton-function test x)))
                       v)))
          (cl:if pos
                 (Some pos)
                 None)))))

  (declare contains-elt? (Eq :a => :a -> Vector :a -> Boolean))
  (define (contains-elt? elt vec)
    (opt:some? (find-elem elt vec)))

  (declare contains-where? ((:a -> Boolean) -> Vector :a -> Boolean))
  (define (contains-where? f vec)
    (for x in vec
      (when (f x)
        (return True)))
    False)

  (declare append (Vector :a -> Vector :a -> Vector :a))
  (define (append v1 v2)
    "Create a new vector containing the elements of `v1` followed by the elements of `v2`."
    (let out = (with-capacity (+ (length v1) (length v2))))
    (extend! out v1)
    (extend! out v2)
    out)

  (declare swap-remove! (UFix -> Vector :a -> Optional :a))
  (define (swap-remove! idx vec)
    "Remove the element `idx` from `vec` and replace it with the last element in `vec`. Then return the removed element."
    (if (>= idx (length vec))
        None
        (Some (swap-remove-unsafe! idx vec))))

  (declare swap-remove-unsafe! (UFix -> Vector :a -> :a))
  (define (swap-remove-unsafe! idx vec)
    "Remove the element `idx` from `vec` and replace it with the last element in `vec` without bounds checking. Then return the removed element."
    (if (== (+ 1 idx) (length vec))
        (pop-unsafe! vec)
        (progn
          (let out = (index-unsafe idx vec))
          (set! idx (pop-unsafe! vec) vec)
          out)))
  
  (declare reverse! (Vector :a -> Vector :a))
  (define (reverse! vec)
    "Reverse a vector in place and return the vector for convenience."
    (lisp :x (vec)
      (cl:dotimes (i (cl:floor (cl:/ (cl:length vec) 2)))
        (cl:rotatef (cl:aref vec i)
                    (cl:aref vec (cl:- (cl:length vec) i 1)))))

    vec)

  (declare sort-by! ((:a -> :a -> Boolean) -> Vector :a -> Unit))
  (define (sort-by! f v)
    "Sort a vector in-place with predicate function `f`."
    (lisp Void (v f)
      (cl:sort
       v
       (cl:lambda (a b)
         (call-coalton-function f a b))))
    Unit)

  (declare sort! (Ord :a => Vector :a -> Unit))
  (define (sort! v)
    "Sort a vector in-place in ascending order."
    (sort-by! < v))

  (declare extend! (iter:IntoIterator :container :elt => Vector :elt -> :container -> Unit))
  (define (extend! vec iter)
    "Push every element in `iter` to the end of `vec`."
    (let iter = (iter:into-iter iter))

    ;; If the iterator is known to require more capacity then vec has,
    ;; resize before pushing elements
    (let size = (with-default 0 (iter:size-hint iter)))
    (let remaining-capacity = (- (capacity vec) (length vec)))
    (when (> size remaining-capacity)
      (set-capacity! (- size remaining-capacity) vec))

    (iter:for-each!
     (fn (x)
       (push! x vec)
       Unit)
     iter)
    Unit)

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => Eq (Vector :a))
    (define (== v1 v2)
      (if (/= (length v1) (length v2))
          False
          (iter:every! id (iter:zip-with! == (iter:into-iter v1) (iter:into-iter v2))))))

  (define-instance (Semigroup (Vector :a))
    (define <> append))

  (define-instance (Functor Vector)
    (define (map f v)
      (let out = (with-capacity (length v)))
      (iter:for-each!
       (fn (x)
         (push! (f x) out)
         Unit)
       (iter:into-iter v)) 
      out))

  (define-instance (Foldable Vector)
    (define (fold f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (b a)
           (call-coalton-function f b a))
         vec
         :initial-value init)))
    (define (foldr f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (a b)
           (call-coalton-function f a b))
         vec
         :initial-value init
         :from-end cl:t))))

  (define-instance (ram:RandomAccess (Vector :t) :t)
    (define (ram:make n x)
      (with-initial-element n x))
    (define (ram:length a)
      (length a))
    (define (ram:readable? _)
      True)
    (define (ram:writable? _)
      True)
    (define (ram:unsafe-aref a n)
      (index-unsafe n a))
    (define (ram:unsafe-set! a n x)
      (set! n x a)))

  (define-instance (Into (List :a) (Vector :a))
    (define (into lst)
      (let ((out (with-capacity (list:length lst)))
            (inner
              (fn (lst)
                (match lst
                  ((Cons x xs)
                   (progn
                     (push! x out)
                     (inner xs)))
                  ((Nil) Unit)))))
        (progn
          (inner lst)
          out))))

  (define-instance (Into (Vector :a) (List :a))
    (define (into v)
        (iter:collect! (iter:into-iter v))))

  (define-instance (Iso (Vector :a) (List :a)))

  (define-instance (iter:IntoIterator (Vector :a) :a)
    (define (iter:into-iter vec)
      (let idx = (cell:new 0))
      (iter:with-size
          (fn ()
            (let res = (index (cell:read idx) vec))
            (cell:increment! idx)
            res)
        (length vec))))

  (define-instance (iter:FromIterator (Vector :a) :a)
    (define (iter:collect! iter)
      (let size = (with-default 0 (iter:size-hint iter)))
      (let vec = (with-capacity size))
      (iter:for-each! (fn (x)
                        (push! x vec)
                        Unit)
                      iter)
      vec))

  (define-instance (Default (Vector :a))
    (define default new)))

;;
;; Collections Instances
;;

(coalton-toplevel
  (define-instance (cln:Collection Vector)
    (define cln:new-collection new)
    (define cln:new-repeat with-initial-element)
    (define (cln:new-from n f)
      (let ((vec (with-capacity n)))
        (for i in (list:range 0 (- n 1))
          (set! i (f i) vec))
        vec))
    (define (cln:flatten vecs)
      (let ((res (with-capacity
                     (fold (fn (sum v)
                             (+ sum (length v)))
                           0
                           vecs)))
            (i (cell:new 0)))
        (for vec in vecs
          (for x in vec
            (set! (cell:read i) x res)
            (cell:increment! i)))
        res))
    (define (cln:filter f vec)
      (iter:collect! (iter:filter! f (iter:into-iter vec))))
    (define (cln:remove-duplicates vec)
      (let ((res (with-capacity (length vec))))
        (for i in (list:range 0 (- (length vec) 1))
          (let elt = (index-unsafe i vec))
          (unless (contains-elt? elt res)
            (push! elt res)
            Unit))
        res))
    (define (cln:empty? vec)
      (== 0 (length vec)))
    (define cln:length length)
    (define cln:contains-elt? contains-elt?)
    (define cln:contains-where? contains-where?)
    (define (cln:count-where f vec)
      (fold (fn (sum elt)
              (if (f elt)
                  (+ sum 1)
                  sum))
            0
            vec))
    (define (cln:add elt vec)
      (let ((res (with-capacity (+ 1 (length vec)))))
        (push! elt res)
        res)))
  
  (define-instance (cln:MutableCollection Vector)
    (define cln:copy copy)
    (define (cln:add! elt vec)
      (push! elt vec)
      vec))
  
  (define-instance (cln:LinearCollection Vector))

  (define-instance (cln:MutableLinearCollection Vector)
    (define cln:reverse! reverse!)))

(cl:defmacro make (cl:&rest elements)
  "Construct a `Vector' containing the ELEMENTS, in the order listed."
  (cl:let* ((length (cl:length elements))
            (vec (cl:gensym "VEC-")))
    `(progn
       (let ,vec = (with-capacity ,length))
       ,@(cl:loop :for elt :in elements
            :collect `(push! ,elt ,vec))
       ,vec)))

; #+sb-package-locks
; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/MUTABLE/VECTOR")