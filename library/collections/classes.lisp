(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:l #:coalton-library/collections/immutable/list)
   (#:types #:coalton-library/types)
   (#:o #:coalton-library/optional))
  (:export
   #:Collection
   #:new-collection
   #:new-repeat
   #:new-from
   #:flatten
   #:filter
   #:remove-duplicates
   #:empty?
   #:length
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add
   #:remove-elt

   #:ImmutableCollection

   #:MutableCollection
   #:copy
   #:add!
   
   #:LinearCollection
   #:head
   #:head#
   #:last
   #:last#
   #:index-elt
   #:index-elt#
   #:index-where
   #:index-where#
   #:find-where
   
   #:MutableLinearCollection
   #:reverse!))

(in-package #:coalton-library/collections/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Collection :m)
    "Types that contain a collection of elements of another type.

Does not have an ordering of elements.

Could be mutable or immutable. Methods are not allowed to modify the
underlying collection. If you need mutability as part of the contract,
probably to prevent defensive copying, use one of the Mutable
collection typeclasses."
    ;; Create new collections
    (new-collection
     "Create a new, empty collection."
     (Unit -> :m :a))
    (new-repeat
     "Create a new collection, attempting to add `elt` `n` times."
     (UFix -> :a -> :m :a))
    (new-from
     "Create a new collection by appling a function over the range [0, n)."
     (UFix -> (UFix -> :a) -> :m :a))
    ;; Manipulate at the collection level
    (flatten
     "Flatten a collection of collections into a collection of their elements."
     (:m (:m :a) -> :m :a))
    (filter
     "Create a new collection with the elements satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> :m :a))
    (remove-duplicates
     "Create a new collection with all distinct elements."
     (Eq :a => :m :a -> :m :a))
    ;; Query the collection
    (empty?
     "Check if the collection contains no elements."
     (:m :a -> Boolean))
    (length
     "The number of elements in the collection"
     (:m :a -> UFix))
    (contains-elt?
     "Check if the collection contains an element."
     (Eq :a => :a -> :m :a -> Boolean))
    (contains-where?
     "Check if the collection contains an element satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> Boolean))
    (count-where
     "The number of elements satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> UFix))
    ;; Manipulate at the element level
    (add
     "Add an element to the collection. For linear collections, should add to
the front or back, depending on which is natural for the underlying data structure."
     (:a -> :m :a -> :m :a))
    (remove-elt
     "Remove all occurrences of `elt` from the collection."
    (Eq :a => :a -> :m :a -> :m :a)))

  (define-class (Collection :m => ImmutableCollection :m)
    "An immutable collection.")

  (define-class (Collection :m => MutableCollection :m)
    "A mutable collection."
    (copy
     "Create a shallow copy of the collection."
     (:m :a -> :m :a))
    (add!
     "Add an element to the collection in place. See `add`."
     (:a -> :m :a -> :m :a))))

;; TODO: Make it so that these must all be the proper KeyedCollection types as well
(coalton-toplevel
  (define-class (Collection :m => LinearCollection :m)
    (head
     "Return the first element of the collection."
     (:m :a -> Optional :a))
    (head#
     "Return the first element of the collection, erroring if it does not exist."
     (:m :a -> :a))
    (last
     "Return the last element of the collection."
     (:m :a -> Optional :a))
    (last#
     "Return the last element of the collection, erroring if it does not exist."
     (:m :a -> :a))
    (index-elt
     "Return the index of the first occurence of `elt`, if it can be found."
     (Eq :a => :a -> :m :a -> Optional UFix))
    (index-elt#
     "Return the index of the first occurence of `elt`, erroring if it cannot be found."
     (Eq :a => :a -> :m :a -> UFix))
    (index-where
     "Return the index of the first element matching a predicate function."
     ((:a -> Boolean) -> :m :a -> Optional UFIx))
    (index-where#
     "Return the index of the first element matching a predicate function, erroring if none can be found."
     ((:a -> Boolean) -> :m :a -> UFix))
    (find-where
     "Return the first element matching a predicate function."
     ((:a -> Boolean) -> :m :a -> Optional :a))
    )

  (define-class (LinearCollection :m => ImmutableLinearCollection :m))

  (define-class (LinearCollection :m => MutableLinearCollection :m)
    (reverse!
     "Reverse the collection in place. The sequence is returned for convenience."
     (:m :a -> :m :a))))

;; TODO: Because `List` is a predefined type, we can't define this
;; in the new collections/immutable/list.lisp file. Define it here
;; for now, and when we remove the deprecated versions move this
;; into the list package.
(coalton-toplevel
  (define-instance (Collection List)
    (define (new-collection)
      Nil)
    (define (new-repeat n elt)
      (l:repeat n elt))
    (define (new-from n f)
      (map f (l:range 0 (- n 1))))
    (define (flatten lst)
      (>>= lst (fn (x) x)))
    (define filter l:filter)
    (define remove-duplicates l:remove-duplicates)
    (define empty? l:empty?)
    (define length l:length)
    (define contains-elt? l:contains-elt?)
    (define contains-where? l:contains-where?)
    (define count-where l:countBy)
    (define add Cons)
    (define (remove-elt elt lst)
      (l:filter (/= elt) lst)))

  (define-instance (ImmutableCollection List)))

(coalton-toplevel
  (define-instance (LinearCollection List)
    (define head l:head)
    (define (head# lst)
      (o:from-some "Attempted to retrieve head of empty list." (l:head lst)))
    (define last l:last)
    (define (last# lst)
      (o:from-some "Attempted to retrieve last element of empty list." (l:last lst)))
    (define index-elt l:elemIndex)
    (define (index-elt# elt lst)
      (o:from-some "Cannot find element in list." (l:elemIndex elt lst)))
    (define index-where l:findIndex)
    (define (index-where# pred lst)
      (o:from-some "Cannot find matching element in list." (l:findIndex pred lst)))
    (define find-where l:find)
  ))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
