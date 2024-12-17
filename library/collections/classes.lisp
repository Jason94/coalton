(coalton-library/utils:defstdlib-package #:coalton-library/collections/classes
  (:use
   #:coalton
   #:coalton-library/classes)
  (:local-nicknames
   (#:types #:coalton-library/types))
  (:export
   ))

(in-package #:coalton-library/collections/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; TODO: Figure out what syntax for this:
  ;; (define-class ((Monoid :m) (Monad :m) => (Collection :m :a))
  (define-class (Collection :m)
    "Types that contain a collection of elements of another type.

Does not have an ordering of elements.

Could be mutable or immutable. All methods are allowed to modify the
underlying collection. If you need immutablility as part of the contract,
use one of the Immutable collection typeclasses."
    ;; Create new collections
    (new
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
     (Eq :a => :m :a -> Boolean))
    (contains-where?
     "Check if the collection contanis an element satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> Boolean))
    (count-where
     "The number of elements satisfying the predicate."
     ((:a -> Boolean) -> :m :a -> UFix))
    ;; Manipulate at the element level
    (add
     "Add an element to the collection. For linear collections, should add to
the front or back, depending on which is natural for the underlying data structure."
     (:a -> :m :a -> :m :a)))

  (define-class (Collection :m => ImmutableCollection :m)
    "An immutable collection.")

  (define-class (Collection :m => MutableCollection :m)
    "A mutable collection."
    (copy
     "Create a shallow copy of the collection."
     (:m :a -> :m :a))
    (filter!
     "Filter the collection in place."
     ((:a -> Boolean) -> :m :a -> :m :a))
    (add!
     "Add an element to the collection in place. See `add`."
     (:a -> :m :a -> :m :a))))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/CLASSES")
