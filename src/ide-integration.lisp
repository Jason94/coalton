(defpackage #:coalton-impl/ide-integration
  (:use
   #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:*symbol-hook*                        ; VARIABLE
   #:ide-integration-p                            ; FUNCTION
   #:symbol-info                          ; STRUCT
   #:symbol-info-name                     ; ACCESSOR
   #:symbol-info-display-name             ; ACCESSOR
   #:symbol-info-category                 ; ACCESSOR
   #:symbol-info-type                     ; ACCESSOR
   #:symbol-info-type-string              ; ACCESSOR
   #:symbol-info-docstring                ; ACCESSOR
   #:symbol-info-source                   ; ACCESSOR
   #:symbol-info-source-name              ; ACCESSOR
   #:symbol-info-source-file-path         ; ACCESSOR
   #:symbol-info-start                    ; ACCESSOR
   #:symbol-info-end                      ; ACCESSOR
   #:collect-translation-unit-symbol-info ; FUNCTION
   ))

(in-package #:coalton-impl/ide-integration)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defvar *symbol-hook* nil
  "Hook called with one SYMBOL-INFO object for each typed symbol seen during compilation.

The hook runs after type checking and before analysis/code generation, while the
typed AST still carries source locations. IDE integrations can bind this to
collect ranges for hover/documentation queries.")

(defun ide-integration-p ()
  "Returns T if ide integration to collect type info is hooked."
  (not (null *symbol-hook*)))

(defstruct (symbol-info
            (:copier nil))
  "IDE-oriented type information for one source occurrence of a Coalton symbol.

START and END are zero-based character offsets into SOURCE. CATEGORY is one of
:DEFINITION, :BINDING, :PATTERN, or :REFERENCE. NAME is the compiler's identifier
after renaming; DISPLAY-NAME preserves the source spelling when it is available.
TYPE is the qualified type object and TYPE-STRING is its printer-friendly
representation in ENV."
  (name             nil                        :read-only t)
  (display-name     nil :type (or null string) :read-only t)
  (category         nil :type keyword          :read-only t)
  (type             nil                        :read-only t)
  (type-string      nil :type (or null string) :read-only t)
  (docstring        nil :type (or null string) :read-only t)
  (source           nil                        :read-only t)
  (source-name      nil :type (or null string) :read-only t)
  (source-file-path nil :type (or null string) :read-only t)
  (start            nil :type (or null fixnum) :read-only t)
  (end              nil :type (or null fixnum) :read-only t))

(defun symbol-source-text (location)
  (when location
    (let ((source (source:location-source location))
          (span (source:location-span location)))
      (when (and source span)
        (source:extract-source-text source span)))))

(defun narrow-location-to-symbol (location name)
  "Return a smaller source location around NAME within LOCATION, plus source spelling.

Typed pattern constructor locations usually cover the whole pattern form, e.g.
`(Some 1)`, but the IDE wants the constructor token itself. This helper is
purely source-location bookkeeping; it does not query or mutate the typechecker
environment."
  (let* ((source (and location (source:location-source location)))
         (span (and location (source:location-span location)))
         (token (and name (symbol-name name)))
         (text (and source span token (source:extract-source-text source span))))
    (when (and source span token text)
      (let ((relative-start (search token text :test #'char-equal)))
        (when relative-start
          (let* ((absolute-start (+ (source:span-start span) relative-start))
                 (absolute-end (+ absolute-start (length token)))
                 (display-name (subseq text relative-start (+ relative-start (length token)))))
            (values (source:make-location source (cons absolute-start absolute-end))
                    display-name)))))))

(defun make-symbol-info* (name type location env category &key display-name docstring)
  (when (and location type)
    (let* ((source (source:location-source location))
           (span (source:location-span location)))
      (make-symbol-info
       :name name
       :display-name (or display-name (symbol-source-text location))
       :category category
       :type type
       :type-string (tc:type-to-string type env)
       :docstring docstring
       :source source
       :source-name (and source (source:source-name source))
       :source-file-path (and source (source:source-file-path source))
       :start (and span (source:span-start span))
       :end (and span (source:span-end span))))))

(defun maybe-emit-symbol-info (info results)
  (when info
    (when *symbol-hook*
      (funcall *symbol-hook* info))
    (push info results))
  results)

(defun collect-pattern-symbol-info (pattern env results)
  (typecase pattern
    (tc:pattern-var
     (maybe-emit-symbol-info
      (make-symbol-info* (tc:pattern-var-name pattern)
                                  (tc:pattern-type pattern)
                                  (source:location pattern)
                                  env
                                  :pattern
                                  :display-name (symbol-name (tc:pattern-var-orig-name pattern)))
      results))
    (tc:pattern-binding
     (setf results (collect-pattern-symbol-info (tc:pattern-binding-var pattern) env results))
     (collect-pattern-symbol-info (tc:pattern-binding-pattern pattern) env results))
    (tc:pattern-constructor
     ;; The pattern constructor itself is a symbol occurrence too. Without
     ;; this, hovering a nullary constructor pattern like (None) falls
     ;; through to the nearest enclosing expression, often a whole macro/do
     ;; form with type Unit. Use PATTERN-TYPE and a narrowed source span; do
     ;; not look up or instantiate the constructor's value type here.
     (let ((name (tc:pattern-constructor-name pattern)))
       (multiple-value-bind (location display-name)
           (narrow-location-to-symbol (source:location pattern) name)
         (setf results
               (maybe-emit-symbol-info
                (make-symbol-info* name
                                            (tc:pattern-type pattern)
                                            (or location (source:location pattern))
                                            env
                                            :pattern-constructor
                                            :display-name (or display-name (symbol-name name)))
                results))))
     (dolist (child (tc:pattern-constructor-patterns pattern) results)
       (setf results (collect-pattern-symbol-info child env results))))
    (t results)))

(defun collect-patterns-symbol-info (patterns env results)
  (dolist (pattern patterns results)
    (setf results (collect-pattern-symbol-info pattern env results))))

(defun name-docstring (name env)
  "Return the docstring registered for NAME in the typechecker name environment."
  (let ((entry (and name
                    (tc:lookup-name env name :no-error t))))
    (when entry
      (source:docstring entry))))

(defun definition-docstring (definition env)
  "typechecker/toplevel-define's don't contain a docstring or link to
the parser object, so retrieve it from the env table."
  (name-docstring
   (tc:node-variable-name
    (tc:toplevel-define-name definition))
   env))

(defun collect-definition-symbol-info (definition env results)
  (let ((name (tc:toplevel-define-name definition)))
    (setf results
          (maybe-emit-symbol-info
           (make-symbol-info* (tc:node-variable-name name)
                              (tc:node-type name)
                              (source:location name)
                              env
                              :definition
                              :docstring (definition-docstring definition env))
           results))
    (setf results (collect-patterns-symbol-info (tc:toplevel-define-params definition) env results))
    (tc:traverse
     (tc:toplevel-define-body definition)
     (tc:make-traverse-block
      :variable
      (lambda (node)
        (let ((name (tc:node-variable-name node)))
          (setf results
                (maybe-emit-symbol-info
                 (make-symbol-info* name
                                    (tc:node-type node)
                                    (source:location node)
                                    env
                                    :reference
                                    :docstring (name-docstring name env))
                 results))
          node))
      :bind
      (lambda (node)
        (setf results (collect-pattern-symbol-info (tc:node-bind-pattern node) env results))
        node)
      :values-bind
      (lambda (node)
        (setf results (collect-patterns-symbol-info (tc:node-values-bind-patterns node) env results))
        node)
      :abstraction
      (lambda (node)
        (setf results (collect-patterns-symbol-info (tc:node-abstraction-params node) env results))
        node)
      :let-binding
      (lambda (node)
        (let ((name (tc:node-let-binding-name node)))
          (setf results
                (maybe-emit-symbol-info
                 (make-symbol-info* (tc:node-variable-name name)
                                             (tc:node-type name)
                                             (source:location name)
                                             env
                                             :binding)
                 results)))
        node)
      :dynamic-binding
      (lambda (node)
        (let ((name (tc:node-dynamic-binding-name node)))
          (setf results
                (maybe-emit-symbol-info
                 (make-symbol-info* (tc:node-variable-name name)
                                             (tc:node-type name)
                                             (source:location name)
                                             env
                                             :binding)
                 results)))
        node)
      :match-branch
      (lambda (node)
        (setf results (collect-pattern-symbol-info (tc:node-match-branch-pattern node) env results))
        node)
      :catch-branch
      (lambda (node)
        (setf results (collect-pattern-symbol-info (tc:node-catch-branch-pattern node) env results))
        node)
      :resumable-branch
      (lambda (node)
        (setf results (collect-pattern-symbol-info (tc:node-resumable-branch-pattern node) env results))
        node)
      :do-bind
      (lambda (node)
        (setf results (collect-pattern-symbol-info (tc:node-do-bind-pattern node) env results))
        node)
      :loop
      (lambda (node)
        (dolist (binding (tc:node-for-bindings node))
          (let ((name (tc:node-for-binding-name binding)))
            (setf results
                  (maybe-emit-symbol-info
                   (make-symbol-info* (tc:node-variable-name name)
                                               (tc:node-type name)
                                               (source:location name)
                                               env
                                               :binding)
                   results))))
        node))))
  results)

(defun collect-instance-method-symbol-info (method env results)
  (collect-definition-symbol-info
   (tc:make-toplevel-define
    :name (tc:instance-method-definition-name method)
    :params (tc:instance-method-definition-params method)
    :keyword-params (tc:instance-method-definition-keyword-params method)
    :function-syntax-p (tc:instance-method-definition-function-syntax-p method)
    :body (tc:instance-method-definition-body method)
    :location (source:location method))
   env
   results))

(defun collect-instance-symbol-info (instance env results)
  (maphash (lambda (_name method)
             (declare (ignore _name))
             (setf results (collect-instance-method-symbol-info method env results)))
           (tc:toplevel-define-instance-methods instance))
  results)

(defun collect-translation-unit-symbol-info (translation-unit env)
  "Return all typed symbol occurrences in TRANSLATION-UNIT and call `*symbol-hook*' for each one.

This is intended as the compiler-side data source for SLIME/SLY/LSP hover and
eldoc integrations. The return value is sorted by source location."
  (declare (type tc:translation-unit translation-unit))
  (let ((results nil))
    (dolist (definition (tc:translation-unit-definitions translation-unit))
      (setf results (collect-definition-symbol-info definition env results)))
    (dolist (instance (tc:translation-unit-instances translation-unit))
      (setf results (collect-instance-symbol-info instance env results)))
    (sort (nreverse results)
          (lambda (a b)
            (let ((a-source (symbol-info-source a))
                  (b-source (symbol-info-source b))
                  (a-start (or (symbol-info-start a) -1))
                  (b-start (or (symbol-info-start b) -1)))
              (if (eq a-source b-source)
                  (< a-start b-start)
                  (string< (or (symbol-info-source-name a) "")
                           (or (symbol-info-source-name b) ""))))))))
