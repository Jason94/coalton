(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:shadow
   #:compile)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:analysis #:coalton-impl/analysis)
   (#:codegen #:coalton-impl/codegen))
  (:export
   #:*global-environment*
   #:*type-at-symbol-hook*              ; VARIABLE
   #:type-at-symbol-info                ; STRUCT
   #:type-at-symbol-info-name           ; ACCESSOR
   #:type-at-symbol-info-display-name   ; ACCESSOR
   #:type-at-symbol-info-category       ; ACCESSOR
   #:type-at-symbol-info-type           ; ACCESSOR
   #:type-at-symbol-info-type-string    ; ACCESSOR
   #:type-at-symbol-info-source         ; ACCESSOR
   #:type-at-symbol-info-source-name    ; ACCESSOR
   #:type-at-symbol-info-source-file-path ; ACCESSOR
   #:type-at-symbol-info-start          ; ACCESSOR
   #:type-at-symbol-info-end            ; ACCESSOR
   #:collect-translation-unit-type-at-symbol-info ; FUNCTION
   #:entry-point                        ; FUNCTION
   #:expression-entry-point             ; FUNCTION
   #:codegen                            ; FUNCTION
   #:compile                            ; FUNCTION
   #:compile-coalton-toplevel           ; FUNCTION
   #:compile-to-lisp                    ; FUNCTION
   ))

(in-package #:coalton-impl/entry)

(defvar *global-environment* (tc:make-default-environment))

(defvar *type-at-symbol-hook* nil
  "Hook called with one TYPE-AT-SYMBOL-INFO object for each typed symbol seen during compilation.

The hook runs after type checking and before analysis/code generation, while the
typed AST still carries source locations. IDE integrations can bind this to
collect ranges for hover/autodoc/eldoc queries.")

(defstruct (type-at-symbol-info
            (:copier nil))
  "IDE-oriented type information for one source occurrence of a Coalton symbol.

START and END are zero-based character offsets into SOURCE. CATEGORY is one of
:DEFINITION, :BINDING, :PATTERN, or :REFERENCE. NAME is the
compiler's identifier after renaming; DISPLAY-NAME preserves the source spelling
when it is available. TYPE is the qualified type object and TYPE-STRING is its
printer-friendly representation in ENV."
  (name nil :read-only t)
  (display-name nil :type (or null string) :read-only t)
  (category nil :type keyword :read-only t)
  (type nil :read-only t)
  (type-string nil :type (or null string) :read-only t)
  (source nil :read-only t)
  (source-name nil :type (or null string) :read-only t)
  (source-file-path nil :type (or null string) :read-only t)
  (start nil :type (or null fixnum) :read-only t)
  (end nil :type (or null fixnum) :read-only t))

(defun symbol-source-text (location)
  (when location
    (let ((source (source:location-source location))
          (span (source:location-span location)))
      (when (and source span)
        (source:extract-source-text source span)))))

(defun narrow-location-to-symbol (location name)
  "Return a smaller source location around NAME within LOCATION, plus source spelling.

Typed pattern constructor locations usually cover the whole pattern form, e.g.
`(Asteroid)`, but the IDE wants the constructor token itself. This helper is
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

(defun make-type-at-symbol-info* (name type location env category &optional display-name)
  (when (and location type)
    (let* ((source (source:location-source location))
           (span (source:location-span location)))
      (make-type-at-symbol-info
       :name name
       :display-name (or display-name (symbol-source-text location))
       :category category
       :type type
       :type-string (tc:type-to-string type env)
       :source source
       :source-name (and source (source:source-name source))
       :source-file-path (and source (source:source-file-path source))
       :start (and span (source:span-start span))
       :end (and span (source:span-end span))))))

(defun maybe-emit-type-at-symbol-info (info results)
  (when info
    (when *type-at-symbol-hook*
      (funcall *type-at-symbol-hook* info))
    (push info results))
  results)

(defun collect-pattern-type-at-symbol-info (pattern env results)
  (typecase pattern
    (tc:pattern-var
     (maybe-emit-type-at-symbol-info
      (make-type-at-symbol-info* (tc:pattern-var-name pattern)
                                  (tc:pattern-type pattern)
                                  (source:location pattern)
                                  env
                                  :pattern
                                  (symbol-name (tc:pattern-var-orig-name pattern)))
      results))
    (tc:pattern-binding
     (setf results (collect-pattern-type-at-symbol-info (tc:pattern-binding-var pattern) env results))
     (collect-pattern-type-at-symbol-info (tc:pattern-binding-pattern pattern) env results))
    (tc:pattern-constructor
     ;; The pattern constructor itself is a symbol occurrence too. Without
     ;; this, hovering a nullary constructor pattern like (Asteroid) falls
     ;; through to the nearest enclosing expression, often a whole macro/do
     ;; form with type Unit. Use PATTERN-TYPE and a narrowed source span; do
     ;; not look up or instantiate the constructor's value type here.
     (let ((name (tc:pattern-constructor-name pattern)))
       (multiple-value-bind (location display-name)
           (narrow-location-to-symbol (source:location pattern) name)
         (setf results
               (maybe-emit-type-at-symbol-info
                (make-type-at-symbol-info* name
                                            (tc:pattern-type pattern)
                                            (or location (source:location pattern))
                                            env
                                            :pattern-constructor
                                            (or display-name (symbol-name name)))
                results))))
     (dolist (child (tc:pattern-constructor-patterns pattern) results)
       (setf results (collect-pattern-type-at-symbol-info child env results))))
    (t results)))

(defun collect-patterns-type-at-symbol-info (patterns env results)
  (dolist (pattern patterns results)
    (setf results (collect-pattern-type-at-symbol-info pattern env results))))

(defun collect-definition-type-at-symbol-info (definition env results)
  (let ((name (tc:toplevel-define-name definition)))
    (setf results
          (maybe-emit-type-at-symbol-info
           (make-type-at-symbol-info* (tc:node-variable-name name)
                                       (tc:node-type name)
                                       (source:location name)
                                       env
                                       :definition)
           results))
    (setf results (collect-patterns-type-at-symbol-info (tc:toplevel-define-params definition) env results))
    (tc:traverse
     (tc:toplevel-define-body definition)
     (tc:make-traverse-block
      :variable
      (lambda (node)
        (setf results
              (maybe-emit-type-at-symbol-info
               (make-type-at-symbol-info* (tc:node-variable-name node)
                                           (tc:node-type node)
                                           (source:location node)
                                           env
                                           :reference)
               results))
        node)
      :bind
      (lambda (node)
        (setf results (collect-pattern-type-at-symbol-info (tc:node-bind-pattern node) env results))
        node)
      :values-bind
      (lambda (node)
        (setf results (collect-patterns-type-at-symbol-info (tc:node-values-bind-patterns node) env results))
        node)
      :abstraction
      (lambda (node)
        (setf results (collect-patterns-type-at-symbol-info (tc:node-abstraction-params node) env results))
        node)
      :let-binding
      (lambda (node)
        (let ((name (tc:node-let-binding-name node)))
          (setf results
                (maybe-emit-type-at-symbol-info
                 (make-type-at-symbol-info* (tc:node-variable-name name)
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
                (maybe-emit-type-at-symbol-info
                 (make-type-at-symbol-info* (tc:node-variable-name name)
                                             (tc:node-type name)
                                             (source:location name)
                                             env
                                             :binding)
                 results)))
        node)
      :match-branch
      (lambda (node)
        (setf results (collect-pattern-type-at-symbol-info (tc:node-match-branch-pattern node) env results))
        node)
      :catch-branch
      (lambda (node)
        (setf results (collect-pattern-type-at-symbol-info (tc:node-catch-branch-pattern node) env results))
        node)
      :resumable-branch
      (lambda (node)
        (setf results (collect-pattern-type-at-symbol-info (tc:node-resumable-branch-pattern node) env results))
        node)
      :do-bind
      (lambda (node)
        (setf results (collect-pattern-type-at-symbol-info (tc:node-do-bind-pattern node) env results))
        node)
      :loop
      (lambda (node)
        (dolist (binding (tc:node-for-bindings node))
          (let ((name (tc:node-for-binding-name binding)))
            (setf results
                  (maybe-emit-type-at-symbol-info
                   (make-type-at-symbol-info* (tc:node-variable-name name)
                                               (tc:node-type name)
                                               (source:location name)
                                               env
                                               :binding)
                   results))))
        node))))
  results)

(defun collect-instance-method-type-at-symbol-info (method env results)
  (collect-definition-type-at-symbol-info
   (tc:make-toplevel-define
    :name (tc:instance-method-definition-name method)
    :params (tc:instance-method-definition-params method)
    :keyword-params (tc:instance-method-definition-keyword-params method)
    :function-syntax-p (tc:instance-method-definition-function-syntax-p method)
    :body (tc:instance-method-definition-body method)
    :location (source:location method))
   env
   results))

(defun collect-instance-type-at-symbol-info (instance env results)
  (maphash (lambda (_name method)
             (declare (ignore _name))
             (setf results (collect-instance-method-type-at-symbol-info method env results)))
           (tc:toplevel-define-instance-methods instance))
  results)

(defun collect-translation-unit-type-at-symbol-info (translation-unit env)
  "Return all typed symbol occurrences in TRANSLATION-UNIT and call `*type-at-symbol-hook*' for each one.

This is intended as the compiler-side data source for SLIME/SLY/LSP hover and
eldoc integrations. The return value is sorted by source location."
  (let ((results nil))
    (dolist (definition (tc:translation-unit-definitions translation-unit))
      (setf results (collect-definition-type-at-symbol-info definition env results)))
    (dolist (instance (tc:translation-unit-instances translation-unit))
      (setf results (collect-instance-type-at-symbol-info instance env results)))
    (sort (nreverse results)
          (lambda (a b)
            (let ((a-source (type-at-symbol-info-source a))
                  (b-source (type-at-symbol-info-source b))
                  (a-start (or (type-at-symbol-info-start a) -1))
                  (b-start (or (type-at-symbol-info-start b) -1)))
              (if (eq a-source b-source)
                  (< a-start b-start)
                  (string< (or (type-at-symbol-info-source-name a) "")
                           (or (type-at-symbol-info-source-name b) ""))))))))


(defun entry-point (program)
  (declare (type parser:program program))

  (let ((*package* (parser:program-lisp-package program))

        (program (parser:rename-variables program))

        (env *global-environment*))

    (setf (parser:program-defines program)
          (tc:resolve-control-flow (parser:program-defines program)))
    (setf (parser:program-instances program)
          (tc:resolve-control-flow (parser:program-instances program)))

    (multiple-value-bind (type-definitions instances env)
        (tc:toplevel-define-type (parser:program-types program)
                                 (parser:program-structs program)
                                 (parser:program-type-aliases program)
                                 env)

      (multiple-value-bind (class-definitions env)
          (tc:toplevel-define-class (parser:program-classes program)
                                    env)

        (let ((all-instances
                (append instances
                        (parser:program-instances program)
                        (tc:derive-class-instances (parser:program-types program)
                                                   (parser:program-structs program)
                                                   env)))) 

          (multiple-value-bind (ty-instances env)
              (tc:toplevel-define-instance all-instances env)


            (multiple-value-bind (toplevel-definitions env)
                (tc:toplevel-define (parser:program-defines program)
                                    (parser:program-declares program)
                                    env)

              (multiple-value-bind (toplevel-instances)
                  (tc:toplevel-typecheck-instance ty-instances
                                                  all-instances
                                                  env)

                (setf env (tc:toplevel-specialize (parser:program-specializations program) env))

                (let ((monomorphize-table (make-hash-table :test #'eq))

                      (inline-p-table (make-hash-table :test #'eq))

                      (translation-unit
                        (tc:make-translation-unit
                         :types type-definitions
                         :definitions toplevel-definitions
                         :classes class-definitions
                         :instances toplevel-instances
                         :lisp-forms (parser:program-lisp-forms program)
                         :package *package*)))

                  (loop :for define :in (parser:program-defines program)
                        :when (parser:toplevel-define-monomorphize define)
                          :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-define-inline define)
                          :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                             inline-p-table)
                                    t))

                  (loop :for declare :in (parser:program-declares program)
                        :when (parser:toplevel-declare-monomorphize declare)
                          :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-declare-inline declare)
                          :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                             inline-p-table)
                                    t))

                  (loop :for ty-instance :in ty-instances
                        :for method-codegen-inline-p := (tc:ty-class-instance-method-codegen-inline-p ty-instance)
                        :do (loop :for (method-codegen-sym . inline-p) :in method-codegen-inline-p
                                  :do (when inline-p (setf (gethash method-codegen-sym inline-p-table) t))))

                  (collect-translation-unit-type-at-symbol-info translation-unit env)

                  (analysis:analyze-translation-unit translation-unit env)

                  (codegen:compile-translation-unit translation-unit monomorphize-table inline-p-table env))))))))))


(defun expression-entry-point (node)
  (declare (type parser:node node))

  (let ((env *global-environment*))

    (multiple-value-bind (ty preds accessors node subs)
        (tc:infer-expression-type (tc:resolve-control-flow
                                   (parser:rename-variables node))
                                  (tc:make-variable :kind tc:+kstar+ :allow-result-p t)
                                  nil
                                  (tc:make-tc-env :env env))

      (multiple-value-bind (preds subs)
          (tc:solve-fundeps env preds subs)

        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (tc:solve-accessors accessors env)
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (tc:tc-error "Ambiguous accessor"
                         (source:note (first accessors)
                                      "accessor is ambiguous")))

          (let* ((preds (tc:reduce-context env preds subs))
                 (subs (tc:compose-substitution-lists
                        (tc:default-subs env nil preds)
                        subs))
                 (preds (tc:reduce-context env preds subs))

                 (node (tc:apply-substitution subs node))
                 (ty (tc:apply-substitution subs ty))

                 (qual-ty (tc:qualify preds ty))
                 (scheme (tc:quantify (tc:type-variables qual-ty) qual-ty)))

            (when (null preds)
              (return-from expression-entry-point
                (let ((node (codegen:optimize-node
                             (codegen:translate-expression node nil env)
                             env)))
                  (codegen:codegen-expression
                   (codegen:direct-application
                    node
                    (codegen:make-function-table env))
                   env))))

            (tc:tc-error "Unable to codegen"
                         (tc:tc-note node
                                     "expression has ambiguous type ~A"
                                     (tc:type-to-string scheme env))
                         (tc:tc-note node
                                     "Add a type assertion with THE to resolve ambiguity"))))))))

(defmacro with-environment-updates (updates &body body)
  "Collect environment updates into a vector bound to UPDATES."
  `(let* ((,updates (make-array 0 :adjustable t :fill-pointer 0))
          (tc:*update-hook* (lambda (name arg-list)
                              (vector-push-extend (cons name arg-list) ,updates))))
     ,@body))

(defun make-environment-updater (update-log)
  "Produce source form of the contents of an environment UPDATE-LOG (i.e., calls to functions in typechecker/environment)."
  (let ((updates (remove-duplicates (coerce update-log 'list) :test #'code-update-eql)))
    `(let ((env *global-environment*))
       ,@(loop :for (fn . args) :in updates
               :collect `(setf env (,fn env ,@(mapcar #'util:runtime-quote args))))
       (setf *global-environment* env)
       (tc:synchronize-type-variable-counter env))))

(defun compile-coalton-toplevel (program)
  "Compile PROGRAM and return a single form suitable for direct inclusion by Lisp compiler. For implementing coalton-toplevel macro."
  (with-environment-updates updates
    (multiple-value-bind (program env)
        (entry-point program)
      (setf *global-environment* env)
      `(progn
         ,(make-prologue)
         ,(make-environment-updater updates)
         ,program))))

(defun code-update-eql (a b)
  "Compare environment updates, returning t for set-code updates of the same symbol."
  (and (eql (first a) 'coalton-impl/typechecker/environment:set-code)
       (eql (first b) 'coalton-impl/typechecker/environment:set-code)
       (eql (second a)
            (second b))))

(defun make-prologue ()
  "Return source form of an assertion that prevents mixing development and release modes."
  `(eval-when (:load-toplevel)
     (unless (eq (settings:coalton-release-p) ,(settings:coalton-release-p))
       ,(if (settings:coalton-release-p)
            `(error "~A was compiled in release mode but loaded in development."
                    ,(or *compile-file-pathname* *load-truename*))
            `(error "~A was compiled in development mode but loaded in release."
                    ,(or *compile-file-pathname* *load-truename*))))))

(defun print-form (stream form &optional (package "CL"))
  "Print a FORM to a STREAM, separated by 2 lines."
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*print-case* ':downcase)
          ;; *print-circle* t allows gensym-generated, uninterned
          ;; *symbols to serve as variables in readable source.
          (*print-circle* t)
          (*print-pretty* t)
          (*print-right-margin* 80))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defun compile-to-lisp (source output)
  "Read Coalton source from SOURCE and write Lisp source to OUTPUT. NAME may be the filename related to the input stream."
  (declare (optimize (debug 3)))
  (with-open-stream (stream (source:source-stream source))
    (parser:with-reader-context stream
      (with-environment-updates updates
        (let* ((program (parser:read-program stream source ':file))
               (program-text (entry-point program))
               (program-package (parser:program-package program))
               (package-name (parser:toplevel-package-name program-package)))
          (print-form output (make-prologue))
          (print-form output (make-environment-updater updates))
          (print-form output (parser:make-defpackage program-package))
          (print-form output `(in-package ,package-name))
          ;; coalton-impl/codegen/program:compile-translation-unit wraps
          ;; definitions in progn to provide a single expression as the
          ;; macroexpansion of coalton-toplevel: unwrap for better
          ;; readability
          (dolist (form (cdr program-text))
            (print-form output form package-name)))))))

(defun codegen (source)
  "Compile Coalton source from SOURCE and return Lisp program text. NAME may be the filename related to the input stream."
  (with-output-to-string (output)
    (compile-to-lisp source output)))

(defun compile (source &key (load t) (output-file nil))
   "Compile Coalton code in SOURCE, returning the pathname of the generated .fasl file. If OUTPUT-FILE is nil, the built-in compiler default output location will be used."
   (uiop:with-temporary-file (:stream lisp-stream
                              :pathname lisp-file
                              :type "lisp"
                              :direction ':output)
     (compile-to-lisp source lisp-stream)
     :close-stream
     (cond ((null output-file)
            (setf output-file (compile-file lisp-file)))
           (t
            (compile-file lisp-file :output-file output-file)))
     (when load
       (load output-file))
     output-file))
