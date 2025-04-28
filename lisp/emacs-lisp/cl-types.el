;; -*- lexical-binding: t; -*-

;; Data types defined by `cl-deftype' are now recognized as argument
;; types for dispatching generic functions methods.

;; Will be removed when included in cl-lib.
(require 'cl-lib)
(eval-when-compile (require 'cl-macs))  ;For cl--find-class.

;; Extend `cl-deftype' to define data types which are also valid
;; argument types for dispatching generic function methods (see also
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=77725>).
;;
;; The main entry points are:
;;
;; - `cl-deftype', that defines new data types.
;;
;; - `cl-types-of', that returns the types an object belongs to.

(defvar cl--type-list nil
  "List of defined types to lookup for method dispatching.")

;; FIXME: The `cl-deftype-handler' property should arguably be turned
;; into a field of this struct (but it has performance and
;; compatibility implications, so let's not make that change for now).
(cl-defstruct
    (cl-type-class
     (:include cl--class)
     (:noinline t)
     (:constructor nil)
     (:constructor cl--type-class-make
                   (name
                    docstring
                    parent-types
                    &aux (parents
                          (mapcar
                           (lambda (type)
                             (or (cl--find-class type)
                                 (error "Unknown type: %S" type)))
                           parent-types))))
     (:copier nil))
  "Type descriptors for types defined by `cl-deftype'.")

(defun cl--type-p (object)
  "Return non-nil if OBJECT is a used defined type.
That is, a type of class `cl-type-class'."
  (and (symbolp object) (cl-type-class-p (cl--find-class object))))

(defmacro cl--type-parents (name)
  "Get parents of type with NAME.
NAME is a symbol representing a type."
  `(cl--class-allparents (cl--find-class ,name)))

(defun cl--type-children (name)
  "Get children of the type with NAME.
NAME is a symbol representing a type.
Return a possibly empty list of types."
  (cl-check-type name (satisfies cl--type-p))
  (let (children)
    (dolist (elt cl--type-list)
      (or (eq name elt)
          (if (memq name (cl--type-parents elt))
              (push elt children))))
    children))

(defun cl--type-dag ()
  "Return a DAG from the list of defined types."
  (mapcar (lambda (type) (cl--type-parents type)) cl--type-list))

;; Keep it for now, for testing.
(defun cl--type-undefine (name)
  "Remove the definitions of type with NAME.
NAME is an unquoted symbol representing a type.
Signal an error if other types inherit from NAME."
  (declare-function cl-remprop "cl-extra" (symbol propname))
  (cl-check-type name (satisfies cl--type-p))
  (when-let* ((children (and (cl--type-p name)
                             (cl--type-children name))))
    (error "Type has children: %S" children))
  (cl-remprop name 'cl--class)
  (cl-remprop name 'cl-deftype-handler)
  (setq cl--type-list (delq name cl--type-list)))

(defun cl--type-deftype (name parents &optional docstring)
  "Generalize type with NAME for method dispatching.
PARENTS is a list of types NAME is a subtype of, or nil.
DOCSTRING is an optional documentation string."
  (let ((oldtlist (copy-sequence cl--type-list))
        (oldplist (copy-sequence (symbol-plist name))))
    (condition-case err
        (let* ((class (cl--find-class name))
               (recorded (memq name cl--type-list)))
          (if (null class)
              (or (null recorded)
                  (error "Type generalized, but doesn't exist"))
            (or recorded (error "Type exists, but not generalized"))
            (or (cl-type-class-p class)
                (error "Type in another class: %S" (type-of class))))
          (if (memq name parents)
              (error "Type in parents: %S" parents))
          ;; Setup a type descriptor for NAME.
          (setf (cl--find-class name)
                (cl--type-class-make name docstring parents))
          (if recorded
              ;; Clear any previous error mark.
              (cl-remprop name 'cl--type-error)
            ;; Record new type to include its dependency in the DAG.
            (push name cl--type-list))
          ;; `cl-types-of' iterates through all known types to collect
          ;; all those an object belongs to, sorted from the most
          ;; specific type to the more general type.  So, keep the
          ;; global list in this order.
          (setq cl--type-list
                (merge-ordered-lists
                 (cl--type-dag)
                 (lambda (_) (error "Invalid dependency graph")))))
      (error
       ;; On error restore previous data.
       (setq cl--type-list oldtlist)
       (setf (symbol-plist name) oldplist)
       (error (format "Define %S failed: %s"
                      name (error-message-string err)))))))

;;;###autoload
(defmacro cl-deftype2 (name arglist &rest body)
  "Define NAME as a new data type.
The type NAME can then be used in `cl-typecase', `cl-check-type',
etc., and as argument type for dispatching generic function methods.

ARGLIST is a Common Lisp argument list of the sort accepted by
`cl-defmacro'.  BODY forms are evaluated and should return a type
specifier that is equivalent to the type (see the Info node `(cl) Type
Predicates' in the GNU Emacs Common Lisp Emulation manual).

If there is a `declare' form in BODY, the spec (parents PARENTS) is
recognized to specify a list of types NAME is a subtype of.  For
instance:

  (cl-deftype2 unsigned-byte (&optional bits)
    \"Unsigned integer.\"
    (list \\='integer 0 (if (eq bits \\='*) bits (1- (ash 1 bits)))))

  (cl-deftype2 unsigned-8bits ()
    \"Unsigned 8-bits integer.\"
    (declare (parents unsigned-byte))
    \\='(unsigned-byte 8))

The list of PARENTS types determines the order of methods invocation,
and missing PARENTS may cause incorrect ordering of methods, while
extraneous PARENTS may cause use of extraneous methods.

If PARENTS is non-nil, ARGLIST must be nil."
  (declare (debug cl-defmacro) (doc-string 3) (indent 2))
  (pcase-let*
      ((`(,decls . ,forms) (macroexp-parse-body body))
       (docstring (if (stringp (car decls))
                      (car decls)
                    (cadr (assq :documentation decls))))
       (parents (cdr (assq 'parents (cdr (assq 'declare decls))))))
    (and parents arglist
         (error "Parents specified, but arglist not empty"))
    (if docstring (setq forms (cons docstring forms)))
    `(eval-and-compile ;;cl-eval-when (compile load eval)
       (cl--type-deftype ',name ',parents ,docstring)
       (define-symbol-prop ',name 'cl-deftype-handler
                           (cl-function
                            (lambda (&cl-defs ('*) ,@arglist)
                              ,@forms))))))

;; Ensure each type satisfies `eql'.
(defvar cl--type-unique (make-hash-table :test 'equal)
  "Record an unique value of each type.")

(defun cl--type-error (type error)
  "Mark TYPE as in-error, and report the produced ERROR value."
  (put type 'cl--type-error error) ;; Mark TYPE as in-error.
  ;; Temporarily raise the recursion limit to avoid another recursion
  ;; error while reporting ERROR.
  (let ((max-lisp-eval-depth (+ 800 max-lisp-eval-depth)))
    (warn  "cl-types-of %s, %s" type (error-message-string error)))
  nil)

;; FIXME: `cl-types-of' CPU cost is proportional to the number of types
;; defined with `cl-deftype', so the more popular it gets, the slower
;; it becomes.  And of course, the cost of each type check is
;; unbounded, so a single "expensive" type can slow everything down
;; further.
;;
;; The usual dispatch is
;;
;;   (lambda (arg &rest args)
;;     (let ((f (gethash (cl-typeof arg) precomputed-methods-table)))
;;       (if f
;;           (apply f arg args)
;;         ;; Slow case when encountering a new type
;;         ...)))
;;
;; where often the most expensive part is `&rest' (which has to
;; allocate a list for those remaining arguments),
;;
;; So we're talking about replacing
;;
;;   &rest + cl-type-of + gethash + if + apply
;;
;; with a function that loops over N types, calling `cl-typep' on each
;; one of them (`cl-typep' itself being a recursive function that
;; basically interprets the type language).  This is going to slow
;; down dispatch very significantly for those generic functions that
;; have a method that dispatches on a user defined type, compared to
;; those that don't.
;;
;; A possible further improvement:
;;
;; - based on the PARENTS declaration, create a map from builtin-type
;;   to the set of cl-types that have that builtin-type among their
;;   parents.  That presumes some PARENTS include some builtin-types,
;;   obviously otherwise the map will be trivial with all cl-types
;;   associated with the `t' "dummy parent".  [ We could even go crazy
;;   and try and guess PARENTS when not provided, by analyzing the
;;   type's definition. ]
;;
;; - in `cl-types-of' start by calling `cl-type-of', then use the map
;;   to find which cl-types may need to be checked.
;;
(defun cl-types-of (object)
  "Return the types OBJECT belongs to.
Return an unique list of types OBJECT belongs to, ordered from the
most specific type to the most general."
  (let ((found (list (cl--type-parents (cl-type-of object)))))
    ;; Build a DAG of all types OBJECT belongs to.
    (dolist (type cl--type-list)
      (and
       ;; Skip type, if it previously produced an error.
       (null (get type 'cl--type-error))
       ;; Skip type not defined by `cl-deftype'.
       (cl-type-class-p (cl--find-class type))
       ;; If BAR is declared as a parent of FOO and `cl-types-of' has
       ;; already decided that the value is of type FOO, then we
       ;; already know BAR will be in the output anyway and there's no
       ;; point testing BAR.  So, skip type already selected as parent
       ;; of another type, assuming that, most of the time, `assq'
       ;; will be faster than `cl-typep'.
       (null (assq type found))
       ;; If OBJECT is of type, add type and its parents to the DAG.
       (condition-case e
           (cl-typep object type)
         (error (cl--type-error type e)))
       ;; (dolist (p (cl--type-parents type))
       ;;   (push (cl--type-parents p) found))
       ;; Equivalent to the `dolist' above, but faster: avoid to
       ;; recompute several lists of parents we already know.
       (let ((pl (cl--type-parents type)))
         (while pl
           (push pl found)
           (setq pl (cdr pl))))))
    ;; Compute an ordered list of types from the collected DAG.
    (setq found (merge-ordered-lists found))
    ;; Return an unique value of this list of types, which is also the
    ;; list of specifiers for this type.
    (with-memoization (gethash found cl--type-unique)
      found)))

;;; Method dispatching
;;
(cl-generic-define-generalizer cl--type-generalizer
  20 ;; "typeof" < "cl-types-of" < "head" priority
  (lambda (obj &rest _) `(cl-types-of ,obj))
  (lambda (tag &rest _) (if (consp tag) tag)))

(cl-defmethod cl-generic-generalizers :extra "cl-types-of" (type)
  "Support for dispatch on types."
  (if (cl--type-p type)
      (list cl--type-generalizer)
    (cl-call-next-method)))

(provide 'cl-types)

;;; cl-types.el ends here
