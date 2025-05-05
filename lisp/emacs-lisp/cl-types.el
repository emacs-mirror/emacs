;; -*- lexical-binding: t; -*-

;; Data types defined by `cl-deftype' are now recognized as argument
;; types for dispatching generic functions methods.

;; Needed until merged in existing libraries.
(require 'cl-lib)
(eval-when-compile (require 'cl-macs))  ;For cl--find-class.
(declare-function cl-remprop "cl-extra" (symbol propname))
(declare-function cl--class-children "cl-extra" (class))

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
  "Precedence list of the defined cl-types.")

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
  "Return non-nil if OBJECT is a cl-type.
That is, a type defined by `cl-deftype', of class `cl-type-class'."
  (and (symbolp object) (cl-type-class-p (cl--find-class object))))

(defun cl--type-deftype (name parents arglist &optional docstring)
  "Register cl-type with NAME for method dispatching.
PARENTS is a list of types NAME is a subtype of, or nil.
DOCSTRING is an optional documentation string."
  (let* ((class (cl--find-class name)))
    (when class
      (or (cl-type-class-p class)
          ;; FIXME: We have some uses `cl-deftype' in Emacs that
          ;; "complement" another declaration of the same type,
          ;; so maybe we should turn this into a warning (and
          ;; not overwrite the `cl--find-class' in that case)?
          (error "Type in another class: %S" (type-of class))))
    ;; Setup a type descriptor for NAME.
    (setf (cl--find-class name)
          (cl--type-class-make name docstring parents))
    ;; Record new type.  The constructor of the class
    ;; `cl-type-class' already ensures that parent types must be
    ;; defined before their "child" types (i.e. already added to
    ;; the `cl--type-list' for types defined with `cl-deftype').
    ;; So it is enough to simply push a new type at the beginning
    ;; of the list.
    ;; Redefinition is more complicated, because child types may
    ;; be in the list, so moving the type to the head can be
    ;; incorrect.  The "cheap" solution is to leave the list
    ;; unchanged (and hope the redefinition doesn't change the
    ;; hierarchy too much).
    ;; Side note: Redefinitions introduce other problems as well
    ;; because the class object's `parents` slot contains
    ;; references to `cl--class` objects, so after a redefinition
    ;; via (setf (cl--find-class FOO) ...), the children's
    ;; `parents` slots point to the old class object.  That's a
    ;; problem that affects all types and that we don't really try
    ;; to solve currently.
    (or (memq name cl--type-list)
        ;; Exclude types that can't be used without arguments.
        ;; They'd signal errors in `cl-types-of'!
        (not (memq (car arglist) '(nil &rest &optional &keys)))
        (push name cl--type-list))))

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
       (declares (assq 'declare decls))
       (parent-decl (assq 'parents (cdr declares)))
       (parents (cdr parent-decl)))
    (when parent-decl
      ;; "Consume" the `parents' declaration.
      (cl-callf (lambda (x) (delq parent-decl x)) (cdr declares))
      (when (equal declares '(declare))
        (cl-callf (lambda (x) (delq declares x)) decls)))
    (if (memq name parents)
        (error "Type in parents: %S" parents))
    (and parents arglist
         (error "Parents specified, but arglist not empty"))
    `(eval-and-compile ;;cl-eval-when (compile load eval)
       ;; FIXME: Where should `cl--type-deftype' go?  Currently, code
       ;; using `cl-deftype' can use (eval-when-compile (require
       ;; 'cl-lib)), so `cl--type-deftype' needs to go either to
       ;; `cl-preloaded.el' or it should be autoloaded even when
       ;; `cl-lib' is not loaded.
       (cl--type-deftype ',name ',parents ',arglist ,docstring)
       (define-symbol-prop ',name 'cl-deftype-handler
                           (cl-function
                            (lambda (&cl-defs ('*) ,@arglist)
                              ,@decls
                              ,@forms))))))

;; Ensure each type satisfies `eql'.
(defvar cl--type-unique (make-hash-table :test 'equal)
  "Record an unique value of each type.")

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
(defun cl-types-of (object &optional types)
  "Return the types OBJECT belongs to.
Return an unique list of types OBJECT belongs to, ordered from the
most specific type to the most general.
TYPES is an internal argument."
  (let* ((found nil))
    ;; Build a list of all types OBJECT belongs to.
    (dolist (type (or types cl--type-list))
      (and
       ;; If OBJECT is of type, add type to the matching list.
       (if types
           ;; For method dispatch, we don't need to filter out errors, since
           ;; we can presume that method dispatch is used only on
           ;; sanely-defined types.
           (cl-typep object type)
         (condition-case-unless-debug e
             (cl-typep object type)
           (error (setq cl--type-list (delq type cl--type-list))
                  (warn  "cl-types-of %S: %s"
                         type (error-message-string e)))))
       (push type found)))
    (push (cl-type-of object) found)
    ;; Return an unique value of the list of types OBJECT belongs to,
    ;; which is also the list of specifiers for OBJECT.
    (with-memoization (gethash found cl--type-unique)
      ;; Compute an ordered list of types from the DAG.
      (merge-ordered-lists
       (mapcar (lambda (type) (cl--class-allparents (cl--find-class type)))
               (nreverse found))))))

;;; Method dispatching
;;

(defvar cl--type-dispatch-list nil
  "List of types that need to be checked during dispatch.")

(cl-generic-define-generalizer cl--type-generalizer
  ;; FIXME: This priority can't be always right.  :-(
  ;; E.g. a method dispatching on a type like (or number function),
  ;; should take precedence over a method on `t' but not over a method
  ;; on `number'.  Similarly a method dispatching on a type like
  ;; (satisfies (lambda (x) (equal x '(A . B)))) should take precedence
  ;; over a method on (head 'A).
  ;; Fixing this 100% is impossible so this generalizer is condemned to
  ;; suffer from "undefined method ordering" problems, unless/until we
  ;; restrict it somehow to a subset that we can handle reliably.
  20 ;; "typeof" < "cl-types-of" < "head" priority
  (lambda (obj &rest _) `(cl-types-of ,obj cl--type-dispatch-list))
  (lambda (tag &rest _) (if (consp tag) tag)))

(cl-defmethod cl-generic-generalizers :extra "cl-types-of" (type)
  "Support for dispatch on cl-types."
  (if (cl--type-p type)
      (progn
        ;; Add a new dispatch type to the dispatch list, then
        ;; synchronize with `cl--type-list' so that both lists follow
        ;; the same type precedence order.
        ;; The `merge-ordered-lists' is `cl-types-of' should we make this
        ;; ordering unnecessary, but it's still handy for all those types
        ;; that don't declare their parents.
        (unless (memq type cl--type-dispatch-list)
          (setq cl--type-dispatch-list
                (seq-intersection cl--type-list
                                  (cons type cl--type-dispatch-list))))
        (list cl--type-generalizer))
    (cl-call-next-method)))

;;; Support for unloading.

;; Keep it for now, for testing.
(defun cl--type-undefine (name)
  "Remove the definition of cl-type with NAME.
NAME is an unquoted symbol representing a cl-type.
Signal an error if NAME has subtypes."
  (cl-check-type name (satisfies cl--type-p))
  (when-let* ((children (cl--class-children (cl--find-class name))))
    (error "Type has children: %S" children))
  (cl-remprop name 'cl--class)
  (cl-remprop name 'cl-deftype-handler)
  (setq cl--type-dispatch-list (delq name cl--type-dispatch-list))
  (setq cl--type-list (delq name cl--type-list)))

(provide 'cl-types)

;;; cl-types.el ends here
