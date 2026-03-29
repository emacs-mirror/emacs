;;; oclosure.el --- Open Closures       -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An OClosure is an object that combines the properties of records
;; with those of a function.  More specifically it is a function extended
;; with a notion of type (e.g. for defmethod dispatch) as well as the
;; ability to have some fields that are accessible from the outside.

;; See "Open closures", ELS'2022 (https://zenodo.org/record/6228797).

;; Here are some cases of "callable objects" where OClosures have found use:
;; - nadvice.el (the original motivation)
;; - kmacros (for cl-print and for `kmacro-extract-lambda')
;; - cl-generic: turn `cl--generic-isnot-nnm-p' into a mere type test
;;   (by putting the no-next-methods into their own class).
;; - Slot accessor functions, where the type-dispatch can be used to
;;   dynamically compute the docstring, and also to pretty print them.
;; - `save-some-buffers-function'
;; Here are other cases of "callable objects" where OClosures could be used:
;; - Use the type to distinguish macros from functions.
;; - Use a `name' and `depth' property from the function passed to
;;   `add-function' (or `add-hook') instead of passing it via "props".
;; - iterators (generator.el), thunks (thunk.el), streams (stream.el).
;; - PEG rules: they're currently just functions, but they should carry
;;   their original (macro-expanded) definition (and should be printed
;;   differently from functions)!
;; - auto-generate docstrings for cl-defstruct slot accessors instead of
;;   storing them in the accessor itself?
;; - SRFI-17's `setter'.
;; - coercion wrappers, as in "Threesomes, with and without blame"
;;   https://dl.acm.org/doi/10.1145/1706299.1706342, or
;;   "On the Runtime Complexity of Type-Directed Unboxing"
;;   https://sv.c.titech.ac.jp/minamide/papers.html
;; - An efficient `negate' operation such that
;;   (negate (negate f)) returns just `f' and (negate #'<) returns #'>=.
;; - Autoloads (tho currently our bytecode functions (and hence OClosures)
;;   are too fat for that).

;; Related constructs:
;; - `funcallable-standard-object' (FSO) in Common-Lisp.  These are different
;;   from OClosures in that they involve an additional indirection to get
;;   to the actual code, and that they offer the possibility of
;;   changing (via mutation) the code associated with
;;   an FSO.  Also the FSO's function can't directly access the FSO's
;;   other fields, contrary to the case with OClosures where those are directly
;;   available as local variables.
;; - Function objects in Javascript.
;; - Function objects in Python.
;; - Callable/Applicable classes in OO languages, i.e. classes with
;;   a single method called `apply' or `call'.  The most obvious
;;   difference with OClosures (beside the fact that Callable can be
;;   extended with additional methods) is that all instances of
;;   a given Callable class have to use the same method, whereas every
;;   OClosure object comes with its own code, so two OClosure objects of the
;;   same type can have different code.  Of course, you can get the
;;   same result by turning every `oclosure-lambda' into its own class
;;   declaration creating an ad-hoc subclass of the specified type.
;;   In this sense, OClosures are just a generalization of `lambda' which brings
;;   some of the extra feature of Callable objects.
;; - Apply hooks and "entities" in MIT Scheme
;;   https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Application-Hooks.html
;;   Apply hooks are basically the same as Common-Lisp's FSOs, and "entities"
;;   are a variant of it where the inner function gets the FSO itself as
;;   additional argument (a kind of "self" arg), thus making it easier
;;   for the code to get data from the object's extra info, tho still
;;   not as easy as with OClosures.
;; - "entities" in Lisp Machine Lisp (LML)
;;   https://hanshuebner.github.io/lmman/fd-clo.xml
;;   These are arguably identical to OClosures, modulo the fact that LML doesn't
;;   have lexically-scoped closures and uses a form of closures based on
;;   capturing (and reinstating) dynamically scoped bindings instead.

;; Naming: OClosures were originally named FunCallableRecords (FCR), but
;; that name suggested these were fundamentally records that happened
;; to be called, whereas OClosures are really just closures that happen
;; to enjoy some characteristics of records.
;; The "O" comes from "Open" because OClosures aren't completely opaque
;; (for that same reason, an alternative name suggested at the time was
;; "disclosures").
;; The "O" can also be understood to mean "Object" since you have notions
;; of inheritance, and the ability to associate methods with particular
;; OClosure types, just as is the case for OO classes.

;;; Code:

;; TODO:
;; - `oclosure-(cl-)defun', `oclosure-(cl-)defsubst', `oclosure-define-inline'?
;; - Use accessor in cl-defstruct.
;; - Add pcase patterns for OClosures.
;; - anonymous OClosure types.
;; - copiers for mixins
;; - class-allocated slots?
;; - code-allocated slots?
;;   The `where' slot of `advice' would like to be code-allocated, and the
;;   interactive-spec of commands is currently code-allocated but would like
;;   to be instance-allocated.  Their scoping rules are a bit odd, so maybe
;;   it's best to avoid them.

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))   ;For `named-let'.

(defun oclosure--index-table (slotdescs)
  (let ((i -1)
        (it (make-hash-table :test #'eq)))
    (dolist (desc slotdescs)
      (let* ((slot (cl--slot-descriptor-name desc)))
        (incf i)
        (when (gethash slot it)
          (error "Duplicate slot name: %S" slot))
        (setf (gethash slot it) i)))
    it))

(cl-defstruct (oclosure--class
               (:constructor nil)
               (:constructor oclosure--class-make
                ( name docstring slots parents allparents
                  &aux (index-table (oclosure--index-table slots))))
               (:include cl--class)
               (:copier nil))
  "Metaclass for OClosure classes."
  ;; The `allparents' slot is used for the predicate that checks if a given
  ;; object is an OClosure of a particular type.
  (allparents nil :read-only t :type (list-of symbol)))

(setf (cl--find-class 'oclosure)
      (oclosure--class-make 'oclosure
                            "The root parent of all OClosure types"
                            nil (list (cl--find-class 'closure))
                            '(oclosure)))
(defun oclosure--p (oclosure)
  (not (not (oclosure-type oclosure))))

(define-symbol-prop 'oclosure 'cl-deftype-satisfies #'oclosure--p)

(defun oclosure--slot-mutable-p (slotdesc)
  (not (alist-get :read-only (cl--slot-descriptor-props slotdesc))))

(defun oclosure--defstruct-make-copiers (copiers slotdescs name)
  (require 'cl-macs)            ;`cl--arglist-args' is not autoloaded.
  (let* ((mutables '())
         (slots (mapcar
                 (lambda (desc)
	           (let ((name (cl--slot-descriptor-name desc)))
	             (when (oclosure--slot-mutable-p desc)
	               (push name mutables))
	             name))
	         slotdescs)))
    (mapcar
     (lambda (copier)
       (pcase-let*
           ((cname (pop copier))
            (args (or (pop copier) `(&key ,@slots)))
            (inline (and (eq :inline (car copier)) (pop copier)))
            (doc (or (pop copier)
                     (format "Copier for objects of type `%s'." name)))
            (obj (make-symbol "obj"))
            (absent (make-symbol "absent"))
            (anames (cl--arglist-args args))
            (mnames
             (let ((res '())
                   (tmp args))
               (while (and tmp
                           (not (memq (car tmp)
                                      cl--lambda-list-keywords)))
                 (push (pop tmp) res))
               res))
            (index -1)
            (mutlist '())
            (argvals
             (mapcar
	      (lambda (slot)
	        (setq index (1+ index))
	        (let* ((mutable (memq slot mutables))
	               (get `(oclosure--get ,obj ,index ,(not (not mutable)))))
	          (push mutable mutlist)
		  (cond
		   ((not (memq slot anames)) get)
		   ((memq slot mnames) slot)
		   (t
		    `(if (eq ',absent ,slot)
		         ,get
		       ,slot)))))
	      slots)))
	 `(,(if inline 'cl-defsubst 'cl-defun) ,cname
               (&cl-defs (',absent) ,obj ,@args)
            ,doc
            (declare (side-effect-free t))
            (oclosure--copy ,obj ',(if (remq nil mutlist) (nreverse mutlist))
                       ,@argvals))))
     copiers)))


(defmacro oclosure-define (name &optional docstring &rest slots)
  "Define a new OClosure type.
NAME should be a symbol which is the name of the new type.
It can also be of the form (NAME . PROPS) in which case PROPS
is a list of additional properties among the following:
  (:predicate PRED): asks to create a predicate function named PRED.
  (:parent TYPE): make TYPE (another OClosure type) be a parent of NAME.
  (:copier COPIER ARGS): asks to create a \"copier\" (i.e. functional update
    function) named COPIER.  It will take an object of type NAME as first
    argument followed by ARGS.  ARGS lists the names of the slots that will
    be updated with the value of the corresponding argument.
SLOTS is a list of slot descriptions.  Each slot can be a single symbol
which is the name of the slot, or it can be of the form (SLOT-NAME . SPROPS)
where SLOT-NAME is then the name of the slot and SPROPS is a property
list of slot properties.  The currently known properties are the following:
  `:mutable': A non-nil value mean the slot can be mutated.
  `:type': Specifies the type of the values expected to appear in the slot."
  (declare (doc-string 2) (indent 1))
  (unless (or (stringp docstring) (null docstring))
    (push docstring slots)
    (setq docstring nil))
  (let* ((options (when (consp name)
                    (prog1 (copy-sequence (cdr name))
                      (setq name (car name)))))
         (get-opt (lambda (opt &optional all)
                    (let ((val (assq opt options))
                          tmp)
                      (when val (setq options (delq val options)))
                      (if (not all)
                          (cdr val)
                        (when val
                          (setq val (list (cdr val)))
                          (while (setq tmp (assq opt options))
                            (push (cdr tmp) val)
                            (setq options (delq tmp options)))
                          (nreverse val))))))
         (predicate (car (funcall get-opt :predicate)))
         (parent-names (or (funcall get-opt :parent)
                           (funcall get-opt :include)))
         (copiers (funcall get-opt :copier 'all)))
    `(progn
       ,(when options (macroexp-warn-and-return name
                       (format "Ignored options: %S" options)
                       nil))
       (eval-and-compile
         (oclosure--define ',name ,docstring ',parent-names ',slots
                           ,@(when predicate `(:predicate ',predicate))))
       (oclosure--define-functions ,name ,copiers))))

(defun oclosure--build-class (name docstring parent-names slots)
  (cl-assert (null (cdr parent-names)))
  (let* ((parent-class (let ((name (or (car parent-names) 'oclosure)))
                         (or (cl--find-class name)
                             (error "Unknown class: %S" name))))
         (slotdescs
          (append
           (oclosure--class-slots parent-class)
           (mapcar (lambda (field)
                     (if (not (consp field))
                         (cl--make-slot-descriptor field nil nil
                                                   '((:read-only . t)))
                       (let ((name (pop field))
                             (type nil)
                             (read-only t)
                             (props '()))
                         (while field
                           (pcase (pop field)
                             (:mutable (setq read-only (not (car field))))
                             (:type (setq type (car field)))
                             (p (message "Unknown property: %S" p)
                                (push (cons p (car field)) props)))
                           (setq field (cdr field)))
                         (cl--make-slot-descriptor name nil type
                                                   `((:read-only . ,read-only)
                                                     ,@props)))))
                   slots))))
    (oclosure--class-make name docstring slotdescs
                          (if (cdr parent-names)
                              (oclosure--class-parents parent-class)
                            (list parent-class))
                          (cons name (oclosure--class-allparents
                                      parent-class)))))

(defmacro oclosure--define-functions (name copiers)
  (let* ((class (cl--find-class name))
         (slotdescs (oclosure--class-slots class)))
    `(progn
     ,@(let ((i -1))
           (mapcar (lambda (desc)
                     (let* ((slot (cl--slot-descriptor-name desc))
                            (mutable (oclosure--slot-mutable-p desc))
                            ;; Always use a double hyphen: if users wants to
                            ;; make it public, they can do so with an alias.
                            (aname (intern (format "%S--%S" name slot))))
                       (incf i)
                       (if (not mutable)
                           `(defalias ',aname
                              ;; We use `oclosure--copy' instead of
                              ;; `oclosure--accessor-copy' here to circumvent
                              ;; bootstrapping problems.
                              (oclosure--copy
                               oclosure--accessor-prototype
                               nil ',name ',slot ,i))
                         (require 'gv)  ;For `gv-setter'.
                         `(progn
                            (defalias ',aname
                              (oclosure--accessor-copy
                               oclosure--mut-getter-prototype
                               ',name ',slot ,i))
                            (defalias ',(gv-setter aname)
                              (oclosure--accessor-copy
                               oclosure--mut-setter-prototype
                               ',name ',slot ,i))))))
                   slotdescs))
       ,@(oclosure--defstruct-make-copiers
          copiers slotdescs name))))

;;;###autoload
(defun oclosure--define (name docstring parent-names slots
                              &rest props)
  (let* ((class (oclosure--build-class name docstring parent-names slots))
         (pred (lambda (oclosure)
                 (let ((type (oclosure-type oclosure)))
                   (when type
                     (memq name (oclosure--class-allparents
                                 (cl--find-class type)))))))
         (predname (or (plist-get props :predicate)
                       (intern (format "%s--internal-p" name)))))
    (setf (cl--find-class name) class)
    (dolist (slot (oclosure--class-slots class))
      (put (cl--slot-descriptor-name slot) 'slot-name t))
    (defalias predname pred)
    (put name 'cl-deftype-satisfies predname)))

(defmacro oclosure--lambda (type bindings mutables args &rest body)
  "Low level construction of an OClosure object.
TYPE should be a form returning an OClosure type (a symbol).
BINDINGS should list all the slots expected by this type, in the proper order.
MUTABLE is a list of symbols indicating which of the BINDINGS
should be mutable.
No checking is performed."
  (declare (indent 3) (debug (sexp (&rest (sexp form)) sexp def-body)))
  (cl-assert lexical-binding)          ;Can't work in dynbind dialect.
  ;; FIXME: Fundamentally `oclosure-lambda' should be a special form.
  ;; We define it here as a macro which expands to something that
  ;; looks like "normal code" in order to avoid backward compatibility
  ;; issues with third party macros that do "code walks" and would
  ;; likely mishandle such a new special form (e.g. `generator.el').
  ;; But don't be fooled: this macro is tightly bound to `cconv.el'.
  (pcase-let*
      ((`(,prebody . ,body) (macroexp-parse-body body))
       (rovars (mapcar #'car bindings)))
    (dolist (mutable mutables)
      (setq rovars (delq mutable rovars)))
    `(let ,(mapcar (lambda (bind)
                     (if (cdr bind) bind
                       ;; Bind to something that doesn't look
                       ;; like a value to avoid the "Variable
                       ;; ‘foo’ left uninitialized" warning.
                       `(,(car bind) (progn nil))))
                   (reverse bindings))
       ;; FIXME: Make sure the slotbinds whose value is duplicable aren't
       ;; just value/variable-propagated by the optimizer (tho I think our
       ;; optimizer is too naive to be a problem currently).
       (oclosure--fix-type
        ;; This `oclosure--fix-type' + `ignore' call is used by the compiler (in
        ;; `cconv.el') to detect and signal an error in case of
        ;; store-conversion (i.e. if a variable/slot is mutated).
        (ignore ,@rovars)
        (lambda ,args
          (:documentation ,type)
          ,@prebody
          ;; Add dummy code which accesses the field's vars to make sure
          ;; they're captured in the closure.
          (if t nil ,@rovars ,@(mapcar (lambda (m) `(setq ,m ,m)) mutables))
          ,@body)))))

(defmacro oclosure-lambda (type-and-slots args &rest body)
  "Define anonymous OClosure function.
TYPE-AND-SLOTS should be of the form (TYPE . SLOTS)
where TYPE is an OClosure type name (defined by `oclosure-define')
and SLOTS is a let-style list of bindings for the various slots of TYPE.
ARGS and BODY are the same as for `lambda'."
  (declare (indent 2) (debug ((sexp &rest (sexp form)) sexp def-body)))
  ;; FIXME: Should `oclosure-define' distinguish "optional" from
  ;; "mandatory" slots, and/or provide default values for slots missing
  ;; from `fields'?
  (pcase-let*
      ((`(,type . ,fields) type-and-slots)
       (class (or (cl--find-class type)
                  (error "Unknown class: %S" type)))
       (slots (oclosure--class-slots class))
       (mutables '())
       (slotbinds (mapcar (lambda (slot)
                            (let ((name (cl--slot-descriptor-name slot)))
                              (when (oclosure--slot-mutable-p slot)
                                (push name mutables))
                              (list name)))
                          slots))
       (tempbinds (mapcar
                   (lambda (field)
                     (let* ((name (car field))
                            (bind (assq name slotbinds)))
                       (cond
                        ;; FIXME: Should we also warn about missing slots?
                        ((not bind)
                         (error "Unknown slot: %S" name))
                        ((cdr bind)
                         (error "Duplicate slot: %S" name))
                        (t
                         (let ((temp (gensym "temp")))
                           (setcdr bind (list temp))
                           (cons temp (cdr field)))))))
                   fields)))
    ;; FIXME: Optimize temps away when they're provided in the right order?
    `(let ,tempbinds
       (oclosure--lambda ',type ,slotbinds ,mutables ,args ,@body))))

(defun oclosure--fix-type (_ignore oclosure)
  "Helper function to implement `oclosure-lambda' via a macro.
This is used as a marker which cconv uses to check that
immutable fields are indeed not mutated."
  (cl-assert (closurep oclosure))
  ;; This should happen only for interpreted closures since `cconv.el'
  ;; should have optimized away the call to this function.
  oclosure)

(defun oclosure--copy (oclosure mutlist &rest args)
  (cl-assert (closurep oclosure))
  (if (byte-code-function-p oclosure)
      (apply #'make-closure oclosure
             (if (null mutlist)
                 args
               (mapcar (lambda (arg) (if (pop mutlist) (list arg) arg)) args)))
    (cl-assert (consp (aref oclosure 1)))
    (cl-assert (null (aref oclosure 3)))
    (cl-assert (symbolp (aref oclosure 4)))
    (let ((env (aref oclosure 2)))
      (make-interpreted-closure
       (aref oclosure 0)
       (aref oclosure 1)
       (named-let loop ((env env) (args args))
         (if (null args) env
           (cons (cons (caar env) (car args))
                 (loop (cdr env) (cdr args)))))
       (aref oclosure 4)
       (if (> (length oclosure) 5)
           `(interactive ,(aref oclosure 5)))))))

(defun oclosure--get (oclosure index mutable)
  (cl-assert (closurep oclosure))
  (let* ((csts (aref oclosure 2)))
    (if (vectorp csts)
        (let ((v (aref csts index)))
          (if mutable (car v) v))
      (cdr (nth index csts)))))

(defun oclosure--set (v oclosure index)
  (cl-assert (closurep oclosure))
  (let ((csts (aref oclosure 2)))
    (if (vectorp csts)
        (let ((cell (aref csts index)))
          (setcar cell v))
      (setcdr (nth index csts) v))))

(defun oclosure-type (oclosure)
  "Return the type of OCLOSURE, or nil if the arg is not an OClosure."
  (and (closurep oclosure)
       (> (length oclosure) 4)
       (let ((type (aref oclosure 4)))
         (if (symbolp type) type))))

(defconst oclosure--accessor-prototype
  ;; Use `oclosure--lambda' to circumvent a bootstrapping problem:
  ;; `oclosure-accessor' is not yet defined at this point but
  ;; `oclosure--accessor-prototype' is needed when defining `oclosure-accessor'.
  (oclosure--lambda 'oclosure-accessor ((type) (slot) (index)) nil
    (oclosure) (oclosure--get oclosure index nil)))

(oclosure-define accessor
  "OClosure function to access a specific slot of an object."
  type slot)

(defun oclosure--accessor-cl-print (object stream)
  (princ "#f(accessor " stream)
  (prin1 (accessor--type object) stream)
  (princ "." stream)
  (prin1 (accessor--slot object) stream)
  (princ ")" stream))

(defun oclosure--accessor-docstring (f)
  ;; This would like to be a (cl-defmethod function-documentation ...)
  ;; but for circularity reason the defmethod is in `simple.el'.
  (format "Access slot \"%S\" of OBJ of type `%S'.\n\n(fn OBJ)"
          (accessor--slot f) (accessor--type f)))

(oclosure-define (oclosure-accessor
                  (:parent accessor)
                  (:copier oclosure--accessor-copy (type slot index)))
  "OClosure function to access a specific slot of an OClosure function."
  index)

(defun oclosure--slot-index (oclosure slotname)
  (gethash slotname
           (oclosure--class-index-table
            (cl--find-class (oclosure-type oclosure)))))

(defun oclosure--slot-value (oclosure slotname)
  (let ((class (cl--find-class (oclosure-type oclosure)))
        (index (oclosure--slot-index oclosure slotname)))
    (oclosure--get oclosure index
                   (oclosure--slot-mutable-p
                    (nth index (oclosure--class-slots class))))))

(defun oclosure--set-slot-value (oclosure slotname value)
  (let ((class (cl--find-class (oclosure-type oclosure)))
        (index (oclosure--slot-index oclosure slotname)))
    (unless (oclosure--slot-mutable-p
             (nth index (oclosure--class-slots class)))
      (signal 'setting-constant (list oclosure slotname)))
    (oclosure--set value oclosure index)))

(defconst oclosure--mut-getter-prototype
  (oclosure-lambda (oclosure-accessor (type) (slot) (index)) (oclosure)
    (oclosure--get oclosure index t)))
(defconst oclosure--mut-setter-prototype
  ;; FIXME: The generated docstring is wrong.
  (oclosure-lambda (oclosure-accessor (type) (slot) (index)) (val oclosure)
    (oclosure--set val oclosure index)))

;; Ideally, this should be in `files.el', but that file is loaded
;; before `oclosure.el'.
(oclosure-define (save-some-buffers-function
                  (:predicate save-some-buffers-function--p)))

;; This OClosure type is used internally by `cconv.el' to handle
;; the case where we need to build a closure whose `interactive' spec
;; captures variables from the context.
;; It arguably belongs with `cconv.el' but is needed at runtime,
;; so we placed it here.
(oclosure-define (cconv--interactive-helper) fun if)
(defun cconv--interactive-helper (fun if)
  "Add interactive \"form\" IF to FUN.
Returns a new command that otherwise behaves like FUN.
IF can be a Lisp form to be interpreted or a function of no arguments."
  (oclosure-lambda (cconv--interactive-helper (fun fun) (if if))
      (&rest args)
    (apply (if (called-interactively-p 'any)
               #'funcall-interactively #'funcall)
           fun args)))

(provide 'oclosure)
;;; oclosure.el ends here
