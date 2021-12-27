;;; fcr.el --- FunCallableRecords       -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2021  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A FunCallableRecord is an object that combines the properties of records
;; with those of a function.  More specifically it is a function extended
;; with a notion of type (e.g. for defmethod dispatch) as well as the
;; ability to have some fields that are accessible from the outside.

;; Here are some cases of "callable objects" where FCRs are used:
;; - nadvice.el
;; - kmacros (for cl-print and for `kmacro-extract-lambda')
;; - cl-generic: turn `cl--generic-isnot-nnm-p' into a mere type test
;;   (by putting the no-next-methods into their own class).
;; - FCR accessor functions, where the type-dispatch is used to
;;   dynamically compute the docstring, and also to pretty them.
;; Here are other cases of "callable objects" where FCRs could be used:
;; - iterators (generator.el), thunks (thunk.el), streams (stream.el).
;; - PEG rules: they're currently just functions, but they should carry
;;   their original (macro-expanded) definition (and should be printed
;;   differently from functions)!
;; - documented functions: this could be a subtype of normal functions, which
;;   simply has an additional `docstring' slot.
;; - commands: this could be a subtype of documented functions, which simply
;;   has an additional `interactive-form' slot.
;; - auto-generate docstrings for cl-defstruct slot accessors instead of
;;   storing them in the accessor itself?

;; Related constructs:
;; - `funcallable-standard-object' (FSO) in Common-Lisp.  These are different
;;   from FCRs in that they involve an additional indirection to get
;;   to the actual code, and that they offer the possibility of
;;   changing (via mutation) the code associated with
;;   an FSO.  Also the FSO's function can't directly access the FSO's
;;   other fields, contrary to the case with FCRs where those are directly
;;   available as local variables.
;; - Function objects in Javascript.
;; - Function objects in Python.
;; - Callable/Applicable classes in OO languages, i.e. classes with
;;   a single method called `apply' or `call'.  The most obvious
;;   difference with FCRs (beside the fact that Callable can be
;;   extended with additional methods) is that all instances of
;;   a given Callable class have to use the same method, whereas every
;;   FCR object comes with its own code, so two FCR objects of the
;;   same type can have different code.  Of course, you can get the
;;   same result by turning every `fcr-lambda' into its own class
;;   declaration creating an ad-hoc subclass of the specified type.
;;   In this sense, FCRs are just a generalization of `lambda' which brings
;;   some of the extra feature of Callable objects.
;; - Apply hooks and "entities" in MIT Scheme
;;   https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Application-Hooks.html
;;   Apply hooks are basically the same as Common-Lisp's FSOs, and "entities"
;;   are a variant of it where the inner function gets the FSO itself as
;;   additional argument (a kind of "self" arg), thus making it easier
;;   for the code to get data from the object's extra info, tho still
;;   not as easy as with FCRs.

;;; Code:

;; Slots are currently immutable, tho they can be updated functionally
;; via the "copiers": we could relax this restriction by either allowing
;; the function itself to mutate the captured variable/slot or by providing
;; `setf' accessors to the slots (or both), but this comes with some problems:
;; - mutation from within the function currently would cause cconv
;;   to perform store-conversion on the variable, so we'd either have
;;   to prevent cconv from doing it (which might require a new bytecode op
;;   to update the in-closure variable), or we'd have to keep track of which
;;   slots have been store-converted so `fcr--get' can access their value
;;   correctly.
;; - If the mutated variable/slot is captured by another (nested) closure
;;   store-conversion is indispensable, so if we want to avoid store-conversion
;;   we'd have to disallow such capture.

;; FIXME:
;; - Snarf-documentation leaves bogus fixnums in place in`create-file-buffer'.
;; - `fcr-cl-defun', `fcr-cl-defsubst', `fcr-defsubst', `fcr-define-inline'?
;; - Use accessor in cl-defstruct

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))   ;For `named-let'.

(cl-defstruct (fcr--class
               (:constructor nil)
               (:constructor fcr--class-make ( name docstring slots parents
                                               allparents))
               (:include cl--class)
               (:copier nil))
  "Metaclass for FunCallableRecord classes."
  (allparents nil :read-only t :type (list-of symbol)))

(setf (cl--find-class 'fcr-object)
      (fcr--class-make 'fcr-object "The root parent of all FCR classes"
                       nil nil '(fcr-object)))
(defun fcr--object-p (fcr)
  (let ((type (fcr-type fcr)))
    (when type
      (memq 'fcr-object (fcr--class-allparents (cl--find-class type))))))
(cl-deftype fcr-object () '(satisfies fcr--object-p))

(defun fcr--defstruct-make-copiers (copiers slotdescs name)
  (require 'cl-macs)            ;`cl--arglist-args' is not autoloaded.
  (let* ((mutables '())
         (slots (mapcar
                 (lambda (desc)
	           (let ((name (cl--slot-descriptor-name desc)))
	             (unless (alist-get :read-only
	                                (cl--slot-descriptor-props desc))
	               (push name mutables))
	             name))
	         slotdescs)))
    (mapcar
     (lambda (copier)
       (pcase-let*
           ((cname (pop copier))
            (args (or (pop copier) `(&key ,@slots)))
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
	               (get `(fcr--get ,obj ,index ,(not (not mutable)))))
	          (push mutable mutlist)
		  (cond
		   ((not (memq slot anames)) get)
		   ((memq slot mnames) slot)
		   (t
		    `(if (eq ',absent ,slot)
		         ,get
		       ,slot)))))
	      slots)))
	 `(cl-defun ,cname (&cl-defs (',absent) ,obj ,@args)
            ,doc
            (declare (side-effect-free t))
            (fcr--copy ,obj ',(if (remq nil mutlist) (nreverse mutlist))
                       ,@argvals))))
     copiers)))

(defmacro fcr-defstruct (name &optional docstring &rest slots)
  (declare (doc-string 2) (indent 1))
  (unless (stringp docstring)
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

         (parent-names (or (or (funcall get-opt :parent)
                               (funcall get-opt :include))
                           '(fcr-object)))
         (copiers (funcall get-opt :copier 'all))

         (parent-slots '())
         (parents
          (mapcar
           (lambda (name)
             (let* ((class (or (cl--find-class name)
                               (error "Unknown parent: %S" name))))
               (setq parent-slots
                     (named-let merge
                         ((slots-a parent-slots)
                          (slots-b (cl--class-slots class)))
                       (cond
                        ((null slots-a) slots-b)
                        ((null slots-b) slots-a)
                        (t
                         (let ((sa (car slots-a))
                               (sb (car slots-b)))
                           (unless (equal sa sb)
                             (error "Slot %s of %s conflicts with slot %s of previous parent"
                                    (cl--slot-descriptor-name sb)
                                    name
                                    (cl--slot-descriptor-name sa)))
                           (cons sa (merge (cdr slots-a) (cdr slots-b))))))))
               class))
           parent-names))
         (slotdescs
          (append
           parent-slots
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
                   slots)))
         (allparents (apply #'append (mapcar #'cl--class-allparents
                                             parents)))
         (class (fcr--class-make name docstring slotdescs parents
                                 (delete-dups
                                  (cons name allparents))))
         (it (make-hash-table :test #'eq)))
    (setf (cl--class-index-table class) it)
    `(progn
       ,(when options (macroexp-warn-and-return
                       (format "Ignored options: %S" options)
                       nil))
       (eval-and-compile
         (fcr--define ',class
                      (lambda (fcr)
                        (let ((type (fcr-type fcr)))
                          (when type
                            (memq ',name (fcr--class-allparents
                                          (cl--find-class type))))))))
       ,@(let ((i -1))
           (mapcar (lambda (desc)
                     (let* ((slot (cl--slot-descriptor-name desc))
                            (mutable
                             (not (alist-get :read-only
                                             (cl--slot-descriptor-props desc))))
                            ;; Always use a double hyphen: if users wants to
                            ;; make it public, they can do so with an alias.
                            (name (intern (format "%S--%S" name slot))))
                       (cl-incf i)
                       (when (gethash slot it)
                         (error "Duplicate slot name: %S" slot))
                       (setf (gethash slot it) i)
                       (if (not mutable)
                           `(defalias ',name
                              ;; We use `fcr--copy' instead of
                              ;; `fcr--accessor-copy' here to circumvent
                              ;; bootstrapping problems.
                              (fcr--copy fcr--accessor-prototype nil
                                         ',name ',slot ,i))
                         `(progn
                            (defalias ',name
                              (fcr--accessor-copy
                               fcr--mut-getter-prototype
                               ',name ',slot ,i))
                            (defalias ',(gv-setter name)
                              (fcr--accessor-copy
                               fcr--mut-setter-prototype
                               ',name ',slot ,i))))))
                   slotdescs))
       ,@(fcr--defstruct-make-copiers
          copiers slotdescs name))))

(defun fcr--define (class pred)
  (let* ((name (cl--class-name class))
         (predname (intern (format "fcr--%s-p" name))))
    (setf (cl--find-class name) class)
    (defalias predname pred)
    (put name 'cl-deftype-satisfies predname)))

(defmacro fcr--lambda (type bindings mutables args &rest body)
  "Low level construction of an FCR object.
TYPE is expected to be a symbol that is (or will be) defined as an FCR type.
BINDINGS should list all the slots expected by this type, in the proper order.
MUTABLE is a list of symbols indicating which of the BINDINGS
should be mutable.
No checking is performed,"
  (declare (indent 3) (debug (sexp (&rest (sexp form)) sexp def-body)))
  ;; FIXME: Fundamentally `fcr-lambda' should be a special form.
  ;; We define it here as a macro which expands to something that
  ;; looks like "normal code" in order to avoid backward compatibility
  ;; issues with third party macros that do "code walks" and would
  ;; likely mishandle such a new special form (e.g. `generator.el').
  ;; But don't be fooled: this macro is tightly bound to `cconv.el'.
  (pcase-let*
      ;; FIXME: Since we use the docstring internally to store the
      ;; type we can't handle actual docstrings.  We could fix this by adding
      ;; a docstring slot to FCRs.
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
       (fcr--fix-type
        ;; This `fcr--fix-type' + `ignore' call is used by the compiler (in
        ;; `cconv.el') to detect and signal an error in case of
        ;; store-conversion (i.e. if a variable/slot is mutated).
        (ignore ,@rovars)
        (lambda ,args
          (:documentation ',type)
          ,@prebody
          ;; Add dummy code which accesses the field's vars to make sure
          ;; they're captured in the closure.
          (if t nil ,@rovars ,@(mapcar (lambda (m) `(setq ,m ,m)) mutables))
          ,@body)))))

(defmacro fcr-lambda (type-and-slots args &rest body)
  "Define anonymous FCR function.
TYPE-AND-SLOTS should be of the form (TYPE . SLOTS)
where TYPE is an FCR type name and
SLOTS is a let-style list of bindings for the various slots of TYPE.
ARGS and BODY are the same as for `lambda'."
  (declare (indent 2) (debug ((sexp &rest (sexp form)) sexp def-body)))
  ;; FIXME: Should `fcr-defstruct' distinguish "optional" from
  ;; "mandatory" slots, and/or provide default values for slots missing
  ;; from `fields'?
  (pcase-let*
      ((`(,type . ,fields) type-and-slots)
       (class (cl--find-class type))
       (slots (fcr--class-slots class))
       (mutables '())
       (slotbinds (mapcar (lambda (slot)
                            (let ((name (cl--slot-descriptor-name slot))
                                  (props (cl--slot-descriptor-props slot)))
                              (unless (alist-get :read-only props)
                                (push name mutables))
                              (list name)))
                          slots))
       (tempbinds (mapcar
                   (lambda (field)
                     (let* ((name (car field))
                            (bind (assq name slotbinds)))
                       (cond
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
       (fcr--lambda ,type ,slotbinds ,mutables ,args ,@body))))

(defun fcr--fix-type (_ignore fcr)
  (if (byte-code-function-p fcr)
      ;; Actually, this should never happen since the `cconv.el' should have
      ;; optimized away the call to this function.
      fcr
    ;; For byte-coded functions, we store the type as a symbol in the docstring
    ;; slot.  For interpreted functions, there's no specific docstring slot
    ;; so `Ffunction' turns the symbol into a string.
    ;; We thus have convert it back into a symbol (via `intern') and then
    ;; stuff it into the environment part of the closure with a special
    ;; marker so we can distinguish this entry from actual variables.
    (cl-assert (eq 'closure (car-safe fcr)))
    (let ((typename (nth 3 fcr))) ;; The "docstring".
      (cl-assert (stringp typename))
      (push (cons :type (intern typename))
            (cadr fcr))
      fcr)))

(defun fcr--copy (fcr mutlist &rest args)
  (if (byte-code-function-p fcr)
      (apply #'make-closure fcr
             (if (null mutlist)
                 args
               (mapcar (lambda (arg) (if (pop mutlist) (list arg) arg)) args)))
    (cl-assert (eq 'closure (car-safe fcr)))
    (cl-assert (eq :type (caar (cadr fcr))))
    (let ((env (cadr fcr)))
      `(closure
           (,(car env)
            ,@(named-let loop ((env (cdr env)) (args args))
                (when args
                  (cons (cons (caar env) (car args))
                        (loop (cdr env) (cdr args)))))
            ,@(nthcdr (1+ (length args)) env))
           ,@(nthcdr 2 fcr)))))

(defun fcr--get (fcr index mutable)
  (if (byte-code-function-p fcr)
      (let* ((csts (aref fcr 2))
             (v (aref csts index)))
        (if mutable (car v) v))
    (cl-assert (eq 'closure (car-safe fcr)))
    (cl-assert (eq :type (caar (cadr fcr))))
    (cdr (nth (1+ index) (cadr fcr)))))

(defun fcr--set (v fcr index)
  (if (byte-code-function-p fcr)
      (let* ((csts (aref fcr 2))
             (cell (aref csts index)))
        (setcar cell v))
    (cl-assert (eq 'closure (car-safe fcr)))
    (cl-assert (eq :type (caar (cadr fcr))))
    (setcdr (nth (1+ index) (cadr fcr)) v)))

(defun fcr-type (fcr)
  "Return the type of FCR, or nil if the arg is not a FunCallableRecord."
  (if (byte-code-function-p fcr)
      (let ((type (and (> (length fcr) 4) (aref fcr 4))))
        (if (symbolp type) type))
    (and (eq 'closure (car-safe fcr))
         (let* ((env (car-safe (cdr fcr)))
                (first-var (car-safe env)))
           (and (eq :type (car-safe first-var))
                (cdr first-var))))))

(defconst fcr--accessor-prototype
  ;; Use `fcr--lambda' to circumvent a bootstrapping problem:
  ;; `fcr-accessor' is not yet defined at this point but
  ;; `fcr--accessor-prototype' is needed when defining `fcr-accessor'.
  (fcr--lambda fcr-accessor ((type) (slot) (index)) nil
    (fcr) (fcr--get fcr index nil)))

(fcr-defstruct accessor
  "FCR function to access a specific slot of an object."
  type slot)

(defun fcr--accessor-cl-print (object stream)
  (princ "#f(accessor " stream)
  (prin1 (accessor--type object) stream)
  (princ "." stream)
  (prin1 (accessor--slot object) stream)
  (princ ")" stream))

(defun fcr--accessor-docstring (f)
  (format "Access slot \"%S\" of OBJ of type `%S'.

\(fn OBJ)"
          (accessor--slot f) (accessor--type f)))

(fcr-defstruct (fcr-accessor
                (:parent accessor)
                (:copier fcr--accessor-copy (type slot index)))
  "FCR function to access a specific slot of an FCR function."
  index)

(defconst fcr--mut-getter-prototype
  (fcr-lambda (fcr-accessor (type) (slot) (index)) (fcr)
    (fcr--get fcr index t)))
(defconst fcr--mut-setter-prototype
  ;; FIXME: The generated docstring is wrong.
  (fcr-lambda (fcr-accessor (type) (slot) (index)) (val fcr)
    (fcr--set val fcr index)))

(provide 'fcr)
;;; fcr.el ends here
