;;; oclosure.el --- Open Closures       -*- lexical-binding: t; -*-

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

;; A OClosure is an object that combines the properties of records
;; with those of a function.  More specifically it is a function extended
;; with a notion of type (e.g. for defmethod dispatch) as well as the
;; ability to have some fields that are accessible from the outside.

;; Here are some cases of "callable objects" where OClosures are used:
;; - nadvice.el
;; - kmacros (for cl-print and for `kmacro-extract-lambda')
;; - cl-generic: turn `cl--generic-isnot-nnm-p' into a mere type test
;;   (by putting the no-next-methods into their own class).
;; - OClosure accessor functions, where the type-dispatch is used to
;;   dynamically compute the docstring, and also to pretty them.
;; Here are other cases of "callable objects" where OClosures could be used:
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
;; - SRFI-17's `setter'.
;; - coercion wrappers, as in "Threesomes, with and without blame"
;;   https://dl.acm.org/doi/10.1145/1706299.1706342, or
;;   "On the Runtime Complexity of Type-Directed Unboxing"
;;   http://sv.c.titech.ac.jp/minamide/papers.html
;; - An efficient `negate' operation such that
;;   (negate f) generally returns (lambda (x) (not (f x)))
;;   but it can optimize (negate (negate f)) to f and (negate #'<) to
;;   #'>=.

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

;; Naming: to replace "OClosure" we could go with
;; - open closures
;; - disclosures
;; - opening
;; - object functions/closures
;; - structured functions/closures (strunctions, strufs)
;; - slotfuns (slotted functions)

;;; Code:

;; Slots are currently immutable, tho they can be updated functionally
;; via the "copiers": we could relax this restriction by either allowing
;; the function itself to mutate the captured variable/slot or by providing
;; `setf' accessors to the slots (or both), but this comes with some problems:
;; - mutation from within the function currently would cause cconv
;;   to perform store-conversion on the variable, so we'd either have
;;   to prevent cconv from doing it (which might require a new bytecode op
;;   to update the in-closure variable), or we'd have to keep track of which
;;   slots have been store-converted so `oclosure--get' can access their value
;;   correctly.
;; - If the mutated variable/slot is captured by another (nested) closure
;;   store-conversion is indispensable, so if we want to avoid store-conversion
;;   we'd have to disallow such capture.

;; FIXME:
;; - Snarf-documentation leaves bogus fixnums in place in`create-file-buffer'.
;; - `oclosure-cl-defun', `oclosure-cl-defsubst', `oclosure-defsubst', `oclosure-define-inline'?
;; - Use accessor in cl-defstruct

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))   ;For `named-let'.

(cl-defstruct (oclosure--class
               (:constructor nil)
               (:constructor oclosure--class-make ( name docstring slots parents
                                               allparents))
               (:include cl--class)
               (:copier nil))
  "Metaclass for OClosure classes."
  (allparents nil :read-only t :type (list-of symbol)))

(setf (cl--find-class 'oclosure-object)
      (oclosure--class-make 'oclosure-object "The root parent of all OClosure classes"
                       nil nil '(oclosure-object)))
(defun oclosure--object-p (oclosure)
  (let ((type (oclosure-type oclosure)))
    (when type
      (memq 'oclosure-object (oclosure--class-allparents (cl--find-class type))))))
(cl-deftype oclosure-object () '(satisfies oclosure--object-p))

(defun oclosure--defstruct-make-copiers (copiers slotdescs name)
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
	 `(cl-defun ,cname (&cl-defs (',absent) ,obj ,@args)
            ,doc
            (declare (side-effect-free t))
            (oclosure--copy ,obj ',(if (remq nil mutlist) (nreverse mutlist))
                       ,@argvals))))
     copiers)))

(defmacro oclosure-define (name &optional docstring &rest slots)
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
                           '(oclosure-object)))
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
         (class (oclosure--class-make name docstring slotdescs parents
                                 (delete-dups
                                  (cons name allparents))))
         (it (make-hash-table :test #'eq)))
    (setf (cl--class-index-table class) it)
    `(progn
       ,(when options (macroexp-warn-and-return
                       (format "Ignored options: %S" options)
                       nil))
       (eval-and-compile
         (oclosure--define ',class
                      (lambda (oclosure)
                        (let ((type (oclosure-type oclosure)))
                          (when type
                            (memq ',name (oclosure--class-allparents
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
                              ;; We use `oclosure--copy' instead of
                              ;; `oclosure--accessor-copy' here to circumvent
                              ;; bootstrapping problems.
                              (oclosure--copy oclosure--accessor-prototype nil
                                         ',name ',slot ,i))
                         `(progn
                            (defalias ',name
                              (oclosure--accessor-copy
                               oclosure--mut-getter-prototype
                               ',name ',slot ,i))
                            (defalias ',(gv-setter name)
                              (oclosure--accessor-copy
                               oclosure--mut-setter-prototype
                               ',name ',slot ,i))))))
                   slotdescs))
       ,@(oclosure--defstruct-make-copiers
          copiers slotdescs name))))

(defun oclosure--define (class pred)
  (let* ((name (cl--class-name class))
         (predname (intern (format "oclosure--%s-p" name))))
    (setf (cl--find-class name) class)
    (defalias predname pred)
    (put name 'cl-deftype-satisfies predname)))

(defmacro oclosure--lambda (type bindings mutables args &rest body)
  "Low level construction of an OClosure object.
TYPE is expected to be a symbol that is (or will be) defined as an OClosure type.
BINDINGS should list all the slots expected by this type, in the proper order.
MUTABLE is a list of symbols indicating which of the BINDINGS
should be mutable.
No checking is performed,"
  (declare (indent 3) (debug (sexp (&rest (sexp form)) sexp def-body)))
  ;; FIXME: Fundamentally `oclosure-lambda' should be a special form.
  ;; We define it here as a macro which expands to something that
  ;; looks like "normal code" in order to avoid backward compatibility
  ;; issues with third party macros that do "code walks" and would
  ;; likely mishandle such a new special form (e.g. `generator.el').
  ;; But don't be fooled: this macro is tightly bound to `cconv.el'.
  (pcase-let*
      ;; FIXME: Since we use the docstring internally to store the
      ;; type we can't handle actual docstrings.  We could fix this by adding
      ;; a docstring slot to OClosures.
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
          (:documentation ',type)
          ,@prebody
          ;; Add dummy code which accesses the field's vars to make sure
          ;; they're captured in the closure.
          (if t nil ,@rovars ,@(mapcar (lambda (m) `(setq ,m ,m)) mutables))
          ,@body)))))

(defmacro oclosure-lambda (type-and-slots args &rest body)
  "Define anonymous OClosure function.
TYPE-AND-SLOTS should be of the form (TYPE . SLOTS)
where TYPE is an OClosure type name and
SLOTS is a let-style list of bindings for the various slots of TYPE.
ARGS and BODY are the same as for `lambda'."
  (declare (indent 2) (debug ((sexp &rest (sexp form)) sexp def-body)))
  ;; FIXME: Should `oclosure-define' distinguish "optional" from
  ;; "mandatory" slots, and/or provide default values for slots missing
  ;; from `fields'?
  (pcase-let*
      ((`(,type . ,fields) type-and-slots)
       (class (cl--find-class type))
       (slots (oclosure--class-slots class))
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
       (oclosure--lambda ,type ,slotbinds ,mutables ,args ,@body))))

(defun oclosure--fix-type (_ignore oclosure)
  (if (byte-code-function-p oclosure)
      ;; Actually, this should never happen since the `cconv.el' should have
      ;; optimized away the call to this function.
      oclosure
    ;; For byte-coded functions, we store the type as a symbol in the docstring
    ;; slot.  For interpreted functions, there's no specific docstring slot
    ;; so `Ffunction' turns the symbol into a string.
    ;; We thus have convert it back into a symbol (via `intern') and then
    ;; stuff it into the environment part of the closure with a special
    ;; marker so we can distinguish this entry from actual variables.
    (cl-assert (eq 'closure (car-safe oclosure)))
    (let ((typename (nth 3 oclosure))) ;; The "docstring".
      (cl-assert (stringp typename))
      (push (cons :type (intern typename))
            (cadr oclosure))
      oclosure)))

(defun oclosure--copy (oclosure mutlist &rest args)
  (if (byte-code-function-p oclosure)
      (apply #'make-closure oclosure
             (if (null mutlist)
                 args
               (mapcar (lambda (arg) (if (pop mutlist) (list arg) arg)) args)))
    (cl-assert (eq 'closure (car-safe oclosure)))
    (cl-assert (eq :type (caar (cadr oclosure))))
    (let ((env (cadr oclosure)))
      `(closure
           (,(car env)
            ,@(named-let loop ((env (cdr env)) (args args))
                (when args
                  (cons (cons (caar env) (car args))
                        (loop (cdr env) (cdr args)))))
            ,@(nthcdr (1+ (length args)) env))
           ,@(nthcdr 2 oclosure)))))

(defun oclosure--get (oclosure index mutable)
  (if (byte-code-function-p oclosure)
      (let* ((csts (aref oclosure 2))
             (v (aref csts index)))
        (if mutable (car v) v))
    (cl-assert (eq 'closure (car-safe oclosure)))
    (cl-assert (eq :type (caar (cadr oclosure))))
    (cdr (nth (1+ index) (cadr oclosure)))))

(defun oclosure--set (v oclosure index)
  (if (byte-code-function-p oclosure)
      (let* ((csts (aref oclosure 2))
             (cell (aref csts index)))
        (setcar cell v))
    (cl-assert (eq 'closure (car-safe oclosure)))
    (cl-assert (eq :type (caar (cadr oclosure))))
    (setcdr (nth (1+ index) (cadr oclosure)) v)))

(defun oclosure-type (oclosure)
  "Return the type of OCLOSURE, or nil if the arg is not a OClosure."
  (if (byte-code-function-p oclosure)
      (let ((type (and (> (length oclosure) 4) (aref oclosure 4))))
        (if (symbolp type) type))
    (and (eq 'closure (car-safe oclosure))
         (let* ((env (car-safe (cdr oclosure)))
                (first-var (car-safe env)))
           (and (eq :type (car-safe first-var))
                (cdr first-var))))))

(defconst oclosure--accessor-prototype
  ;; Use `oclosure--lambda' to circumvent a bootstrapping problem:
  ;; `oclosure-accessor' is not yet defined at this point but
  ;; `oclosure--accessor-prototype' is needed when defining `oclosure-accessor'.
  (oclosure--lambda oclosure-accessor ((type) (slot) (index)) nil
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
  (format "Access slot \"%S\" of OBJ of type `%S'.

\(fn OBJ)"
          (accessor--slot f) (accessor--type f)))

(oclosure-define (oclosure-accessor
                (:parent accessor)
                (:copier oclosure--accessor-copy (type slot index)))
  "OClosure function to access a specific slot of an OClosure function."
  index)

(defconst oclosure--mut-getter-prototype
  (oclosure-lambda (oclosure-accessor (type) (slot) (index)) (oclosure)
    (oclosure--get oclosure index t)))
(defconst oclosure--mut-setter-prototype
  ;; FIXME: The generated docstring is wrong.
  (oclosure-lambda (oclosure-accessor (type) (slot) (index)) (val oclosure)
    (oclosure--set val oclosure index)))

(provide 'oclosure)
;;; oclosure.el ends here
