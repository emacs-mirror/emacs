;;; eieio-core.el --- Core implementation for eieio  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1996, 1998-2015 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.4
;; Keywords: OO, lisp

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The "core" part of EIEIO is the implementation for the object
;; system (such as eieio-defclass, or eieio-defmethod) but not the
;; base classes for the object system, which are defined in EIEIO.
;;
;; See the commentary for eieio.el for more about EIEIO itself.

;;; Code:

(require 'cl-lib)
(require 'pcase)

;;;
;; A few functions that are better in the official EIEIO src, but
;; used from the core.
(declare-function slot-unbound "eieio")
(declare-function slot-missing "eieio")
(declare-function child-of-class-p "eieio")
(declare-function same-class-p "eieio")
(declare-function object-of-class-p "eieio")


;;;
;; Variable declarations.
;;
(defvar eieio-hook nil
  "This hook is executed, then cleared each time `defclass' is called.")

(defvar eieio-error-unsupported-class-tags nil
  "Non-nil to throw an error if an encountered tag is unsupported.
This may prevent classes from CLOS applications from being used with EIEIO
since EIEIO does not support all CLOS tags.")

(defvar eieio-skip-typecheck nil
  "If non-nil, skip all slot typechecking.
Set this to t permanently if a program is functioning well to get a
small speed increase.  This variable is also used internally to handle
default setting for optimization purposes.")

(defvar eieio-optimize-primary-methods-flag t
  "Non-nil means to optimize the method dispatch on primary methods.")

(defvar eieio-backward-compatibility t
  "If nil, drop support for some behaviors of older versions of EIEIO.
Currently under control of this var:
- Define every class as a var whose value is the class symbol.
- Define <class>-child-p and <class>-list-p predicates.
- Allow object names in constructors.")

(defconst eieio-unbound
  (if (and (boundp 'eieio-unbound) (symbolp eieio-unbound))
      eieio-unbound
    (make-symbol "unbound"))
  "Uninterned symbol representing an unbound slot in an object.")

;; This is a bootstrap for eieio-default-superclass so it has a value
;; while it is being built itself.
(defvar eieio-default-superclass nil)

(progn
  ;; Arrange for field access not to bother checking if the access is indeed
  ;; made to an eieio--class object.
  (cl-declaim (optimize (safety 0)))
(cl-defstruct (eieio--class
               (:constructor nil)
               (:constructor eieio--class-make (symbol &aux (tag 'defclass)))
               (:type vector)
               (:copier nil))
  ;; We use an untagged cl-struct, with our own hand-made tag as first field
  ;; (containing the symbol `defclass').  It would be better to use a normal
  ;; cl-struct with its normal tag (e.g. so that cl-defstruct can define the
  ;; predicate for us), but that breaks compatibility with .elc files compiled
  ;; against older versions of EIEIO.
  tag
  symbol ;; symbol (self-referencing)
  parent children
  symbol-hashtable ;; hashtable permitting fast access to variable position indexes
  ;; @todo
  ;; the word "public" here is leftovers from the very first version.
  ;; Get rid of it!
  public-a                        ;; class attribute index
  public-d                        ;; class attribute defaults index
  public-doc                      ;; class documentation strings for attributes
  public-type                     ;; class type for a slot
  public-custom                   ;; class custom type for a slot
  public-custom-label             ;; class custom group for a slot
  public-custom-group             ;; class custom group for a slot
  public-printer                  ;; printer for a slot
  protection                      ;; protection for a slot
  initarg-tuples                  ;; initarg tuples list
  class-allocation-a              ;; class allocated attributes
  class-allocation-doc            ;; class allocated documentation
  class-allocation-type           ;; class allocated value type
  class-allocation-custom         ;; class allocated custom descriptor
  class-allocation-custom-label   ;; class allocated custom descriptor
  class-allocation-custom-group   ;; class allocated custom group
  class-allocation-printer        ;; class allocated printer for a slot
  class-allocation-protection     ;; class allocated protection list
  class-allocation-values         ;; class allocated value vector
  default-object-cache ;; what a newly created object would look like.
                       ; This will speed up instantiation time as
                       ; only a `copy-sequence' will be needed, instead of
                       ; looping over all the values and setting them from
                       ; the default.
  options ;; storage location of tagged class option
          ; Stored outright without modifications or stripping
  )
  ;; Set it back to the default value.
  (cl-declaim (optimize (safety 1))))


(cl-defstruct (eieio--object
               (:type vector)           ;We manage our own tagging system.
               (:constructor nil)
               (:copier nil))
  ;; `class-tag' holds a symbol, which is not the class name, but is instead
  ;; properly prefixed as an internal EIEIO thingy and which holds the class
  ;; object/struct in its `symbol-value' slot.
  class-tag)

(eval-and-compile
  (defconst eieio--object-num-slots
    (length (get 'eieio--object 'cl-struct-slots))))

(defsubst eieio--object-class-object (obj)
  (symbol-value (eieio--object-class-tag obj)))

(defsubst eieio--object-class-name (obj)
  ;; FIXME: Most uses of this function should be changed to use
  ;; eieio--object-class-object instead!
  (eieio--class-symbol (eieio--object-class-object obj)))


;;; Important macros used internally in eieio.

(defmacro eieio--class-v (class)        ;Use a macro, so it acts as a GV place.
  "Internal: Return the class vector from the CLASS symbol."
  (declare (debug t))
  ;; No check: If eieio gets this far, it has probably been checked already.
  `(get ,class 'eieio-class-definition))

(defsubst eieio--class-object (class)
  "Return the class object."
  (if (symbolp class)
      ;; Keep the symbol if class-v is nil, for better error messages.
      (or (eieio--class-v class) class)
    class))

(defsubst eieio--class-p (class)
  "Return non-nil if CLASS is a valid class object."
  (condition-case nil
      (eq (aref class 0) 'defclass)
    (error nil)))

(defun class-p (class)
  "Return non-nil if CLASS is a valid class vector.
CLASS is a symbol."                     ;FIXME: Is it a vector or a symbol?
  (and (symbolp class) (eieio--class-p (eieio--class-v class))))

(defun eieio-class-name (class)
  "Return a Lisp like symbol name for CLASS."
  ;; FIXME: What's a "Lisp like symbol name"?
  ;; FIXME: CLOS returns a symbol, but the code returns a string.
  (if (eieio--class-p class) (setq class (eieio--class-symbol class)))
  (cl-check-type class class)
  ;; I think this is supposed to return a symbol, but to me CLASS is a symbol,
  ;; and I wanted a string.  Arg!
  (format "#<class %s>" (symbol-name class)))
(define-obsolete-function-alias 'class-name #'eieio-class-name "24.4")

(defalias 'eieio--class-constructor #'identity
  "Return the symbol representing the constructor of CLASS.")

(defmacro eieio--class-option-assoc (list option)
  "Return from LIST the found OPTION, or nil if it doesn't exist."
  `(car-safe (cdr (memq ,option ,list))))

(defsubst eieio--class-option (class option)
  "Return the value stored for CLASS' OPTION.
Return nil if that option doesn't exist."
  (eieio--class-option-assoc (eieio--class-options class) option))

(defun eieio-object-p (obj)
  "Return non-nil if OBJ is an EIEIO object."
  (and (vectorp obj)
       (> (length obj) 0)
       (let ((tag (eieio--object-class-tag obj)))
         (and (symbolp tag)
              ;; (eq (symbol-function tag) :quick-object-witness-check)
              (boundp tag)
              (eieio--class-p (symbol-value tag))))))

(define-obsolete-function-alias 'object-p 'eieio-object-p "25.1")

(defsubst class-abstract-p (class)
  "Return non-nil if CLASS is abstract.
Abstract classes cannot be instantiated."
  (eieio--class-option (eieio--class-v class) :abstract))

(defsubst eieio--class-method-invocation-order (class)
  "Return the invocation order of CLASS.
Abstract classes cannot be instantiated."
  (or (eieio--class-option class :method-invocation-order)
      :breadth-first))



;;;
;; Class Creation

(defvar eieio-defclass-autoload-map (make-hash-table)
  "Symbol map of superclasses we find in autoloads.")

;; We autoload this because it's used in `make-autoload'.
;;;###autoload
(defun eieio-defclass-autoload (cname _superclasses filename doc)
  "Create autoload symbols for the EIEIO class CNAME.
SUPERCLASSES are the superclasses that CNAME inherits from.
DOC is the docstring for CNAME.
This function creates a mock-class for CNAME and adds it into
SUPERCLASSES as children.
It creates an autoload function for CNAME's constructor."
  ;; Assume we've already debugged inputs.

  ;; We used to store the list of superclasses in the `parent' slot (as a list
  ;; of class names).  But now this slot holds a list of class objects, and
  ;; those parents may not exist yet, so the corresponding class objects may
  ;; simply not exist yet.  So instead we just don't store the list of parents
  ;; here in eieio-defclass-autoload at all, since it seems that they're just
  ;; not needed before the class is actually loaded.
  (let* ((oldc (eieio--class-v cname))
	 (newc (eieio--class-make cname)))
    (if (eieio--class-p oldc)
	nil ;; Do nothing if we already have this class.

      ;; turn this into a usable self-pointing symbol
      (when eieio-backward-compatibility
        (set cname cname)
        (make-obsolete-variable cname (format "use '%s instead" cname) "25.1"))

      ;; Store the new class vector definition into the symbol.  We need to
      ;; do this first so that we can call defmethod for the accessor.
      ;; The vector will be updated by the following while loop and will not
      ;; need to be stored a second time.
      (setf (eieio--class-v cname) newc)

      ;; Create an autoload on top of our constructor function.
      (autoload cname filename doc nil nil)
      (autoload (intern (format "%s-p" cname)) filename "" nil nil)
      (when eieio-backward-compatibility
        (autoload (intern (format "%s-child-p" cname)) filename "" nil nil)
        (autoload (intern (format "%s-list-p" cname)) filename "" nil nil)))))

(defsubst eieio-class-un-autoload (cname)
  "If class CNAME is in an autoload state, load its file."
  (autoload-do-load (symbol-function cname))) ; cname

(cl-deftype list-of (elem-type)
  `(and list
        (satisfies (lambda (list)
                     (cl-every (lambda (elem) (cl-typep elem ',elem-type))
                               list)))))


(defun eieio-make-class-predicate (class)
  (lambda (obj)
    (:documentation
     (format "Return non-nil if OBJ is an object of type `%S'.\n\n(fn OBJ)"
             class))
    (and (eieio-object-p obj)
         (same-class-p obj class))))

(defun eieio-make-child-predicate (class)
  (lambda (obj)
    (:documentation
     (format "Return non-nil if OBJ is an object of type `%S' or a subclass.
\n(fn OBJ)" class))
    (and (eieio-object-p obj)
         (object-of-class-p obj class))))

(defun eieio-defclass-internal (cname superclasses slots options)
  "Define CNAME as a new subclass of SUPERCLASSES.
SLOTS are the slots residing in that class definition, and OPTIONS
holds the class options.
See `defclass' for more information."
  ;; Run our eieio-hook each time, and clear it when we are done.
  ;; This way people can add hooks safely if they want to modify eieio
  ;; or add definitions when eieio is loaded or something like that.
  (run-hooks 'eieio-hook)
  (setq eieio-hook nil)

  (let* ((oldc (let ((c (eieio--class-v cname))) (if (eieio--class-p c) c)))
	 (newc (if (and oldc (not (eieio--class-default-object-cache oldc)))
                   ;; The oldc class is a stub setup by eieio-defclass-autoload.
                   ;; Reuse it instead of creating a new one, so that existing
                   ;; references are still valid.
                   oldc
                 (eieio--class-make cname)))
	 (groups nil) ;; list of groups id'd from slots
	 (clearparent nil))

    ;; If this class already existed, and we are updating its structure,
    ;; make sure we keep the old child list.  This can cause bugs, but
    ;; if no new slots are created, it also saves time, and prevents
    ;; method table breakage, particularly when the users is only
    ;; byte compiling an EIEIO file.
    (if oldc
	(setf (eieio--class-children newc) (eieio--class-children oldc))
      ;; If the old class did not exist, but did exist in the autoload map,
      ;; then adopt those children.  This is like the above, but deals with
      ;; autoloads nicely.
      (let ((children (gethash cname eieio-defclass-autoload-map)))
	(when children
          (setf (eieio--class-children newc) children)
	  (remhash cname eieio-defclass-autoload-map))))

    (if superclasses
	(progn
	  (dolist (p superclasses)
	    (if (not (and p (symbolp p)))
		(error "Invalid parent class %S" p)
              (let ((c (eieio--class-v p)))
                (if (not (eieio--class-p c))
		    ;; bad class
		    (error "Given parent class %S is not a class" p)
		  ;; good parent class...
		  ;; save new child in parent
                  (cl-pushnew cname (eieio--class-children c))
		  ;; Get custom groups, and store them into our local copy.
		  (mapc (lambda (g) (cl-pushnew g groups :test #'equal))
			(eieio--class-option c :custom-groups))
		  ;; Save parent in child.
                  (push c (eieio--class-parent newc))))))
	  ;; Reverse the list of our parents so that they are prioritized in
	  ;; the same order as specified in the code.
	  (cl-callf nreverse (eieio--class-parent newc)))
      ;; If there is nothing to loop over, then inherit from the
      ;; default superclass.
      (unless (eq cname 'eieio-default-superclass)
	;; adopt the default parent here, but clear it later...
	(setq clearparent t)
        ;; save new child in parent
        (cl-pushnew cname (eieio--class-children eieio-default-superclass))
        ;; save parent in child
        (setf (eieio--class-parent newc) (list eieio-default-superclass))))

    ;; turn this into a usable self-pointing symbol;  FIXME: Why?
    (when eieio-backward-compatibility
      (set cname cname)
      (make-obsolete-variable cname (format "use '%s instead" cname) "25.1"))

    ;; Create a handy list of the class test too
    (when eieio-backward-compatibility
      (let ((csym (intern (concat (symbol-name cname) "-list-p"))))
        (defalias csym
              `(lambda (obj)
                 ,(format
                   "Test OBJ to see if it a list of objects which are a child of type %s"
                   cname)
                 (when (listp obj)
                   (let ((ans t)) ;; nil is valid
                     ;; Loop over all the elements of the input list, test
                     ;; each to make sure it is a child of the desired object class.
                     (while (and obj ans)
                       (setq ans (and (eieio-object-p (car obj))
                                      (object-of-class-p (car obj) ,cname)))
                       (setq obj (cdr obj)))
                     ans))))
        (make-obsolete csym (format "use (cl-typep ... '(list-of %s)) instead"
                                    cname)
                       "25.1")))

    ;; Before adding new slots, let's add all the methods and classes
    ;; in from the parent class.
    (eieio-copy-parents-into-subclass newc)

    ;; Store the new class vector definition into the symbol.  We need to
    ;; do this first so that we can call defmethod for the accessor.
    ;; The vector will be updated by the following while loop and will not
    ;; need to be stored a second time.
    (setf (eieio--class-v cname) newc)

    ;; Query each slot in the declaration list and mangle into the
    ;; class structure I have defined.
    (pcase-dolist (`(,name . ,slot) slots)
      (let* ((init    (or (plist-get slot :initform)
			  (if (member :initform slot) nil
			    eieio-unbound)))
	     (initarg (plist-get slot :initarg))
	     (docstr  (plist-get slot :documentation))
	     (prot    (plist-get slot :protection))
	     (alloc   (plist-get slot :allocation))
	     (type    (plist-get slot :type))
	     (custom  (plist-get slot :custom))
	     (label   (plist-get slot :label))
	     (customg (plist-get slot :group))
	     (printer (plist-get slot :printer))

	     (skip-nil (eieio--class-option-assoc options :allow-nil-initform))
	     )

	;; Clean up the meaning of protection.
        (setq prot
              (pcase prot
                ((or 'nil 'public ':public) nil)
                ((or 'protected ':protected) 'protected)
                ((or 'private ':private) 'private)
                (_ (signal 'invalid-slot-type (list :protection prot)))))

	;; The default type specifier is supposed to be t, meaning anything.
	(if (not type) (setq type t))

	;; intern the symbol so we can use it blankly
        (if eieio-backward-compatibility
            (and initarg (not (keywordp initarg))
                 (progn
                   (set initarg initarg)
                   (make-obsolete-variable
                    initarg (format "use '%s instead" initarg) "25.1"))))

	;; The customgroup should be a list of symbols
	(cond ((null customg)
	       (setq customg '(default)))
	      ((not (listp customg))
	       (setq customg (list customg))))
	;; The customgroup better be a symbol, or list of symbols.
	(mapc (lambda (cg)
		(if (not (symbolp cg))
		    (signal 'invalid-slot-type (list :group cg))))
		customg)

	;; First up, add this slot into our new class.
	(eieio--add-new-slot newc name init docstr type custom label customg printer
			     prot initarg alloc 'defaultoverride skip-nil)

	;; We need to id the group, and store them in a group list attribute.
	(dolist (cg customg)
          (cl-pushnew cg groups :test 'equal))
	))

    ;; Now that everything has been loaded up, all our lists are backwards!
    ;; Fix that up now.
    (cl-callf nreverse (eieio--class-public-a newc))
    (cl-callf nreverse (eieio--class-public-d newc))
    (cl-callf nreverse (eieio--class-public-doc newc))
    (cl-callf (lambda (types) (apply #'vector (nreverse types)))
        (eieio--class-public-type newc))
    (cl-callf nreverse (eieio--class-public-custom newc))
    (cl-callf nreverse (eieio--class-public-custom-label newc))
    (cl-callf nreverse (eieio--class-public-custom-group newc))
    (cl-callf nreverse (eieio--class-public-printer newc))
    (cl-callf nreverse (eieio--class-protection newc))
    (cl-callf nreverse (eieio--class-initarg-tuples newc))

    ;; The storage for class-class-allocation-type needs to be turned into
    ;; a vector now.
    (cl-callf (lambda (cat) (apply #'vector cat))
        (eieio--class-class-allocation-type newc))

    ;; Also, take class allocated values, and vectorize them for speed.
    (cl-callf (lambda (cavs) (apply #'vector cavs))
        (eieio--class-class-allocation-values newc))

    ;; Attach slot symbols into a hashtable, and store the index of
    ;; this slot as the value this table.
    (let* ((cnt 0)
	   (pubsyms (eieio--class-public-a newc))
	   (prots (eieio--class-protection newc))
	   (oa (make-hash-table :test #'eq)))
      (while pubsyms
	(let ((newsym (list cnt)))
          (setf (gethash (car pubsyms) oa) newsym)
          (setq cnt (1+ cnt))
          (if (car prots) (setcdr newsym (car prots))))
	(setq pubsyms (cdr pubsyms)
	      prots (cdr prots)))
      (setf (eieio--class-symbol-hashtable newc) oa))

    ;; Set up a specialized doc string.
    ;; Use stored value since it is calculated in a non-trivial way
    (put cname 'variable-documentation
	 (eieio--class-option-assoc options :documentation))

    ;; Save the file location where this class is defined.
    (add-to-list 'current-load-list `(eieio-defclass . ,cname))

    ;; We have a list of custom groups.  Store them into the options.
    (let ((g (eieio--class-option-assoc options :custom-groups)))
      (mapc (lambda (cg) (cl-pushnew cg g :test 'equal)) groups)
      (if (memq :custom-groups options)
	  (setcar (cdr (memq :custom-groups options)) g)
	(setq options (cons :custom-groups (cons g options)))))

    ;; Set up the options we have collected.
    (setf (eieio--class-options newc) options)

    ;; if this is a superclass, clear out parent (which was set to the
    ;; default superclass eieio-default-superclass)
    (if clearparent (setf (eieio--class-parent newc) nil))

    ;; Create the cached default object.
    (let ((cache (make-vector (+ (length (eieio--class-public-a newc))
                                 (eval-when-compile eieio--object-num-slots))
                              nil))
          ;; We don't strictly speaking need to use a symbol, but the old
          ;; code used the class's name rather than the class's object, so
          ;; we follow this preference for using a symbol, which is probably
          ;; convenient to keep the printed representation of such Elisp
          ;; objects readable.
          (tag (intern (format "eieio-class-tag--%s" cname))))
      (set tag newc)
      (fset tag :quick-object-witness-check)
      (setf (eieio--object-class-tag cache) tag)
      (let ((eieio-skip-typecheck t))
	;; All type-checking has been done to our satisfaction
	;; before this call.  Don't waste our time in this call..
	(eieio-set-defaults cache t))
      (setf (eieio--class-default-object-cache newc) cache))

    ;; Return our new class object
    ;; newc
    cname
    ))

(defsubst eieio-eval-default-p (val)
  "Whether the default value VAL should be evaluated for use."
  (and (consp val) (symbolp (car val)) (fboundp (car val))))

(defun eieio--perform-slot-validation-for-default (slot spec value skipnil)
  "For SLOT, signal if SPEC does not match VALUE.
If SKIPNIL is non-nil, then if VALUE is nil return t instead."
  (if (not (or (eieio-eval-default-p value) ;FIXME: Why?
               eieio-skip-typecheck
               (and skipnil (null value))
               (eieio--perform-slot-validation spec value)))
      (signal 'invalid-slot-type (list slot spec value))))

(defun eieio--add-new-slot (newc a d doc type cust label custg print prot init alloc
				 &optional defaultoverride skipnil)
  "Add into NEWC attribute A.
If A already exists in NEWC, then do nothing.  If it doesn't exist,
then also add in D (default), DOC, TYPE, CUST, LABEL, CUSTG, PRINT, PROT, and INIT arg.
Argument ALLOC specifies if the slot is allocated per instance, or per class.
If optional DEFAULTOVERRIDE is non-nil, then if A exists in NEWC,
we must override its value for a default.
Optional argument SKIPNIL indicates if type checking should be skipped
if default value is nil."
  ;; Make sure we duplicate those items that are sequences.
  (condition-case nil
      (if (sequencep d) (setq d (copy-sequence d)))
    ;; This copy can fail on a cons cell with a non-cons in the cdr.  Let's skip it if it doesn't work.
    (error nil))
  (if (sequencep type) (setq type (copy-sequence type)))
  (if (sequencep cust) (setq cust (copy-sequence cust)))
  (if (sequencep custg) (setq custg (copy-sequence custg)))

  ;; To prevent override information w/out specification of storage,
  ;; we need to do this little hack.
  (if (member a (eieio--class-class-allocation-a newc)) (setq alloc :class))

  (if (or (not alloc) (and (symbolp alloc) (eq alloc :instance)))
      ;; In this case, we modify the INSTANCE version of a given slot.

      (progn

	;; Only add this element if it is so-far unique
	(if (not (member a (eieio--class-public-a newc)))
	    (progn
	      (eieio--perform-slot-validation-for-default a type d skipnil)
	      (push a (eieio--class-public-a newc))
	      (push d (eieio--class-public-d newc))
	      (push doc (eieio--class-public-doc newc))
	      (push type (eieio--class-public-type newc))
	      (push cust (eieio--class-public-custom newc))
	      (push label (eieio--class-public-custom-label newc))
	      (push custg (eieio--class-public-custom-group newc))
	      (push print (eieio--class-public-printer newc))
	      (push prot (eieio--class-protection newc))
	      (setf (eieio--class-initarg-tuples newc) (cons (cons init a) (eieio--class-initarg-tuples newc)))
	      )
	  ;; When defaultoverride is true, we are usually adding new local
	  ;; attributes which must override the default value of any slot
	  ;; passed in by one of the parent classes.
	  (when defaultoverride
	    ;; There is a match, and we must override the old value.
	    (let* ((ca (eieio--class-public-a newc))
		   (np (member a ca))
		   (num (- (length ca) (length np)))
		   (dp (if np (nthcdr num (eieio--class-public-d newc))
			 nil))
		   (tp (if np (nth num (eieio--class-public-type newc))))
		   )
	      (if (not np)
		  (error "EIEIO internal error overriding default value for %s"
			 a)
		;; If type is passed in, is it the same?
		(if (not (eq type t))
		    (if (not (equal type tp))
			(error
			 "Child slot type `%s' does not match inherited type `%s' for `%s'"
			 type tp a)))
		;; If we have a repeat, only update the initarg...
		(unless (eq d eieio-unbound)
		  (eieio--perform-slot-validation-for-default a tp d skipnil)
		  (setcar dp d))
		;; If we have a new initarg, check for it.
		(when init
		  (let* ((inits (eieio--class-initarg-tuples newc))
			 (inita (rassq a inits)))
		    ;; Replace the CAR of the associate INITA.
		    ;;(message "Initarg: %S replace %s" inita init)
		    (setcar inita init)
		    ))

		;; PLN Tue Jun 26 11:57:06 2007 : The protection is
		;; checked and SHOULD match the superclass
		;; protection. Otherwise an error is thrown. However
		;; I wonder if a more flexible schedule might be
		;; implemented.
		;;
		;; EML - We used to have (if prot... here,
		;;       but a prot of 'nil means public.
		;;
		(let ((super-prot (nth num (eieio--class-protection newc)))
		      )
		  (if (not (eq prot super-prot))
		      (error "Child slot protection `%s' does not match inherited protection `%s' for `%s'"
			     prot super-prot a)))
		;; End original PLN

		;; PLN Tue Jun 26 11:57:06 2007 :
		;; Do a non redundant combination of ancient custom
		;; groups and new ones.
		(when custg
		  (let* ((groups
			  (nthcdr num (eieio--class-public-custom-group newc)))
			 (list1 (car groups))
			 (list2 (if (listp custg) custg (list custg))))
		    (if (< (length list1) (length list2))
			(setq list1 (prog1 list2 (setq list2 list1))))
		    (dolist (elt list2)
		      (unless (memq elt list1)
			(push elt list1)))
		    (setcar groups list1)))
		;;  End PLN

		;;  PLN Mon Jun 25 22:44:34 2007 : If a new cust is
		;;  set, simply replaces the old one.
		(when cust
		  ;; (message "Custom type redefined to %s" cust)
		  (setcar (nthcdr num (eieio--class-public-custom newc)) cust))

		;; If a new label is specified, it simply replaces
		;; the old one.
		(when label
		  ;; (message "Custom label redefined to %s" label)
		  (setcar (nthcdr num (eieio--class-public-custom-label newc)) label))
		;;  End PLN

		;; PLN Sat Jun 30 17:24:42 2007 : when a new
		;; doc is specified, simply replaces the old one.
		(when doc
		  ;;(message "Documentation redefined to %s" doc)
		  (setcar (nthcdr num (eieio--class-public-doc newc))
			  doc))
		;; End PLN

		;; If a new printer is specified, it simply replaces
		;; the old one.
		(when print
		  ;; (message "printer redefined to %s" print)
		  (setcar (nthcdr num (eieio--class-public-printer newc)) print))

		)))
	  ))

    ;; CLASS ALLOCATED SLOTS
    (let ((value (eieio-default-eval-maybe d)))
      (if (not (member a (eieio--class-class-allocation-a newc)))
	  (progn
	    (eieio--perform-slot-validation-for-default a type value skipnil)
	    ;; Here we have found a :class version of a slot.  This
	    ;; requires a very different approach.
	    (push a (eieio--class-class-allocation-a newc))
	    (push doc (eieio--class-class-allocation-doc newc))
	    (push type (eieio--class-class-allocation-type newc))
	    (push cust (eieio--class-class-allocation-custom newc))
	    (push label (eieio--class-class-allocation-custom-label newc))
	    (push custg (eieio--class-class-allocation-custom-group newc))
	    (push prot (eieio--class-class-allocation-protection newc))
	    ;; Default value is stored in the 'values section, since new objects
	    ;; can't initialize from this element.
	    (push value (eieio--class-class-allocation-values newc)))
	(when defaultoverride
	  ;; There is a match, and we must override the old value.
	  (let* ((ca (eieio--class-class-allocation-a newc))
		 (np (member a ca))
		 (num (- (length ca) (length np)))
		 (dp (if np
			 (nthcdr num
				 (eieio--class-class-allocation-values newc))
		       nil))
		 (tp (if np (nth num (eieio--class-class-allocation-type newc))
		       nil)))
	    (if (not np)
		(error "EIEIO internal error overriding default value for %s"
		       a)
	      ;; If type is passed in, is it the same?
	      (if (not (eq type t))
		  (if (not (equal type tp))
		      (error
		       "Child slot type `%s' does not match inherited type `%s' for `%s'"
		       type tp a)))
	      ;; EML - Note: the only reason to override a class bound slot
	      ;;       is to change the default, so allow unbound in.

	      ;; If we have a repeat, only update the value...
	      (eieio--perform-slot-validation-for-default a tp value skipnil)
	      (setcar dp value))

	    ;; PLN Tue Jun 26 11:57:06 2007 : The protection is
	    ;; checked and SHOULD match the superclass
	    ;; protection. Otherwise an error is thrown. However
	    ;; I wonder if a more flexible schedule might be
	    ;; implemented.
	    (let ((super-prot
		   (car (nthcdr num (eieio--class-class-allocation-protection newc)))))
	      (if (not (eq prot super-prot))
		  (error "Child slot protection `%s' does not match inherited protection `%s' for `%s'"
			 prot super-prot a)))
	    ;; Do a non redundant combination of ancient custom groups
	    ;; and new ones.
	    (when custg
	      (let* ((groups
		      (nthcdr num (eieio--class-class-allocation-custom-group newc)))
		     (list1 (car groups))
		     (list2 (if (listp custg) custg (list custg))))
		(if (< (length list1) (length list2))
		    (setq list1 (prog1 list2 (setq list2 list1))))
		(dolist (elt list2)
		  (unless (memq elt list1)
		    (push elt list1)))
		(setcar groups list1)))

	    ;; PLN Sat Jun 30 17:24:42 2007 : when a new
	    ;; doc is specified, simply replaces the old one.
	    (when doc
	      ;;(message "Documentation redefined to %s" doc)
	      (setcar (nthcdr num (eieio--class-class-allocation-doc newc))
		      doc))
	    ;; End PLN

	    ;; If a new printer is specified, it simply replaces
	    ;; the old one.
	    (when print
	      ;; (message "printer redefined to %s" print)
	      (setcar (nthcdr num (eieio--class-class-allocation-printer newc)) print))

	    ))
	))
    ))

(defun eieio-copy-parents-into-subclass (newc)
  "Copy into NEWC the slots of PARENTS.
Follow the rules of not overwriting early parents when applying to
the new child class."
  (let ((sn (eieio--class-option-assoc (eieio--class-options newc)
                                       :allow-nil-initform)))
    (dolist (pcv (eieio--class-parent newc))
      ;; First, duplicate all the slots of the parent.
      (let ((pa (eieio--class-public-a pcv))
            (pd (eieio--class-public-d pcv))
            (pdoc (eieio--class-public-doc pcv))
            (ptype (eieio--class-public-type pcv))
            (pcust (eieio--class-public-custom pcv))
            (plabel (eieio--class-public-custom-label pcv))
            (pcustg (eieio--class-public-custom-group pcv))
            (printer (eieio--class-public-printer pcv))
            (pprot (eieio--class-protection pcv))
            (pinit (eieio--class-initarg-tuples pcv))
            (i 0))
        (while pa
          (eieio--add-new-slot newc
                               (car pa) (car pd) (car pdoc) (aref ptype i)
                               (car pcust) (car plabel) (car pcustg)
                               (car printer)
                               (car pprot) (car-safe (car pinit)) nil nil sn)
          ;; Increment each value.
          (setq pa (cdr pa)
                pd (cdr pd)
                pdoc (cdr pdoc)
                i (1+ i)
                pcust (cdr pcust)
                plabel (cdr plabel)
                pcustg (cdr pcustg)
                printer (cdr printer)
                pprot (cdr pprot)
                pinit (cdr pinit))
          )) ;; while/let
      ;; Now duplicate all the class alloc slots.
      (let ((pa (eieio--class-class-allocation-a pcv))
            (pdoc (eieio--class-class-allocation-doc pcv))
            (ptype (eieio--class-class-allocation-type pcv))
            (pcust (eieio--class-class-allocation-custom pcv))
            (plabel (eieio--class-class-allocation-custom-label pcv))
            (pcustg (eieio--class-class-allocation-custom-group pcv))
            (printer (eieio--class-class-allocation-printer pcv))
            (pprot (eieio--class-class-allocation-protection pcv))
            (pval (eieio--class-class-allocation-values pcv))
            (i 0))
        (while pa
          (eieio--add-new-slot newc
                               (car pa) (aref pval i) (car pdoc) (aref ptype i)
                               (car pcust) (car plabel) (car pcustg)
                               (car printer)
                               (car pprot) nil :class sn)
          ;; Increment each value.
          (setq pa (cdr pa)
                pdoc (cdr pdoc)
                pcust (cdr pcust)
                plabel (cdr plabel)
                pcustg (cdr pcustg)
                printer (cdr printer)
                pprot (cdr pprot)
                i (1+ i))
          )))))


;;; Slot type validation

;; This is a hideous hack for replacing `typep' from cl-macs, to avoid
;; requiring the CL library at run-time.  It can be eliminated if/when
;; `typep' is merged into Emacs core.

(defun eieio--perform-slot-validation (spec value)
  "Return non-nil if SPEC does not match VALUE."
  (or (eq spec t)			; t always passes
      (eq value eieio-unbound)		; unbound always passes
      (cl-typep value spec)))

(defun eieio--validate-slot-value (class slot-idx value slot)
  "Make sure that for CLASS referencing SLOT-IDX, VALUE is valid.
Checks the :type specifier.
SLOT is the slot that is being checked, and is only used when throwing
an error."
  (if eieio-skip-typecheck
      nil
    ;; Trim off object IDX junk added in for the object index.
    (setq slot-idx (- slot-idx (eval-when-compile eieio--object-num-slots)))
    (let ((st (aref (eieio--class-public-type class) slot-idx)))
      (if (not (eieio--perform-slot-validation st value))
	  (signal 'invalid-slot-type
                  (list (eieio--class-symbol class) slot st value))))))

(defun eieio--validate-class-slot-value (class slot-idx value slot)
  "Make sure that for CLASS referencing SLOT-IDX, VALUE is valid.
Checks the :type specifier.
SLOT is the slot that is being checked, and is only used when throwing
an error."
  (if eieio-skip-typecheck
      nil
    (let ((st (aref (eieio--class-class-allocation-type class)
		    slot-idx)))
      (if (not (eieio--perform-slot-validation st value))
	  (signal 'invalid-slot-type
                  (list (eieio--class-symbol class) slot st value))))))

(defun eieio-barf-if-slot-unbound (value instance slotname fn)
  "Throw a signal if VALUE is a representation of an UNBOUND slot.
INSTANCE is the object being referenced.  SLOTNAME is the offending
slot.  If the slot is ok, return VALUE.
Argument FN is the function calling this verifier."
  (if (and (eq value eieio-unbound) (not eieio-skip-typecheck))
      (slot-unbound instance (eieio--object-class-name instance) slotname fn)
    value))


;;; Get/Set slots in an object.
;;
(defun eieio-oref (obj slot)
  "Return the value in OBJ at SLOT in the object vector."
  (cl-check-type slot symbol)
  (cl-check-type obj (or eieio-object class))
  (let* ((class (cond ((symbolp obj)
                       (error "eieio-oref called on a class!")
                       (let ((c (eieio--class-v obj)))
                         (if (eieio--class-p c) (eieio-class-un-autoload obj))
                         c))
                      (t (eieio--object-class-object obj))))
	 (c (eieio--slot-name-index class slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c (eieio--class-slot-name-index class slot))
	    ;; Oref that slot.
	    (aref (eieio--class-class-allocation-values class) c)
	  ;; The slot-missing method is a cool way of allowing an object author
	  ;; to intercept missing slot definitions.  Since it is also the LAST
	  ;; thing called in this fn, its return value would be retrieved.
	  (slot-missing obj slot 'oref)
	  ;;(signal 'invalid-slot-name (list (eieio-object-name obj) slot))
	  )
      (cl-check-type obj eieio-object)
      (eieio-barf-if-slot-unbound (aref obj c) obj slot 'oref))))


(defun eieio-oref-default (obj slot)
  "Do the work for the macro `oref-default' with similar parameters.
Fills in OBJ's SLOT with its default value."
  (cl-check-type obj (or eieio-object class))
  (cl-check-type slot symbol)
  (let* ((cl (cond ((symbolp obj) (eieio--class-v obj))
                   (t (eieio--object-class-object obj))))
	 (c (eieio--slot-name-index cl slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c
		  (eieio--class-slot-name-index cl slot))
	    ;; Oref that slot.
	    (aref (eieio--class-class-allocation-values cl)
		  c)
	  (slot-missing obj slot 'oref-default)
	  ;;(signal 'invalid-slot-name (list (class-name cl) slot))
	  )
      (eieio-barf-if-slot-unbound
       (let ((val (nth (- c (eval-when-compile eieio--object-num-slots))
                       (eieio--class-public-d cl))))
	 (eieio-default-eval-maybe val))
       obj (eieio--class-symbol cl) 'oref-default))))

(defun eieio-default-eval-maybe (val)
  "Check VAL, and return what `oref-default' would provide."
  ;; FIXME: What the hell is this supposed to do?  Shouldn't it evaluate
  ;; variables as well?  Why not just always call `eval'?
  (cond
   ;; Is it a function call?  If so, evaluate it.
   ((eieio-eval-default-p val)
    (eval val))
   ;;;; check for quoted things, and unquote them
   ;;((and (consp val) (eq (car val) 'quote))
   ;; (car (cdr val)))
   ;; return it verbatim
   (t val)))

(defun eieio-oset (obj slot value)
  "Do the work for the macro `oset'.
Fills in OBJ's SLOT with VALUE."
  (cl-check-type obj eieio-object)
  (cl-check-type slot symbol)
  (let* ((class (eieio--object-class-object obj))
         (c (eieio--slot-name-index class slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c
		  (eieio--class-slot-name-index class slot))
	    ;; Oset that slot.
	    (progn
	      (eieio--validate-class-slot-value class c value slot)
	      (aset (eieio--class-class-allocation-values class)
		    c value))
	  ;; See oref for comment on `slot-missing'
	  (slot-missing obj slot 'oset value)
	  ;;(signal 'invalid-slot-name (list (eieio-object-name obj) slot))
	  )
      (eieio--validate-slot-value class c value slot)
      (aset obj c value))))

(defun eieio-oset-default (class slot value)
  "Do the work for the macro `oset-default'.
Fills in the default value in CLASS' in SLOT with VALUE."
  (setq class (eieio--class-object class))
  (cl-check-type class eieio--class)
  (cl-check-type slot symbol)
  (let* ((c (eieio--slot-name-index class slot)))
    (if (not c)
        ;; It might be missing because it is a :class allocated slot.
        ;; Let's check that info out.
        (if (setq c (eieio--class-slot-name-index class slot))
            (progn
              ;; Oref that slot.
              (eieio--validate-class-slot-value class c value slot)
              (aset (eieio--class-class-allocation-values class) c
                    value))
          (signal 'invalid-slot-name (list (eieio--class-symbol class) slot)))
      (eieio--validate-slot-value class c value slot)
      ;; Set this into the storage for defaults.
      (if (eieio-eval-default-p value)
          (error "Can't set default to a sexp that gets evaluated again"))
      (setcar (nthcdr (- c (eval-when-compile eieio--object-num-slots))
                      (eieio--class-public-d class))
              value)
      ;; Take the value, and put it into our cache object.
      (eieio-oset (eieio--class-default-object-cache class)
                  slot value)
      )))


;;; EIEIO internal search functions
;;
(defun eieio--slot-name-index (class slot)
  "In CLASS find the index of the named SLOT.
The slot is a symbol which is installed in CLASS by the `defclass' call.
If SLOT is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; Removed checks to outside this call
  (let* ((fsym (gethash slot (eieio--class-symbol-hashtable class)))
	 (fsi (car fsym)))
    (if (integerp fsi)
        (+ (eval-when-compile eieio--object-num-slots) fsi)
      (let ((fn (eieio--initarg-to-attribute class slot)))
	(if fn (eieio--slot-name-index class fn) nil)))))

(defun eieio--class-slot-name-index (class slot)
  "In CLASS find the index of the named SLOT.
The slot is a symbol which is installed in CLASS by the `defclass'
call.  If SLOT is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; This will happen less often, and with fewer slots.  Do this the
  ;; storage cheap way.
  (let* ((a (eieio--class-class-allocation-a class))
	 (l1 (length a))
	 (af (memq slot a))
	 (l2 (length af)))
    ;; Slot # is length of the total list, minus the remaining list of
    ;; the found slot.
    (if af (- l1 l2))))

;;;
;; Way to assign slots based on a list.  Used for constructors, or
;; even resetting an object at run-time
;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all slots to their defaults.
If SET-ALL is non-nil, then when a default is nil, that value is
reset.  If SET-ALL is nil, the slots are only reset if the default is
not nil."
  (let ((pub (eieio--class-public-a (eieio--object-class-object obj))))
    (while pub
      (let ((df (eieio-oref-default obj (car pub))))
        (if (or df set-all)
            (eieio-oset obj (car pub) df)))
      (setq pub (cdr pub)))))

(defun eieio--initarg-to-attribute (class initarg)
  "For CLASS, convert INITARG to the actual attribute name.
If there is no translation, pass it in directly (so we can cheat if
need be... May remove that later...)"
  (let ((tuple (assoc initarg (eieio--class-initarg-tuples class))))
    (if tuple
	(cdr tuple)
      nil)))

;;;
;; Method Invocation order: C3
(defun eieio--c3-candidate (class remaining-inputs)
  "Return CLASS if it can go in the result now, otherwise nil."
  ;; Ensure CLASS is not in any position but the first in any of the
  ;; element lists of REMAINING-INPUTS.
  (and (not (let ((found nil))
	      (while (and remaining-inputs (not found))
		(setq found (member class (cdr (car remaining-inputs)))
		      remaining-inputs (cdr remaining-inputs)))
	      found))
       class))

(defun eieio--c3-merge-lists (reversed-partial-result remaining-inputs)
  "Merge REVERSED-PARTIAL-RESULT REMAINING-INPUTS in a consistent order, if possible.
If a consistent order does not exist, signal an error."
  (if (let ((tail remaining-inputs)
	    (found nil))
	(while (and tail (not found))
	  (setq found (car tail) tail (cdr tail)))
	(not found))
      ;; If all remaining inputs are empty lists, we are done.
      (nreverse reversed-partial-result)
    ;; Otherwise, we try to find the next element of the result. This
    ;; is achieved by considering the first element of each
    ;; (non-empty) input list and accepting a candidate if it is
    ;; consistent with the rests of the input lists.
    (let* ((found nil)
	   (tail remaining-inputs)
	   (next (progn
		   (while (and tail (not found))
		     (setq found (and (car tail)
				      (eieio--c3-candidate (caar tail)
                                                           remaining-inputs))
			   tail (cdr tail)))
		   found)))
      (if next
	  ;; The graph is consistent so far, add NEXT to result and
	  ;; merge input lists, dropping NEXT from their heads where
	  ;; applicable.
	  (eieio--c3-merge-lists
	   (cons next reversed-partial-result)
	   (mapcar (lambda (l) (if (eq (cl-first l) next) (cl-rest l) l))
		   remaining-inputs))
	;; The graph is inconsistent, give up
	(signal 'inconsistent-class-hierarchy (list remaining-inputs))))))

(defun eieio--class-precedence-c3 (class)
  "Return all parents of CLASS in c3 order."
  (let ((parents (eieio--class-parent (eieio--class-v class))))
    (eieio--c3-merge-lists
     (list class)
     (append
      (or
       (mapcar #'eieio--class-precedence-c3 parents)
       `((,eieio-default-superclass)))
      (list parents))))
  )
;;;
;; Method Invocation Order: Depth First

(defun eieio--class-precedence-dfs (class)
  "Return all parents of CLASS in depth-first order."
  (let* ((parents (eieio--class-parent class))
	 (classes (copy-sequence
		   (apply #'append
			  (list class)
			  (or
			   (mapcar
			    (lambda (parent)
			      (cons parent
				    (eieio--class-precedence-dfs parent)))
			    parents)
			   `((,eieio-default-superclass))))))
	 (tail classes))
    ;; Remove duplicates.
    (while tail
      (setcdr tail (delq (car tail) (cdr tail)))
      (setq tail (cdr tail)))
    classes))

;;;
;; Method Invocation Order: Breadth First
(defun eieio--class-precedence-bfs (class)
  "Return all parents of CLASS in breadth-first order."
  (let* ((result)
         (queue (or (eieio--class-parent class)
                    `(,eieio-default-superclass))))
    (while queue
      (let ((head (pop queue)))
	(unless (member head result)
	  (push head result)
	  (unless (eq head eieio-default-superclass)
	    (setq queue (append queue (or (eieio--class-parent head)
					  `(,eieio-default-superclass))))))))
    (cons class (nreverse result)))
  )

;;;
;; Method Invocation Order

(defun eieio--class-precedence-list (class)
  "Return (transitively closed) list of parents of CLASS.
The order, in which the parents are returned depends on the
method invocation orders of the involved classes."
  (if (or (null class) (eq class eieio-default-superclass))
      nil
    (unless (eieio--class-default-object-cache class)
      (eieio-class-un-autoload (eieio--class-symbol class)))
    (cl-case (eieio--class-method-invocation-order class)
      (:depth-first
       (eieio--class-precedence-dfs class))
      (:breadth-first
       (eieio--class-precedence-bfs class))
      (:c3
       (eieio--class-precedence-c3 class))))
  )
(define-obsolete-function-alias
  'class-precedence-list 'eieio--class-precedence-list "24.4")


;;; Here are some special types of errors
;;
(define-error 'invalid-slot-name "Invalid slot name")
(define-error 'invalid-slot-type "Invalid slot type")
(define-error 'unbound-slot "Unbound slot")
(define-error 'inconsistent-class-hierarchy "Inconsistent class hierarchy")

;;; Hooking into cl-generic.

(require 'cl-generic)

;;;; General support to dispatch based on the type of the argument.

(add-function :before-until cl-generic-tagcode-function
              #'eieio--generic-tagcode)
(defun eieio--generic-tagcode (type name)
  ;; CLHS says:
  ;;    A class must be defined before it can be used as a parameter
  ;;    specializer in a defmethod form.
  ;; So we can ignore types that are not known to denote classes.
  (and (eieio--class-p (eieio--class-object type))
       ;; Use the exact same code as for cl-struct, so that methods
       ;; that dispatch on both kinds of objects get to share this
       ;; part of the dispatch code.
       `(50 . ,(cl--generic-struct-tag name))))

(add-function :before-until cl-generic-tag-types-function
              #'eieio--generic-tag-types)
(defun eieio--generic-tag-types (tag)
  (and (symbolp tag) (boundp tag) (eieio--class-p (symbol-value tag))
       (mapcar #'eieio--class-symbol
               (eieio--class-precedence-list (symbol-value tag)))))

;;;; Dispatch for arguments which are classes.

;; Since EIEIO does not support metaclasses, users can't easily use the
;; "dispatch on argument type" for class arguments.  That's why EIEIO's
;; `defmethod' added the :static qualifier.  For cl-generic, such a qualifier
;; would not make much sense (e.g. to which argument should it apply?).
;; Instead, we add a new "subclass" specializer.

(add-function :before-until cl-generic-tagcode-function
              #'eieio--generic-subclass-tagcode)
(defun eieio--generic-subclass-tagcode (type name)
  (when (eq 'subclass (car-safe type))
    `(60 . (and (symbolp ,name) (eieio--class-v ,name)))))

(add-function :before-until cl-generic-tag-types-function
              #'eieio--generic-subclass-tag-types)
(defun eieio--generic-subclass-tag-types (tag)
  (when (eieio--class-p tag)
    (mapcar (lambda (class)
              `(subclass
                ,(if (symbolp class) class (eieio--class-symbol class))))
            (eieio--class-precedence-list tag))))


;;;### (autoloads nil "eieio-compat" "eieio-compat.el" "5b04c9a8fff2bd3f3d3ac54aba0f65b7")
;;; Generated autoloads from eieio-compat.el

(autoload 'eieio--defalias "eieio-compat" "\
Like `defalias', but with less side-effects.
More specifically, it has no side-effects at all when the new function
definition is the same (`eq') as the old one.

\(fn NAME BODY)" nil nil)

(autoload 'defgeneric "eieio-compat" "\
Create a generic function METHOD.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as its purpose is to decide which method body
is appropriate to use.  Uses `defmethod' to create methods, and calls
`defgeneric' for you.  With this implementation the ARGS are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method.

\(fn METHOD ARGS &optional DOC-STRING)" nil t)

(function-put 'defgeneric 'doc-string-elt '3)

(make-obsolete 'defgeneric 'cl-defgeneric '"25.1")

(autoload 'defmethod "eieio-compat" "\
Create a new METHOD through `defgeneric' with ARGS.

The optional second argument KEY is a specifier that
modifies how the method is called, including:
   :before  - Method will be called before the :primary
   :primary - The default if not specified
   :after   - Method will be called after the :primary
   :static  - First arg could be an object or class
The next argument is the ARGLIST.  The ARGLIST specifies the arguments
to the method as with `defun'.  The first argument can have a type
specifier, such as:
  ((VARNAME CLASS) ARG2 ...)
where VARNAME is the name of the local variable for the method being
created.  The CLASS is a class symbol for a class made with `defclass'.
A DOCSTRING comes after the ARGLIST, and is optional.
All the rest of the args are the BODY of the method.  A method will
return the value of the last form in the BODY.

Summary:

 (defmethod mymethod [:before | :primary | :after | :static]
                     ((typearg class-name) arg2 &optional opt &rest rest)
    \"doc-string\"
     body)

\(fn METHOD &rest ARGS)" nil t)

(function-put 'defmethod 'doc-string-elt '3)

(make-obsolete 'defmethod 'cl-defmethod '"25.1")

(autoload 'eieio--defgeneric-init-form "eieio-compat" "\


\(fn METHOD DOC-STRING)" nil nil)

(autoload 'eieio--defmethod "eieio-compat" "\


\(fn METHOD KIND ARGCLASS CODE)" nil nil)

(autoload 'eieio-defmethod "eieio-compat" "\
Obsolete work part of an old version of the `defmethod' macro.

\(fn METHOD ARGS)" nil nil)

(make-obsolete 'eieio-defmethod 'cl-defmethod '"24.1")

(autoload 'eieio-defgeneric "eieio-compat" "\
Obsolete work part of an old version of the `defgeneric' macro.

\(fn METHOD DOC-STRING)" nil nil)

(make-obsolete 'eieio-defgeneric 'cl-defgeneric '"24.1")

(autoload 'eieio-defclass "eieio-compat" "\


\(fn CNAME SUPERCLASSES SLOTS OPTIONS)" nil nil)

(make-obsolete 'eieio-defclass 'eieio-defclass-internal '"25.1")

;;;***


(provide 'eieio-core)

;;; eieio-core.el ends here
