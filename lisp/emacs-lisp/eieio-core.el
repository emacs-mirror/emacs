;;; eieio-core.el --- Core implementation for eieio  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1996, 1998-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The "core" part of EIEIO is the implementation for the object
;; system (such as eieio-defclass, or eieio-defmethod) but not the
;; base classes for the object system, which are defined in EIEIO.
;;
;; See the commentary for eieio.el for more about EIEIO itself.

;;; Code:

(require 'cl-lib)
(require 'eieio-loaddefs nil t)

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
  (eval-when-compile (cl-declaim (optimize (safety 0))))

(cl-defstruct (eieio--class
               (:constructor nil)
               (:constructor eieio--class-make (name))
               (:include cl--class)
               (:copier nil))
  children
  initarg-tuples                  ;; initarg tuples list
  (class-slots nil :type eieio--slot)
  class-allocation-values         ;; class allocated value vector
  default-object-cache ;; what a newly created object would look like.
                       ; This will speed up instantiation time as
                       ; only a `copy-sequence' will be needed, instead of
                       ; looping over all the values and setting them from
                       ; the default.
  options ;; storage location of tagged class option
          ; Stored outright without modifications or stripping
  )
  ;; Set it back to the default value.  NOTE: Using the default
  ;; `safety' value does NOT give the default
  ;; `byte-compile-delete-errors' value.  Therefore limit this (and
  ;; the above `cl-declaim') to compile time so that we don't affect
  ;; code which only loads this library.
  (eval-when-compile (cl-declaim (optimize (safety 1)))))


(eval-and-compile
  (defconst eieio--object-num-slots 1))

(defsubst eieio--object-class-tag (obj)
  (aref obj 0))

(defsubst eieio--object-class (obj)
  (eieio--object-class-tag obj))


;;; Important macros used internally in eieio.

(require 'cl-macs)  ;For cl--find-class.

(defsubst eieio--class-object (class)
  "Return the class object."
  (if (symbolp class)
      ;; Keep the symbol if class-v is nil, for better error messages.
      (or (cl--find-class class) class)
    class))

(defun class-p (x)
  "Return non-nil if X is a valid class vector.
X can also be is a symbol."
  (eieio--class-p (if (symbolp x) (cl--find-class x) x)))

(defun eieio--class-print-name (class)
  "Return a printed representation of CLASS."
  (format "#<class %s>" (eieio-class-name class)))

(defun eieio-class-name (class)
  "Return a Lisp like symbol name for CLASS."
  (setq class (eieio--class-object class))
  (cl-check-type class eieio--class)
  (eieio--class-name class))
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
  (and (recordp obj)
       (eieio--class-p (eieio--object-class-tag obj))))

(define-obsolete-function-alias 'object-p 'eieio-object-p "25.1")

(defun class-abstract-p (class)
  "Return non-nil if CLASS is abstract.
Abstract classes cannot be instantiated."
  (eieio--class-option (cl--find-class class) :abstract))

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
  (let* ((oldc (cl--find-class cname))
	 (newc (eieio--class-make cname)))
    (if (eieio--class-p oldc)
	nil ;; Do nothing if we already have this class.

      ;; turn this into a usable self-pointing symbol
      (when eieio-backward-compatibility
        (set cname cname)
        (make-obsolete-variable cname (format "use \\='%s instead" cname)
                                "25.1"))

      ;; Store the new class vector definition into the symbol.  We need to
      ;; do this first so that we can call defmethod for the accessor.
      ;; The vector will be updated by the following while loop and will not
      ;; need to be stored a second time.
      (setf (cl--find-class cname) newc)

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

(defvar eieio--known-slot-names nil)

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

  (let* ((oldc (let ((c (cl--find-class cname))) (if (eieio--class-p c) c)))
	 (newc (or oldc
                   ;; Reuse `oldc' instead of creating a new one, so that
                   ;; existing references stay valid.  E.g. when
                   ;; reloading the file that does the `defclass', we don't
                   ;; want to create a new class object.
                   (eieio--class-make cname)))
	 (groups nil) ;; list of groups id'd from slots
	 (clearparent nil))

    ;; If this class already existed, and we are updating its structure,
    ;; make sure we keep the old child list.  This can cause bugs, but
    ;; if no new slots are created, it also saves time, and prevents
    ;; method table breakage, particularly when the users is only
    ;; byte compiling an EIEIO file.
    (if oldc
        (progn
          (cl-assert (eq newc oldc))
          ;; Reset the fields.
          (setf (eieio--class-parents newc) nil)
          (setf (eieio--class-slots newc) nil)
          (setf (eieio--class-initarg-tuples newc) nil)
          (setf (eieio--class-class-slots newc) nil))
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
              (let ((c (cl--find-class p)))
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
                  (push c (eieio--class-parents newc))))))
	  ;; Reverse the list of our parents so that they are prioritized in
	  ;; the same order as specified in the code.
	  (cl-callf nreverse (eieio--class-parents newc)))
      ;; If there is nothing to loop over, then inherit from the
      ;; default superclass.
      (unless (eq cname 'eieio-default-superclass)
	;; adopt the default parent here, but clear it later...
	(setq clearparent t)
        ;; save new child in parent
        (cl-pushnew cname (eieio--class-children eieio-default-superclass))
        ;; save parent in child
        (setf (eieio--class-parents newc) (list eieio-default-superclass))))

    ;; turn this into a usable self-pointing symbol;  FIXME: Why?
    (when eieio-backward-compatibility
      (set cname cname)
      (make-obsolete-variable cname (format "use \\='%s instead" cname)
                              "25.1"))

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
        (make-obsolete csym (format
                             "use (cl-typep ... \\='(list-of %s)) instead"
                             cname)
                       "25.1")))

    ;; Before adding new slots, let's add all the methods and classes
    ;; in from the parent class.
    (eieio-copy-parents-into-subclass newc)

    ;; Store the new class vector definition into the symbol.  We need to
    ;; do this first so that we can call defmethod for the accessor.
    ;; The vector will be updated by the following while loop and will not
    ;; need to be stored a second time.
    (setf (cl--find-class cname) newc)

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
                    initarg (format "use \\='%s instead" initarg) "25.1"))))

	;; The customgroup should be a list of symbols.
	(cond ((and (null customg) custom)
	       (setq customg '(default)))
	      ((not (listp customg))
	       (setq customg (list customg))))
	;; The customgroup better be a list of symbols.
	(dolist (cg customg)
          (unless (symbolp cg)
            (signal 'invalid-slot-type (list :group cg))))

	;; First up, add this slot into our new class.
	(eieio--add-new-slot
         newc (cl--make-slot-descriptor
               name init type
               `(,@(if docstr `((:documentation . ,docstr)))
                 ,@(if custom  `((:custom . ,custom)))
                 ,@(if label   `((:label . ,label)))
                 ,@(if customg `((:group . ,customg)))
                 ,@(if printer `((:printer . ,printer)))
                 ,@(if prot    `((:protection . ,prot)))))
         initarg alloc 'defaultoverride skip-nil)

	;; We need to id the group, and store them in a group list attribute.
	(dolist (cg customg)
          (cl-pushnew cg groups :test #'equal))
	))

    ;; Now that everything has been loaded up, all our lists are backwards!
    ;; Fix that up now and then them into vectors.
    (cl-callf (lambda (slots) (apply #'vector (nreverse slots)))
        (eieio--class-slots newc))
    (cl-callf nreverse (eieio--class-initarg-tuples newc))

    ;; The storage for class-class-allocation-type needs to be turned into
    ;; a vector now.
    (cl-callf (lambda (slots) (apply #'vector slots))
        (eieio--class-class-slots newc))

    ;; Also, setup the class allocated values.
    (let* ((slots (eieio--class-class-slots newc))
           (n (length slots))
           (v (make-vector n nil)))
      (dotimes (i n)
        (setf (aref v i) (eieio-default-eval-maybe
                          (cl--slot-descriptor-initform (aref slots i)))))
      (setf (eieio--class-class-allocation-values newc) v))

    ;; Attach slot symbols into a hash table, and store the index of
    ;; this slot as the value this table.
    (let* ((slots (eieio--class-slots newc))
	   ;; (cslots (eieio--class-class-slots newc))
	   (oa (make-hash-table :test #'eq)))
      ;; (dotimes (cnt (length cslots))
      ;;   (setf (gethash (cl--slot-descriptor-name (aref cslots cnt)) oa) (- -1 cnt)))
      (dotimes (cnt (length slots))
        (setf (gethash (cl--slot-descriptor-name (aref slots cnt)) oa) cnt))
      (setf (eieio--class-index-table newc) oa))

    ;; Set up a specialized doc string.
    ;; Use stored value since it is calculated in a non-trivial way
    (let ((docstring (eieio--class-option-assoc options :documentation)))
      (setf (eieio--class-docstring newc) docstring)
      (when eieio-backward-compatibility
        (put cname 'variable-documentation docstring)))

    ;; Save the file location where this class is defined.
    (add-to-list 'current-load-list `(define-type . ,cname))

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
    (if clearparent (setf (eieio--class-parents newc) nil))

    ;; Create the cached default object.
    (let ((cache (make-record newc
                              (+ (length (eieio--class-slots newc))
                                 (eval-when-compile eieio--object-num-slots)
                                 -1)
                              nil)))
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

(defun eieio--perform-slot-validation-for-default (slot skipnil)
  "For SLOT, signal if its type does not match its default value.
If SKIPNIL is non-nil, then if default value is nil return t instead."
  (let ((value (cl--slot-descriptor-initform slot))
        (spec (cl--slot-descriptor-type slot)))
    (if (not (or (eieio-eval-default-p value) ;FIXME: Why?
                 eieio-skip-typecheck
                 (and skipnil (null value))
                 (eieio--perform-slot-validation spec value)))
        (signal 'invalid-slot-type (list (cl--slot-descriptor-name slot) spec value)))))

(defun eieio--slot-override (old new skipnil)
  (cl-assert (eq (cl--slot-descriptor-name old) (cl--slot-descriptor-name new)))
  ;; There is a match, and we must override the old value.
  (let* ((a (cl--slot-descriptor-name old))
         (tp (cl--slot-descriptor-type old))
         (d (cl--slot-descriptor-initform new))
         (type (cl--slot-descriptor-type new))
         (oprops (cl--slot-descriptor-props old))
         (nprops (cl--slot-descriptor-props new))
         (custg (alist-get :group nprops)))
    ;; If type is passed in, is it the same?
    (if (not (eq type t))
        (if (not (equal type tp))
            (error
             "Child slot type `%s' does not match inherited type `%s' for `%s'"
             type tp a))
      (setf (cl--slot-descriptor-type new) tp))
    ;; If we have a repeat, only update the initarg...
    (unless (eq d eieio-unbound)
      (eieio--perform-slot-validation-for-default new skipnil)
      (setf (cl--slot-descriptor-initform old) d))

    ;; PLN Tue Jun 26 11:57:06 2007 : The protection is
    ;; checked and SHOULD match the superclass
    ;; protection. Otherwise an error is thrown. However
    ;; I wonder if a more flexible schedule might be
    ;; implemented.
    ;;
    ;; EML - We used to have (if prot... here,
    ;;       but a prot of 'nil means public.
    ;;
    (let ((super-prot (alist-get :protection oprops))
          (prot (alist-get :protection nprops)))
      (if (not (eq prot super-prot))
          (error "Child slot protection `%s' does not match inherited protection `%s' for `%s'"
                 prot super-prot a)))
    ;; End original PLN

    ;; PLN Tue Jun 26 11:57:06 2007 :
    ;; Do a non redundant combination of ancient custom
    ;; groups and new ones.
    (when custg
      (let* ((list1 (alist-get :group oprops)))
        (dolist (elt custg)
          (unless (memq elt list1)
            (push elt list1)))
        (setf (alist-get :group (cl--slot-descriptor-props old)) list1)))
    ;;  End PLN

    ;;  PLN Mon Jun 25 22:44:34 2007 : If a new cust is
    ;;  set, simply replaces the old one.
    (dolist (prop '(:custom :label :documentation :printer))
      (when (alist-get prop (cl--slot-descriptor-props new))
        (setf (alist-get prop (cl--slot-descriptor-props old))
              (alist-get prop (cl--slot-descriptor-props new))))

      )  ))

(defun eieio--add-new-slot (newc slot init alloc
				 &optional defaultoverride skipnil)
  "Add into NEWC attribute SLOT.
If a slot of that name already exists in NEWC, then do nothing.  If it doesn't exist,
INIT is the initarg, if any.
Argument ALLOC specifies if the slot is allocated per instance, or per class.
If optional DEFAULTOVERRIDE is non-nil, then if A exists in NEWC,
we must override its value for a default.
Optional argument SKIPNIL indicates if type checking should be skipped
if default value is nil."
  ;; Make sure we duplicate those items that are sequences.
  (let* ((a (cl--slot-descriptor-name slot))
         (d (cl--slot-descriptor-initform slot))
         (old (car (cl-member a (eieio--class-slots newc)
                              :key #'cl--slot-descriptor-name)))
         (cold (car (cl-member a (eieio--class-class-slots newc)
                               :key #'cl--slot-descriptor-name))))
    (cl-pushnew a eieio--known-slot-names)
    (condition-case nil
        (if (sequencep d) (setq d (copy-sequence d)))
      ;; This copy can fail on a cons cell with a non-cons in the cdr.  Let's
      ;; skip it if it doesn't work.
      (error nil))
    ;; (if (sequencep type) (setq type (copy-sequence type)))
    ;; (if (sequencep cust) (setq cust (copy-sequence cust)))
    ;; (if (sequencep custg) (setq custg (copy-sequence custg)))

    ;; To prevent override information w/out specification of storage,
    ;; we need to do this little hack.
    (if cold (setq alloc :class))

    (if (memq alloc '(nil :instance))
        ;; In this case, we modify the INSTANCE version of a given slot.
        (progn
          ;; Only add this element if it is so-far unique
          (if (not old)
              (progn
                (eieio--perform-slot-validation-for-default slot skipnil)
                (push slot (eieio--class-slots newc))
                )
            ;; When defaultoverride is true, we are usually adding new local
            ;; attributes which must override the default value of any slot
            ;; passed in by one of the parent classes.
            (when defaultoverride
              (eieio--slot-override old slot skipnil)))
          (when init
            (cl-pushnew (cons init a) (eieio--class-initarg-tuples newc)
                        :test #'equal)))

      ;; CLASS ALLOCATED SLOTS
      (if (not cold)
          (progn
            (eieio--perform-slot-validation-for-default slot skipnil)
            ;; Here we have found a :class version of a slot.  This
            ;; requires a very different approach.
            (push slot (eieio--class-class-slots newc)))
        (when defaultoverride
          ;; There is a match, and we must override the old value.
          (eieio--slot-override cold slot skipnil))))))

(defun eieio-copy-parents-into-subclass (newc)
  "Copy into NEWC the slots of PARENTS.
Follow the rules of not overwriting early parents when applying to
the new child class."
  (let ((sn (eieio--class-option-assoc (eieio--class-options newc)
                                       :allow-nil-initform)))
    (dolist (pcv (eieio--class-parents newc))
      ;; First, duplicate all the slots of the parent.
      (let ((pslots (eieio--class-slots pcv))
            (pinit (eieio--class-initarg-tuples pcv)))
        (dotimes (i (length pslots))
	  (let* ((sd (cl--copy-slot-descriptor (aref pslots i)))
                 (init (car (rassq (cl--slot-descriptor-name sd) pinit))))
	    (eieio--add-new-slot newc sd init nil nil sn))
          )) ;; while/let
      ;; Now duplicate all the class alloc slots.
      (let ((pcslots (eieio--class-class-slots pcv)))
        (dotimes (i (length pcslots))
          (eieio--add-new-slot newc (cl--copy-slot-descriptor
                                     (aref pcslots i))
                               nil :class sn)
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
    (let ((st (cl--slot-descriptor-type (aref (eieio--class-slots class)
                                              slot-idx))))
      (if (not (eieio--perform-slot-validation st value))
	  (signal 'invalid-slot-type
                  (list (eieio--class-name class) slot st value))))))

(defun eieio--validate-class-slot-value (class slot-idx value slot)
  "Make sure that for CLASS referencing SLOT-IDX, VALUE is valid.
Checks the :type specifier.
SLOT is the slot that is being checked, and is only used when throwing
an error."
  (if eieio-skip-typecheck
      nil
    (let ((st (cl--slot-descriptor-type (aref (eieio--class-class-slots class)
                                              slot-idx))))
      (if (not (eieio--perform-slot-validation st value))
	  (signal 'invalid-slot-type
                  (list (eieio--class-name class) slot st value))))))

(defun eieio-barf-if-slot-unbound (value instance slotname fn)
  "Throw a signal if VALUE is a representation of an UNBOUND slot.
INSTANCE is the object being referenced.  SLOTNAME is the offending
slot.  If the slot is ok, return VALUE.
Argument FN is the function calling this verifier."
  (if (and (eq value eieio-unbound) (not eieio-skip-typecheck))
      (slot-unbound instance (eieio--object-class instance) slotname fn)
    value))


;;; Get/Set slots in an object.

(defun eieio-oref (obj slot)
  "Return the value in OBJ at SLOT in the object vector."
  (declare (compiler-macro
            (lambda (exp)
              (ignore obj)
              (pcase slot
                ((and (or `',name (and name (pred keywordp)))
                      (guard (not (memq name eieio--known-slot-names))))
                 (macroexp--warn-and-return
                  (format-message "Unknown slot `%S'" name) exp 'compile-only))
                (_ exp)))))
  (cl-check-type slot symbol)
  (cl-check-type obj (or eieio-object class))
  (let* ((class (cond ((symbolp obj)
                       (error "eieio-oref called on a class: %s" obj)
                       (let ((c (cl--find-class obj)))
                         (if (eieio--class-p c) (eieio-class-un-autoload obj))
                         c))
                      (t (eieio--object-class obj))))
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
	  (slot-missing obj slot 'oref))
      (cl-check-type obj eieio-object)
      (eieio-barf-if-slot-unbound (aref obj c) obj slot 'oref))))


(defun eieio-oref-default (obj slot)
  "Do the work for the macro `oref-default' with similar parameters.
Fills in OBJ's SLOT with its default value."
  (cl-check-type obj (or eieio-object class))
  (cl-check-type slot symbol)
  (let* ((cl (cond ((symbolp obj) (cl--find-class obj))
                   ((eieio-object-p obj) (eieio--object-class obj))
                   (t obj)))
	 (c (eieio--slot-name-index cl slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c
		  (eieio--class-slot-name-index cl slot))
	    ;; Oref that slot.
	    (aref (eieio--class-class-allocation-values cl)
		  c)
	  (slot-missing obj slot 'oref-default))
      (eieio-barf-if-slot-unbound
       (let ((val (cl--slot-descriptor-initform
                   (aref (eieio--class-slots cl)
                         (- c (eval-when-compile eieio--object-num-slots))))))
	 (eieio-default-eval-maybe val))
       obj (eieio--class-name cl) 'oref-default))))

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
  (let* ((class (eieio--object-class obj))
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
	  (slot-missing obj slot 'oset value))
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
          (signal 'invalid-slot-name (list (eieio--class-name class) slot)))
      ;; `oset-default' on an instance-allocated slot is allowed by EIEIO but
      ;; not by CLOS and is mildly inconsistent with the :initform thingy, so
      ;; it'd be nice to get of it.  This said, it is/was used at one place by
      ;; gnus/registry.el, so it might be used elsewhere as well, so let's
      ;; keep it for now.
      ;; FIXME: Generate a compile-time warning for it!
      ;; (error "Can't `oset-default' an instance-allocated slot: %S of %S"
      ;;        slot class)
      (eieio--validate-slot-value class c value slot)
      ;; Set this into the storage for defaults.
      (if (eieio-eval-default-p value)
          (error "Can't set default to a sexp that gets evaluated again"))
      (setf (cl--slot-descriptor-initform
             ;; FIXME: Apparently we set it both in `slots' and in
             ;; `object-cache', which seems redundant.
             (aref (eieio--class-slots class)
                   (- c (eval-when-compile eieio--object-num-slots))))
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
  (let* ((fsi (gethash slot (eieio--class-index-table class))))
    (if (integerp fsi)
        (+ (eval-when-compile eieio--object-num-slots) fsi)
      (let ((fn (eieio--initarg-to-attribute class slot)))
	(if fn
            ;; Accessing a slot via its :initarg is accepted by EIEIO
            ;; (but not CLOS) but is a bad idea (for one: it's slower).
            ;; FIXME: We should emit a compile-time warning when this happens!
            (eieio--slot-name-index class fn)
          nil)))))

(defun eieio--class-slot-name-index (class slot)
  "In CLASS find the index of the named SLOT.
The slot is a symbol which is installed in CLASS by the `defclass'
call.  If SLOT is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; This will happen less often, and with fewer slots.  Do this the
  ;; storage cheap way.
  (let ((index nil)
        (slots (eieio--class-class-slots class)))
    (dotimes (i (length slots))
      (if (eq slot (cl--slot-descriptor-name (aref slots i)))
          (setq index i)))
    index))

;;;
;; Way to assign slots based on a list.  Used for constructors, or
;; even resetting an object at run-time
;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all slots to their defaults.
If SET-ALL is non-nil, then when a default is nil, that value is
reset.  If SET-ALL is nil, the slots are only reset if the default is
not nil."
  (let ((slots (eieio--class-slots (eieio--object-class obj))))
    (dotimes (i (length slots))
      (let* ((name (cl--slot-descriptor-name (aref slots i)))
             (df (eieio-oref-default obj name)))
        (if (or df set-all)
            (eieio-oset obj name df))))))

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
  (setq remaining-inputs (delq nil remaining-inputs))
  (if (null remaining-inputs)
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
		     (setq found (eieio--c3-candidate (caar tail)
                                                      remaining-inputs)
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

(defsubst eieio--class/struct-parents (class)
  (or (eieio--class-parents class)
      `(,eieio-default-superclass)))

(defun eieio--class-precedence-c3 (class)
  "Return all parents of CLASS in c3 order."
  (let ((parents (eieio--class-parents class)))
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
  (let* ((parents (eieio--class-parents class))
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
         (queue (eieio--class/struct-parents class)))
    (while queue
      (let ((head (pop queue)))
	(unless (member head result)
	  (push head result)
	  (unless (eq head eieio-default-superclass)
	    (setq queue (append queue (eieio--class/struct-parents head)))))))
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
      (eieio-class-un-autoload (eieio--class-name class)))
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

(cl-generic-define-generalizer eieio--generic-generalizer
  ;; Use the exact same tagcode as for cl-struct, so that methods
  ;; that dispatch on both kinds of objects get to share this
  ;; part of the dispatch code.
  50 #'cl--generic-struct-tag
  (lambda (tag &rest _)
    (let ((class (cl--find-class tag)))
      (and (eieio--class-p class)
           (mapcar #'eieio--class-name
                   (eieio--class-precedence-list class))))))

(cl-defmethod cl-generic-generalizers :extra "class" (specializer)
  "Support for dispatch on types defined by EIEIO's `defclass'."
  ;; CLHS says:
  ;;    A class must be defined before it can be used as a parameter
  ;;    specializer in a defmethod form.
  ;; So we can ignore types that are not known to denote classes.
  (or
   (and (eieio--class-p (eieio--class-object specializer))
        (list eieio--generic-generalizer))
   (cl-call-next-method)))

;;;; Dispatch for arguments which are classes.

;; Since EIEIO does not support metaclasses, users can't easily use the
;; "dispatch on argument type" for class arguments.  That's why EIEIO's
;; `defmethod' added the :static qualifier.  For cl-generic, such a qualifier
;; would not make much sense (e.g. to which argument should it apply?).
;; Instead, we add a new "subclass" specializer.

(defun eieio--generic-subclass-specializers (tag &rest _)
  (when (eieio--class-p tag)
    (mapcar (lambda (class)
              `(subclass ,(eieio--class-name class)))
            (eieio--class-precedence-list tag))))

(cl-generic-define-generalizer eieio--generic-subclass-generalizer
  60 (lambda (name &rest _) `(and (symbolp ,name) (cl--find-class ,name)))
  #'eieio--generic-subclass-specializers)

(cl-defmethod cl-generic-generalizers ((_specializer (head subclass)))
  "Support for (subclass CLASS) specializers.
These match if the argument is the name of a subclass of CLASS."
  (list eieio--generic-subclass-generalizer))

(provide 'eieio-core)

;;; eieio-core.el ends here
