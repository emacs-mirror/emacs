;;; eieio-base.el --- Base classes for EIEIO.  -*- lexical-binding:t -*-

;; Copyright (C) 2000-2022 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: OO, lisp
;; Package: eieio

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
;; Base classes for EIEIO.  These classes perform some basic tasks
;; but are generally useless on their own.  To use any of these classes,
;; inherit from one or more of them.

;;; Code:

(require 'eieio)
(require 'seq)
(eval-when-compile (require 'cl-lib))

;;; eieio-instance-inheritor
;;
;; Enable instance inheritance via the `clone' method.
;; Works by using the `slot-unbound' method which usually throws an
;; error if a slot is unbound.
(defclass eieio-instance-inheritor ()
  ((parent-instance :initarg :parent-instance
		    :type eieio-instance-inheritor
		    :documentation
		    "The parent of this instance.
If a slot of this class is referenced, and is unbound, then the parent
is checked for a value.")
   )
  "This special class can enable instance inheritance.
Use `clone' to make a new object that does instance inheritance from
a parent instance.  When a slot in the child is referenced, and has
not been set, use values from the parent."
  :abstract t)

(cl-defmethod slot-unbound ((object eieio-instance-inheritor)
                         _class slot-name _fn)
  "If a slot OBJECT in this CLASS is unbound, try to inherit, or throw a signal.
SLOT-NAME is the offending slot.  FN is the function signaling the error."
  (if (slot-boundp object 'parent-instance)
      ;; It may not look like it, but this line recurses back into this
      ;; method if the parent instance's slot is unbound.
      (eieio-oref (oref object parent-instance) slot-name)
    ;; Throw the regular signal.
    (cl-call-next-method)))

(cl-defmethod clone ((obj eieio-instance-inheritor) &rest params)
  "Clone OBJ, initializing `:parent' to OBJ.
All slots are unbound, except those initialized with PARAMS."
  ;; call next method without params as we makeunbound slots anyhow
  (let ((nobj  (if (stringp (car params))
                   (cl-call-next-method obj (pop params))
                 (cl-call-next-method obj))))
    (dolist (descriptor (eieio-class-slots (eieio-object-class nobj)))
      (let ((slot (eieio-slot-descriptor-name descriptor)))
        (slot-makeunbound nobj slot)))
    (when params
      (shared-initialize nobj params))
    (oset nobj parent-instance obj)
    nobj))

(cl-defmethod eieio-instance-inheritor-slot-boundp ((object eieio-instance-inheritor)
						slot)
  "Return non-nil if the instance inheritor OBJECT's SLOT is bound.
See `slot-boundp' for details on binding slots.
The instance inheritor uses unbound slots as a way of cascading cloned
slot values, so testing for a slot being bound requires extra steps
for this kind of object."
  (if (slot-boundp object slot)
      ;; If it is regularly bound, return t.
      t
    (if (slot-boundp object 'parent-instance)
	(eieio-instance-inheritor-slot-boundp (oref object parent-instance)
					      slot)
      nil)))


;;; eieio-instance-tracker
;;
;; Track all created instances of this class.
;; The class must initialize the `tracking-symbol' slot, and that
;; symbol is then used to contain these objects.
(defclass eieio-instance-tracker ()
  ((tracking-symbol :type symbol
		    :allocation :class
		    :documentation
		    "The symbol used to maintain a list of our instances.
The instance list is treated as a variable, with new instances added to it.")
   )
  "This special class enables instance tracking.
Inheritors from this class must overload `tracking-symbol' which is
a variable symbol used to store a list of all instances."
  :abstract t)

(cl-defmethod initialize-instance :after ((this eieio-instance-tracker)
				       &rest _slots)
  "Make sure THIS is in our master list of this class.
Optional argument SLOTS are the initialization arguments."
  ;; Theoretically, this is never called twice for a given instance.
  (let ((sym (oref this tracking-symbol)))
    (if (not (memq this (symbol-value sym)))
	(set sym (append (symbol-value sym) (list this))))))

(cl-defmethod delete-instance ((this eieio-instance-tracker))
  "Remove THIS from the master list of this class."
  (set (oref this tracking-symbol)
       (delq this (symbol-value (oref this tracking-symbol)))))

;; In retrospect, this is a silly function.
(defun eieio-instance-tracker-find (key slot list-symbol)
  "Find KEY as an element of SLOT in the objects in LIST-SYMBOL.
Returns the first match."
  (object-assoc key slot (symbol-value list-symbol)))

;;; eieio-singleton
;;
;; The singleton Design Pattern specifies that there is but one object
;; of a given class ever created.  The EIEIO singleton base class defines
;; a CLASS allocated slot which contains the instance used.  All calls to
;; `make-instance' will either create a new instance and store it in this
;; slot, or it will just return what is there.
(defclass eieio-singleton ()
  ((singleton :type eieio-singleton
	      :allocation :class
	      :documentation
	      "The only instance of this class that will be instantiated.
Multiple calls to `make-instance' will return this object."))
  "This special class causes subclasses to be singletons.
A singleton is a class which will only ever have one instance."
  :abstract t)

(cl-defmethod make-instance ((class (subclass eieio-singleton)) &rest _slots)
  "Constructor for singleton CLASS.
NAME and SLOTS initialize the new object.
This constructor guarantees that no matter how many you request,
only one object ever exists."
  ;; NOTE TO SELF: In next version, make `slot-boundp' support classes
  ;; with class allocated slots or default values.
  (let ((old (oref-default class singleton)))
    (if (eq old eieio--unbound)
	(oset-default class singleton (cl-call-next-method))
      old)))


;;; Named object

(defclass eieio-named ()
  ((object-name :initarg :object-name :initform nil))
  "Object with a name."
  :abstract t)

(cl-defmethod eieio-object-name-string ((obj eieio-named))
  "Return a string which is OBJ's name."
  (or (slot-value obj 'object-name)
      (cl-call-next-method)))

(cl-defgeneric eieio-object-set-name-string (obj name)
  "Set the string which is OBJ's NAME."
  (declare (obsolete "inherit from `eieio-named' and use (setf (slot-value OBJ \\='object-name) NAME) instead" "25.1"))
  (cl-check-type name string)
  (setf (gethash obj eieio--object-names) name))
(define-obsolete-function-alias
  'object-set-name-string 'eieio-object-set-name-string "24.4")

(with-suppressed-warnings ((obsolete eieio-object-set-name-string))
  (cl-defmethod eieio-object-set-name-string ((obj eieio-named) name)
    "Set the string which is OBJ's NAME."
    (cl-check-type name string)
    (eieio-oset obj 'object-name name)))

(cl-defmethod clone ((obj eieio-named) &rest params)
  "Clone OBJ, initializing `:parent' to OBJ.
All slots are unbound, except those initialized with PARAMS."
  (let* ((newname (and (stringp (car params)) (pop params)))
         (nobj (apply #'cl-call-next-method obj params))
         (nm (slot-value nobj 'object-name)))
    (eieio-oset nobj 'object-name
                (or newname
                    (if (equal nm (slot-value obj 'object-name))
                        (save-match-data
                          (if (and nm (string-match "-\\([0-9]+\\)" nm))
                              (let ((num (1+ (string-to-number
                                              (match-string 1 nm)))))
                                (concat (substring nm 0 (match-beginning 0))
                                        "-" (int-to-string num)))
                            (concat nm "-1")))
                      nm)))
    nobj))

(cl-defmethod make-instance ((class (subclass eieio-named)) &rest args)
  (if (not (stringp (car args)))
      (cl-call-next-method)
    (funcall (if eieio-backward-compatibility #'ignore #'message)
             "Obsolete: name passed without :object-name to %S constructor"
             class)
    (apply #'cl-call-next-method class :object-name args)))

;;; eieio-persistent
;;
;; For objects which must save themselves to disk.  Provides an
;; `object-write' method to save an object to disk, and a
;; `eieio-persistent-read' function to call to read an object
;; from disk.
;;
;; Also provide the method `eieio-persistent-path-relative' to
;; calculate path names relative to a given instance.  This will
;; make the saved object location independent by converting all file
;; references to be relative to the directory the object is saved to.
;; You must call `eieio-persistent-path-relative' on each file name
;; saved in your object.
(defclass eieio-persistent ()
  ((file :initarg :file
	 :type string
	 :documentation
	 "The save file for this persistent object.
This must be a string, and must be specified when the new object is
instantiated.")
   (extension :type string
	      :allocation :class
	      :initform ".eieio"
	      :documentation
	      "Extension of files saved by this object.
Enables auto-choosing nice file names based on name.")
   (file-header-line :type string
		     :allocation :class
		     :initform ";; EIEIO PERSISTENT OBJECT"
		     :documentation
		     "Header line for the save file.
This is used with the `object-write' method.")
   (do-backups :type boolean
	       :allocation :class
	       :initform t
	       :documentation
	       "Saving this object should make backup files.
Setting to nil will mean no backups are made."))
  "This special class enables persistence through save files.
Use the `object-write' method to write this object to disk.  The save
format is Emacs Lisp code which calls the constructor for the saved
object.  For this reason, only slots which do not have an `:initarg'
specified will not be saved."
  :abstract t)

(cl-defmethod eieio-persistent-save-interactive ((this eieio-persistent) prompt
					      &optional name)
  "Prepare to save THIS.  Use in an `interactive' statement.
Query user for file name with PROMPT if THIS does not yet specify
a file.  Optional argument NAME specifies a default file name."
  (unless (slot-boundp this 'file)
      (oset this file
	    (read-file-name prompt nil
			    (if   name
				(concat name (oref this extension))
			      ))))
  (oref this file))

(defun eieio-persistent-read (filename &optional class allow-subclass)
  "Read a persistent object from FILENAME, and return it.
Signal an error if the object in FILENAME is not a constructor
for CLASS.  Optional ALLOW-SUBCLASS says that it is ok for
`eieio-persistent-read' to load in subclasses of class instead of
being pedantic."
  (unless class
    (warn "`eieio-persistent-read' called without specifying a class"))
  (when class (cl-check-type class class))
  (let ((ret nil)
	(buffstr nil))
    (unwind-protect
	(progn
	  (with-current-buffer (get-buffer-create " *tmp eieio read*")
	    (insert-file-contents filename nil nil nil t)
	    (goto-char (point-min))
	    (setq buffstr (buffer-string)))
	  ;; Do the read in the buffer the read was initialized from
	  ;; so that any initialize-instance calls that depend on
	  ;; the current buffer will work.
	  (setq ret (read buffstr))
	  (when (not (child-of-class-p (car ret) 'eieio-persistent))
	    (error
             "Invalid object: %s is not a subclass of `eieio-persistent'"
             (car ret)))
	  (when (and class
		     (not (or (eq (car ret) class) ; same class
			      (and allow-subclass  ; subclass
				   (child-of-class-p (car ret) class)))))
	    (error
             "Invalid object: %s is not an object of class %s nor a subclass"
             (car ret) class))
          (setq ret (eieio-persistent-make-instance (car ret) (cdr ret)))
	  (oset ret file filename))
      (kill-buffer " *tmp eieio read*"))
    ret))

(cl-defgeneric eieio-persistent-make-instance (objclass inputlist)
  "Convert INPUTLIST, representing slot values, to an instance of OBJCLASS.
Clean slot values, and possibly recursively create additional
objects found there."
  (:method
   ((objclass (subclass eieio-default-superclass)) inputlist)

   (let* ((name nil)
          (slots (if (stringp (car inputlist))
                     (progn
                       ;; Earlier versions of `object-write' added a
                       ;; string name for the object, now obsolete.
                       ;; Save as 'name' in case this object is subclass
                       ;; of eieio-named with no :object-name slot specified.
                       (setq name (car inputlist))
                       (cdr inputlist))
                   inputlist))
          (createslots nil))
     ;; If OBJCLASS is an eieio autoload object, then we need to
     ;; load it (we don't need the return value).
     (eieio--full-class-object objclass)
     (while slots
       (let ((initarg (car slots))
	     (value (car (cdr slots))))

	 ;; Strip out quotes, list functions, and update object
	 ;; constructors as needed.
	 (setq value (eieio-persistent-fix-value value))

	 (push initarg createslots)
	 (push value createslots))

       (setq slots (cdr (cdr slots))))

     (let ((newobj (apply #'make-instance objclass (nreverse createslots))))

       ;; Check for special case of subclass of `eieio-named', and do
       ;; name assignment.
       (when (and eieio-backward-compatibility
                  (object-of-class-p newobj 'eieio-named)
                  (not (oref newobj object-name))
                  name)
         (oset newobj object-name name))

       newobj))))

(defun eieio-persistent-fix-value (proposed-value)
  "Fix PROPOSED-VALUE.
Remove leading quotes from lists, and the symbol `list' from the
head of lists.  Explicitly construct any objects found, and strip
any text properties from string values.

This function will descend into the contents of lists, hash
tables, and vectors."
  (cond ((consp proposed-value)
	 ;; Lists with something in them need special treatment.
	 (cond ((eq (car proposed-value) 'quote)
                (while (eq (car-safe proposed-value) 'quote)
		  (setq proposed-value (car (cdr proposed-value))))
                proposed-value)

	       ;; An empty list sometimes shows up as (list), which is dumb, but
	       ;; we need to support it for backward compar.
	       ((and (eq (car proposed-value) 'list)
		     (= (length proposed-value) 1))
		nil)

	       ;; List of object constructors.
	       ((and (eq (car proposed-value) 'list)
		     ;; 2nd item is a list.
		     (consp (car (cdr proposed-value)))
		     ;; 1st elt of 2nd item is a class name.
		     (class-p (car (car (cdr proposed-value)))))

		;; We have a list of objects here.  Lets load them
		;; in.
		(let ((objlist nil))
		  (dolist (subobj (cdr proposed-value))
		    (push (eieio-persistent-make-instance
                           (car subobj) (cdr subobj))
			  objlist))
		  ;; return the list of objects ... reversed.
		  (nreverse objlist)))
	       ;; We have a slot with a single object that can be
	       ;; saved here.  Recurse and evaluate that
	       ;; sub-object.
	       ((class-p (car proposed-value))
		(eieio-persistent-make-instance
		 (car proposed-value) (cdr proposed-value)))
	       (t
		proposed-value)))
        ;; For hash-tables and vectors, the top-level `read' will not
        ;; "look inside" member values, so we need to do that
        ;; explicitly.  Because `eieio-override-prin1' is recursive in
        ;; the case of hash-tables and vectors, we recurse
        ;; `eieio-persistent-validate/fix-slot-value' here as well.
        ((hash-table-p proposed-value)
         (maphash
          (lambda (key value)
            (setf (gethash key proposed-value)
                  (if (class-p (car-safe value))
                      (eieio-persistent-make-instance
                       (car value) (cdr value))
                    (eieio-persistent-fix-value value))))
          proposed-value)
         proposed-value)

        ((vectorp proposed-value)
         (dotimes (i (length proposed-value))
           (let ((val (aref proposed-value i)))
             (aset proposed-value i
                   (if (class-p (car-safe val))
                       (eieio-persistent-make-instance
                        (car val) (cdr val))
                     (eieio-persistent-fix-value val)))))
         proposed-value)

	((stringp proposed-value)
	 ;; Else, check for strings, remove properties.
	 (substring-no-properties proposed-value))

	(t
	 ;; Else, just return whatever the constant was.
	 proposed-value)))

(cl-defmethod object-write ((this eieio-persistent) &optional comment)
  "Write persistent object THIS out to the current stream.
Optional argument COMMENT is a header line comment."
  (cl-call-next-method this (or comment (oref this file-header-line))))

(cl-defmethod eieio-persistent-path-relative ((this eieio-persistent) file)
  "For object THIS, make absolute file name FILE relative."
  (file-relative-name (expand-file-name file)
		      (file-name-directory (oref this file))))

(cl-defmethod eieio-persistent-save ((this eieio-persistent) &optional file)
  "Save persistent object THIS to disk.
Optional argument FILE overrides the file name specified in the object
instance."
  (when file (setq file (expand-file-name file)))
  (with-temp-buffer
    (let* ((cfn (or file (oref this file)))
           (default-directory (file-name-directory cfn)))
      (cl-letf ((standard-output (current-buffer))
                ((oref this file)       ;FIXME: Why change it?
                 (if file
                     ;; FIXME: Makes a name relative to (oref this file),
                     ;; whereas I think it should be relative to cfn.
                     (eieio-persistent-path-relative this file)
                   (file-name-nondirectory cfn))))
        (object-write this (oref this file-header-line)))
      (let ((backup-inhibited (not (oref this do-backups)))
            (coding-system-for-write 'utf-8-emacs))
        ;; Old way - write file.  Leaves message behind.
        ;;(write-file cfn nil)

        ;; New way - Avoid the vast quantities of error checking
        ;; just so I can get at the special flags that disable
        ;; displaying random messages.
        (write-region (point-min) (point-max) cfn nil 1)
        ))))

;; Notes on the persistent object:
;; It should also set up some hooks to help it keep itself up to date.



(provide 'eieio-base)

;;; eieio-base.el ends here
