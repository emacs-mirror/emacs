;;; custom.el --- User friendly customization support.

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: help
;; Version: 0.5

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; WARNING: This package is still under construction and not all of
;; the features below are implemented.
;;
;; This package provides a framework for adding user friendly
;; customization support to Emacs.  Having to do customization by
;; editing a text file in some arcane syntax is user hostile in the
;; extreme, and to most users emacs lisp definitely count as arcane.
;;
;; The intent is that authors of emacs lisp packages declare the
;; variables intended for user customization with `custom-declare'.
;; Custom can then automatically generate a customization buffer with
;; `custom-buffer-create' where the user can edit the package
;; variables in a simple and intuitive way, as well as a menu with
;; `custom-menu-create' where he can set the more commonly used
;; variables interactively.
;;
;; It is also possible to use custom for modifying the properties of
;; other objects than the package itself, by specifying extra optional
;; arguments to `custom-buffer-create'.
;;
;; Custom is inspired by OPEN LOOK property windows.

;;; Todo:  
;;
;; - Toggle documentation in three states `none', `one-line', `full'.
;; - Function to generate an XEmacs menu from a CUSTOM.
;; - Write TeXinfo documentation.
;; - Make it possible to hide sections by clicking at the level.
;; - Declare AUC TeX variables.
;; - Declare (ding) Gnus variables.
;; - Declare Emacs variables.
;; - Implement remaining types.
;; - XEmacs port.
;; - Allow `URL', `info', and internal hypertext buttons.
;; - Support meta-variables and goal directed customization.
;; - Make it easy to declare custom types independently.
;; - Make it possible to declare default value and type for a single
;;   variable, storing the data in a symbol property.
;; - Syntactic sugar for CUSTOM declarations.
;; - Use W3 for variable documentation.

;;; Code:

(eval-when-compile
  (require 'cl))

;;; Compatibility:

(defun custom-xmas-add-text-properties (start end props &optional object)
  (add-text-properties start end props object)
  (put-text-property start end 'start-open t object)
  (put-text-property start end 'end-open t object))

(defun custom-xmas-put-text-property (start end prop value &optional object)
  (put-text-property start end prop value object)
  (put-text-property start end 'start-open t object)
  (put-text-property start end 'end-open t object))

(defun custom-xmas-extent-start-open ()
  (map-extents (lambda (extent arg)
		 (set-extent-property extent 'start-open t))
	       nil (point) (min (1+ (point)) (point-max))))
		  
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (progn
      (fset 'custom-add-text-properties 'custom-xmas-add-text-properties)
      (fset 'custom-put-text-property 'custom-xmas-put-text-property)
      (fset 'custom-extent-start-open 'custom-xmas-extent-start-open)
      (fset 'custom-set-text-properties
	    (if (fboundp 'set-text-properties)
		'set-text-properties))
      (fset 'custom-buffer-substring-no-properties
	    (if (fboundp 'buffer-substring-no-properties)
		'buffer-substring-no-properties
	      'custom-xmas-buffer-substring-no-properties)))
  (fset 'custom-add-text-properties 'add-text-properties)
  (fset 'custom-put-text-property 'put-text-property)
  (fset 'custom-extent-start-open 'ignore)
  (fset 'custom-set-text-properties 'set-text-properties)
  (fset 'custom-buffer-substring-no-properties 
	'buffer-substring-no-properties))

(defun custom-xmas-buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without text properties, as a string."
  (let ((string (buffer-substring beg end)))
    (custom-set-text-properties 0 (length string) nil string)
    string))

(or (fboundp 'add-to-list)
    ;; Introduced in Emacs 19.29.
    (defun add-to-list (list-var element)
      "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
      (or (member element (symbol-value list-var))
	  (set list-var (cons element (symbol-value list-var))))))

(or (fboundp 'plist-get)
    ;; Introduced in Emacs 19.29.
    (defun plist-get (plist prop)
      "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
      (let (result)
	(while plist
	  (if (eq (car plist) prop)
	      (setq result (car (cdr plist))
		    plist nil)
	    (set plist (cdr (cdr plist)))))
	result)))

(or (fboundp 'plist-put)
    ;; Introduced in Emacs 19.29.
    (defun plist-put (plist prop val)    
      "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects."
      (if (null plist)
	  (list prop val)
	(let ((current plist))
	  (while current
	    (cond ((eq (car current) prop)
		   (setcar (cdr current) val)
		   (setq current nil))
		  ((null (cdr (cdr current)))
		   (setcdr (cdr current) (list prop val))
		   (setq current nil))
		  (t
		   (setq current (cdr (cdr current)))))))
	plist)))

(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num))))))

(or (fboundp 'facep)
    ;; Introduced in Emacs 19.29.
    (defun facep (x)
      "Return t if X is a face name or an internal face vector."
      (and (or (and (fboundp 'internal-facep) (internal-facep x))
	       (and 
		(symbolp x) 
		(assq x (and (boundp 'global-face-data) global-face-data))))
	   t)))

;; XEmacs and Emacs 19.29 facep does different things.
(if (fboundp 'find-face)
    (fset 'custom-facep 'find-face)
  (fset 'custom-facep 'facep))

(if (custom-facep 'underline)
    ()
  ;; No underline face in XEmacs 19.12.
  (and (fboundp 'make-face)
       (funcall (intern "make-face") 'underline))
  ;; Must avoid calling set-face-underline-p directly, because it
  ;; is a defsubst in emacs19, and will make the .elc files non
  ;; portable!
  (or (and (fboundp 'face-differs-from-default-p)
	   (face-differs-from-default-p 'underline))
      (and (fboundp 'set-face-underline-p)
	   (funcall 'set-face-underline-p 'underline t))))

(defun custom-xmas-set-text-properties (start end props &optional buffer)
  (if (null buffer)
      (if props
	  (while props
	    (custom-put-text-property 
	     start end (car props) (nth 1 props) buffer)
	    (setq props (nthcdr 2 props)))
	(remove-text-properties start end ()))))

(or (fboundp 'event-point)
    ;; Missing in Emacs 19.29.
    (defun event-point (event)
      "Return the character position of the given mouse-motion, button-press,
or button-release event.  If the event did not occur over a window, or did
not occur over text, then this returns nil.  Otherwise, it returns an index
into the buffer visible in the event's window."
      (posn-point (event-start event))))

(eval-when-compile
  (defvar x-colors nil)
  (defvar custom-button-face nil)
  (defvar custom-field-uninitialized-face nil)
  (defvar custom-field-invalid-face nil)
  (defvar custom-field-modified-face nil)
  (defvar custom-field-face nil)
  (defvar custom-mouse-face nil)
  (defvar custom-field-active-face nil))

;; We can't easily check for a working intangible.
(defconst intangible (if (and (boundp 'emacs-minor-version)
			      (or (> emacs-major-version 19)
				  (and (> emacs-major-version 18)
				       (> emacs-minor-version 28))))
			 (setq intangible 'intangible)
		       (setq intangible 'intangible-if-it-had-been-working))
  "The symbol making text intangible.")

(defconst rear-nonsticky (if (string-match "XEmacs" emacs-version)
			     'end-open
			   'rear-nonsticky)
  "The symbol making text properties non-sticky in the rear end.")

(defconst front-sticky (if (string-match "XEmacs" emacs-version)
			   'front-closed
			 'front-sticky)
  "The symbol making text properties sticky in the front.")

(defconst mouse-face (if (string-match "XEmacs" emacs-version)
			 'highlight
		       'mouse-face)
  "Symbol used for highlighting text under mouse.")

;; Put it in the Help menu, if possible.
(if (string-match "XEmacs" emacs-version)
    (if (featurep 'menubar)
	;; XEmacs (disabled because it doesn't work)
	(and current-menubar
	     (add-menu-item '("Help") "Customize..." 'customize t)))
  ;; Emacs 19.28 and earlier
  (global-set-key [ menu-bar help customize ]
		  '("Customize..." . customize))
  ;; Emacs 19.29 and later
  (global-set-key [ menu-bar help-menu customize ] 
		  '("Customize..." . customize)))

;; XEmacs popup-menu stolen from w3.el.
(defun custom-x-really-popup-menu (pos title menudesc)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event menu)
    (while menudesc
      (setq menu (cons (vector (car (car menudesc))
			       (list (car (car menudesc))) t) menu)
	    menudesc (cdr menudesc)))
    (setq menu (cons title menu))
    (popup-menu menu)
    (catch 'popup-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (misc-user-event-p event) (stringp (car-safe						   (event-object event))))
	       (throw 'popup-done (event-object event)))
	      ((and (misc-user-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       nil)
	      ((not (popup-menu-up-p))
	       (throw 'popup-done nil))
	      ((button-release-event-p event);; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please make a choice from the menu.")))))))

;;; Categories:
;;
;; XEmacs use inheritable extents for the same purpose as Emacs uses
;; the category text property.

(if (string-match "XEmacs" emacs-version)
    (progn 
      ;; XEmacs categories.
      (defun custom-category-create (name)
	(set name (make-extent nil nil))
	"Create a text property category named NAME.")

      (defun custom-category-put (name property value)
	"In CATEGORY set PROPERTY to VALUE."
	(set-extent-property (symbol-value name) property value))
      
      (defun custom-category-get (name property)
	"In CATEGORY get PROPERTY."
	(extent-property (symbol-value name) property))
      
      (defun custom-category-set (from to category)
	"Make text between FROM and TWO have category CATEGORY."
	(let ((extent (make-extent from to)))
	  (set-extent-parent extent (symbol-value category)))))
      
  ;; Emacs categories.
  (defun custom-category-create (name)
    "Create a text property category named NAME."
    (set name name))

  (defun custom-category-put (name property value)
    "In CATEGORY set PROPERTY to VALUE."
    (put name property value))

  (defun custom-category-get (name property)
    "In CATEGORY get PROPERTY."
    (get name property))

  (defun custom-category-set (from to category)
    "Make text between FROM and TWO have category CATEGORY."
    (custom-put-text-property from to 'category category)))

;;; External Data:
;; 
;; The following functions and variables defines the interface for
;; connecting a CUSTOM with an external entity, by default an emacs
;; lisp variable.

(defvar custom-external 'default-value
  "Function returning the external value of NAME.")

(defvar custom-external-set 'set-default
  "Function setting the external value of NAME to VALUE.")

(defun custom-external (name)
  "Get the external value associated with NAME."
  (funcall custom-external name))

(defun custom-external-set (name value)
  "Set the external value associated with NAME to VALUE."
  (funcall custom-external-set name value))

(defvar custom-name-fields nil
  "Alist of custom names and their associated editing field.")
(make-variable-buffer-local 'custom-name-fields)

(defun custom-name-enter (name field)
  "Associate NAME with FIELD."
  (if (null name)
      ()
    (custom-assert 'field)
    (setq custom-name-fields (cons (cons name field) custom-name-fields))))

(defun custom-name-field (name)
  "The editing field associated with NAME."
  (cdr (assq name custom-name-fields)))

(defun custom-name-value (name)
  "The value currently displayed for NAME in the customization buffer."
  (let* ((field (custom-name-field name))
	 (custom (custom-field-custom field)))
    (custom-field-parse field)
    (funcall (custom-property custom 'export) custom
	     (car (custom-field-extract custom field)))))

(defvar custom-save 'custom-save
  "Function that will save current customization buffer.")

;;; Custom Functions:
;;
;; The following functions are part of the public interface to the
;; CUSTOM datastructure.  Each CUSTOM describes a group of variables,
;; a single variable, or a component of a structured variable.  The
;; CUSTOM instances are part of two hierarchies, the first is the
;; `part-of' hierarchy in which each CUSTOM is a component of another
;; CUSTOM, except for the top level CUSTOM which is contained in
;; `custom-data'.  The second hierarchy is a `is-a' type hierarchy
;; where each CUSTOM is a leaf in the hierarchy defined by the `type'
;; property and `custom-type-properties'.

(defvar custom-file "~/.custom.el"
  "Name of file with customization information.")

(defconst custom-data
  '((tag . "Emacs")
    (doc . "The extensible self-documenting text editor.")
    (type . group)
    (data "\n"
	  ((header . nil)
	   (compact . t)
	   (type . group)
	   (doc . "\
Press [Save] to save any changes permanently after you are done editing.  
You can load customization information from other files by editing the
`File' field and pressing the [Load] button.  When you press [Save] the
customization information of all files you have loaded, plus any
changes you might have made manually, will be stored in the file 
specified by the `File' field.")
	   (data ((tag . "Load")
		  (type . button)
		  (query . custom-load))
		 ((tag . "Save")
		  (type . button)
		  (query . custom-save))
		 ((name . custom-file)
		  (default . "~/.custom.el")
		  (doc . "Name of file with customization information.\n")
		  (tag . "File")
		  (type . file))))))
  "The global customization information.  
A custom association list.")

(defun custom-declare (path custom)
  "Declare variables for customization.  
PATH is a list of tags leading to the place in the customization
hierarchy the new entry should be added.  CUSTOM is the entry to add."
  (custom-initialize custom)
  (let ((current (custom-travel-path custom-data path)))
    (or (member custom (custom-data current))
	(nconc (custom-data current) (list custom)))))

(put 'custom-declare 'lisp-indent-hook 1)

(defconst custom-type-properties
  '((repeat (type . default)
	    ;; See `custom-match'.
	    (import . custom-repeat-import)
	    (eval . custom-repeat-eval)
	    (quote . custom-repeat-quote)
	    (accept . custom-repeat-accept)
	    (extract . custom-repeat-extract)
	    (validate . custom-repeat-validate)
	    (insert . custom-repeat-insert)
	    (match . custom-repeat-match)
	    (query . custom-repeat-query)
	    (prefix . "")
	    (del-tag . "[DEL]")
	    (add-tag . "[INS]"))
    (pair (type . group)
	  ;; A cons-cell.
	  (accept . custom-pair-accept)
	  (eval . custom-pair-eval)
	  (import . custom-pair-import)
	  (quote . custom-pair-quote)
	  (valid . (lambda (c d) (consp d)))
	  (extract . custom-pair-extract))
    (list (type . group)
	  ;; A lisp list.
	  (quote . custom-list-quote)
	  (valid . (lambda (c d)
		     (listp d)))
	  (extract . custom-list-extract))
    (group (type . default)
	   ;; See `custom-match'.
	   (face-tag . nil)
	   (eval . custom-group-eval)
	   (import . custom-group-import)
	   (initialize . custom-group-initialize)
	   (apply . custom-group-apply)
	   (reset . custom-group-reset)
	   (factory-reset . custom-group-factory-reset)
	   (extract . nil)
	   (validate . custom-group-validate)
	   (query . custom-toggle-hide)
	   (accept . custom-group-accept)
	   (insert . custom-group-insert)
	   (find . custom-group-find))
    (toggle (type . choice)
	    ;; Booleans.
	    (data ((type . const)
		   (tag . "On ")
		   (default . t))
		  ((type . const)
		   (tag . "Off")
		   (default . nil))))
    (triggle (type . choice)
	     ;; On/Off/Default.
	     (data ((type . const)
		    (tag . "On ")
		    (default . t))
		   ((type . const)
		    (tag . "Off")
		    (default . nil))
		   ((type . const)
		    (tag . "Def")
		    (default . custom:asis))))
    (choice (type . default)
	    ;; See `custom-match'.
	    (query . custom-choice-query)
	    (accept . custom-choice-accept)
	    (extract . custom-choice-extract)
	    (validate . custom-choice-validate)
	    (insert . custom-choice-insert)
	    (none (tag . "Unknown")
		  (default . __uninitialized__)
		  (type . const)))
    (const (type . default)
	   ;; A `const' only matches a single lisp value.
	   (extract . (lambda (c f) (list (custom-default c))))
	   (validate . (lambda (c f) nil))
	   (valid . custom-const-valid)
	   (update . custom-const-update)
	   (insert . custom-const-insert))
    (face-doc (type . doc)
	      ;; A variable containing a face.
	      (doc . "\
You can customize the look of Emacs by deciding which faces should be
used when.  If you push one of the face buttons below, you will be
given a choice between a number of standard faces.  The name of the
selected face is shown right after the face button, and it is
displayed its own face so you can see how it looks.  If you know of
another standard face not listed and want to use it, you can select
`Other' and write the name in the editing field.

If none of the standard faces suits you, you can select `Customize' to
create your own face.  This will make six fields appear under the face
button.  The `Fg' and `Bg' fields are the foreground and background
colors for the face, respectively.  You should type the name of the
color in the field.  You can use any X11 color name.  A list of X11
color names may be available in the file `/usr/lib/X11/rgb.txt' on
your system.  The special color name `default' means that the face
will not change the color of the text.  The `Stipple' field is weird,
so just ignore it.  The three remaining fields are toggles, which will
make the text `bold', `italic', or `underline' respectively.  For some
fonts `bold' or `italic' will not make any visible change."))
    (face (type . choice)
	  (eval . custom-face-eval)
	  (import . custom-face-import)
	  (data ((tag . "None")
		 (default . nil)
		 (type . const))
		((tag . "Default")
		 (default . default)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Bold")
		 (default . bold)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Bold-italic")
		 (default . bold-italic)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Italic")
		 (default . italic)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Underline")
		 (default . underline)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Highlight")
		 (default . highlight)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Modeline")
		 (default . modeline)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Region")
		 (default . region)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Secondary Selection")
		 (default . secondary-selection)
		 (face . custom-const-face)
		 (type . const))
		((tag . "Customized")
		 (compact . t)
		 (face-tag . custom-face-hack)
		 (eval . custom-face-eval)
		 (data ((hidden . t)
			(tag . "")
			(doc . "\
Select the properties you want this face to have.")
			(default . custom-face-lookup)
			(type . const))
		       "\n"
		       ((tag . "Fg")
			(hidden . t)
			(default . "default")
			(width . 20)
			(type . string))
		       ((tag . "Bg")
			(default . "default")
			(width . 20)
			(type . string))
		       ((tag . "Stipple")
			(default . "default")
			(width . 20)
			(type . string))
		       "\n"
		       ((tag . "Bold")
			(default . custom:asis)
			(type . triggle))
		       "              "
		       ((tag . "Italic")
			(default . custom:asis)
			(type . triggle))
		       "             "
		       ((tag . "Underline")
			(hidden . t)
			(default . custom:asis)
			(type . triggle)))
		 (default . (custom-face-lookup "default" "default" "default"
						nil nil nil))
		 (type . list))
		((prompt . "Other")
		 (face . custom-field-value)
		 (default . __uninitialized__)
		 (type . symbol))))
    (file (type . string)
	  ;; A string containing a file or directory name.
	  (directory . nil)
	  (default-file . nil)
	  (query . custom-file-query))
    (sexp (type . default)
	  ;; Any lisp expression.
	  (width . 40)
	  (default . (__uninitialized__ . "Uninitialized"))
	  (read . custom-sexp-read)
	  (write . custom-sexp-write))
    (symbol (type . sexp)
	    ;; A lisp symbol.
	    (width . 40)
	    (valid . (lambda (c d) (symbolp d))))
    (integer (type . sexp)
	     ;; A lisp integer.
	     (width . 10)
	     (valid . (lambda (c d) (integerp d))))
    (string (type . default)
	    ;; A lisp string.
	    (width . 40) 
	    (valid . (lambda (c d) (stringp d)))
	    (read . custom-string-read)
	    (write . custom-string-write))
    (button (type . default)
	    ;; Push me.
	    (accept . ignore)
	    (extract . nil)
	    (validate . ignore)
	    (insert . custom-button-insert))
    (doc (type . default)
	 ;; A documentation only entry with no value.
	 (header . nil)
	 (reset . ignore)
	 (extract . nil)
	 (validate . ignore)
	 (insert . custom-documentation-insert))
    (default (width . 20)
             (valid . (lambda (c v) t))
	     (insert . custom-default-insert)
	     (update . custom-default-update)
	     (query . custom-default-query)
	     (tag . nil)
	     (prompt . nil)
	     (doc . nil)
	     (header . t)
	     (padding . ? )
	     (quote . custom-default-quote)
	     (eval . (lambda (c v) nil))
	     (export . custom-default-export)
	     (import . (lambda (c v) (list v)))
	     (synchronize . ignore)
	     (initialize . custom-default-initialize)
	     (extract . custom-default-extract)
	     (validate . custom-default-validate)
	     (apply . custom-default-apply)
	     (reset . custom-default-reset)
	     (factory-reset . custom-default-factory-reset)
	     (accept . custom-default-accept)
	     (match . custom-default-match)
	     (name . nil)
	     (compact . nil)
	     (hidden . nil)
	     (face . custom-default-face)
	     (data . nil)
	     (calculate . nil)
	     (default . __uninitialized__)))
  "Alist of default properties for type symbols.
The format is `((SYMBOL (PROPERTY . VALUE)... )... )'.")

(defconst custom-local-type-properties nil
  "Local type properties.
Entries in this list take precedence over `custom-type-properties'.")

(make-variable-buffer-local 'custom-local-type-properties)

(defconst custom-nil '__uninitialized__
  "Special value representing an uninitialized field.")

(defconst custom-invalid '__invalid__
  "Special value representing an invalid field.")

(defconst custom:asis 'custom:asis)
;; Bad, ugly, and horrible kludge.

(defun custom-property (custom property)
  "Extract from CUSTOM property PROPERTY."
  (let ((entry (assq property custom)))
    (while (null entry)
      ;; Look in superclass.
      (let ((type (custom-type custom)))
	(setq custom (cdr (or (assq type custom-local-type-properties)
			      (assq type custom-type-properties)))
	      entry (assq property custom))
	(custom-assert 'custom)))
    (cdr entry)))

(defun custom-super (custom property)
  "Extract from CUSTOM property PROPERTY.  Start with CUSTOM's superclass."
  (let ((entry nil))
    (while (null entry)
      ;; Look in superclass.
      (let ((type (custom-type custom)))
	(setq custom (cdr (or (assq type custom-local-type-properties)
			      (assq type custom-type-properties)))
	      entry (assq property custom))
	(custom-assert 'custom)))
    (cdr entry)))

(defun custom-property-set (custom property value)
  "Set CUSTOM PROPERTY to VALUE by side effect.
CUSTOM must have at least one property already."
  (let ((entry (assq property custom)))
    (if entry
	(setcdr entry value)
      (setcdr custom (cons (cons property value) (cdr custom))))))

(defun custom-type (custom)
  "Extract `type' from CUSTOM."
  (cdr (assq 'type custom)))

(defun custom-name (custom)
  "Extract `name' from CUSTOM."
  (custom-property custom 'name))

(defun custom-tag (custom)
  "Extract `tag' from CUSTOM."
  (custom-property custom 'tag))

(defun custom-face-tag (custom)
  "Extract `face-tag' from CUSTOM."
  (custom-property custom 'face-tag))

(defun custom-prompt (custom)
  "Extract `prompt' from CUSTOM.  
If none exist, default to `tag' or, failing that, `type'."
  (or (custom-property custom 'prompt)
      (custom-property custom 'tag)
      (capitalize (symbol-name (custom-type custom)))))

(defun custom-default (custom)
  "Extract `default' from CUSTOM."
  (let ((value (custom-property custom 'calculate)))
    (if value
	(eval value)
      (custom-property custom 'default))))
	       
(defun custom-data (custom)
  "Extract the `data' from CUSTOM."
  (custom-property custom 'data))

(defun custom-documentation (custom)
  "Extract `doc' from CUSTOM."
  (custom-property custom 'doc))

(defun custom-width (custom)
  "Extract `width' from CUSTOM."
  (custom-property custom 'width))

(defun custom-compact (custom)
  "Extract `compact' from CUSTOM."
  (custom-property custom 'compact))

(defun custom-padding (custom)
  "Extract `padding' from CUSTOM."
  (custom-property custom 'padding))

(defun custom-valid (custom value)
  "Non-nil if CUSTOM may validly be set to VALUE."
  (and (not (and (listp value) (eq custom-invalid (car value))))
       (funcall (custom-property custom 'valid) custom value)))

(defun custom-import (custom value)
  "Import CUSTOM VALUE from external variable.

This function change VALUE into a form that makes it easier to edit 
internally.  What the internal form is exactly depends on CUSTOM.  
The internal form is returned."
  (if (eq custom-nil value)
      (list custom-nil)
    (funcall (custom-property custom 'import) custom value)))

(defun custom-eval (custom value)
  "Return non-nil if CUSTOM's VALUE needs to be evaluated."
  (funcall (custom-property custom 'eval) custom value))

(defun custom-quote (custom value)
  "Quote CUSTOM's VALUE if necessary."
  (funcall (custom-property custom 'quote) custom value))

(defun custom-write (custom value)
  "Convert CUSTOM VALUE to a string."
  (cond ((eq value custom-nil) 
	 "")
	((and (listp value) (eq (car value) custom-invalid))
	 (cdr value))
	(t
	 (funcall (custom-property custom 'write) custom value))))

(defun custom-read (custom string)
  "Convert CUSTOM field content STRING into lisp."
  (condition-case nil
      (funcall (custom-property custom 'read) custom string)
    (error (cons custom-invalid string))))

(defun custom-match (custom values)
  "Match CUSTOM with a list of VALUES.

Return a cons-cell where the car is the sublist of VALUES matching CUSTOM,
and the cdr is the remaining VALUES.

A CUSTOM is actually a regular expression over the alphabet of lisp
types.  Most CUSTOM types are just doing a literal match, e.g. the
`symbol' type matches any lisp symbol.  The exceptions are:

group:    which corresponds to a `(' and `)' group in a regular expression.
choice:   which corresponds to a group of `|' in a regular expression.
repeat:   which corresponds to a `*' in a regular expression.
optional: which corresponds to a `?', and isn't implemented yet."
  (if (memq values (list custom-nil nil))
      ;; Nothing matches the uninitialized or empty list.
      (cons custom-nil nil)
    (funcall (custom-property custom 'match) custom values)))

(defun custom-initialize (custom)
  "Initialize `doc' and `default' attributes of CUSTOM."
  (funcall (custom-property custom 'initialize) custom))

(defun custom-find (custom tag)
  "Find child in CUSTOM with `tag' TAG."
  (funcall (custom-property custom 'find) custom tag))

(defun custom-travel-path (custom path)
  "Find decedent of CUSTOM by looking through PATH."
  (if (null path)
      custom
    (custom-travel-path (custom-find custom (car path)) (cdr path))))

(defun custom-field-extract (custom field)
  "Extract CUSTOM's value in FIELD."
  (if (stringp custom)
      nil
    (funcall (custom-property (custom-field-custom field) 'extract)
	     custom field)))

(defun custom-field-validate (custom field)
  "Validate CUSTOM's value in FIELD.
Return nil if valid, otherwise return a cons-cell where the car is the
position of the error, and the cdr is a text describing the error."
  (if (stringp custom)
      nil
    (funcall (custom-property custom 'validate) custom field)))

;;; Field Functions:
;;
;; This section defines the public functions for manipulating the
;; FIELD datatype.  The FIELD instance hold information about a
;; specific editing field in the customization buffer.
;;
;; Each FIELD can be seen as an instantiation of a CUSTOM.

(defvar custom-field-last nil)
;; Last field containing point.
(make-variable-buffer-local 'custom-field-last)

(defvar custom-modified-list nil)
;; List of modified fields.
(make-variable-buffer-local 'custom-modified-list)

(defun custom-field-create (custom value)
  "Create a field structure of type CUSTOM containing VALUE.

A field structure is an array [ CUSTOM VALUE ORIGINAL START END ], where
CUSTOM defines the type of the field, 
VALUE is the current value of the field,
ORIGINAL is the original value when created, and
START and END are markers to the start and end of the field."
  (vector custom value custom-nil nil nil))

(defun custom-field-custom (field)
  "Return the `custom' attribute of FIELD."
  (aref field 0))
  
(defun custom-field-value (field)
  "Return the `value' attribute of FIELD."
  (aref field 1))

(defun custom-field-original (field)
  "Return the `original' attribute of FIELD."
  (aref field 2))

(defun custom-field-start (field)
  "Return the `start' attribute of FIELD."
  (aref field 3))

(defun custom-field-end (field)
  "Return the `end' attribute of FIELD."
  (aref field 4))
  
(defun custom-field-value-set (field value)
  "Set the `value' attribute of FIELD to VALUE."
  (aset field 1 value))

(defun custom-field-original-set (field original)
  "Set the `original' attribute of FIELD to ORIGINAL."
  (aset field 2 original))

(defun custom-field-move (field start end)
  "Set the `start'and `end' attributes of FIELD to START and END."
  (set-marker (or (aref field 3) (aset field 3 (make-marker))) start)
  (set-marker (or (aref field 4) (aset field 4 (make-marker))) end))

(defun custom-field-query (field)
  "Query user for content of current field."
  (funcall (custom-property (custom-field-custom field) 'query) field))

(defun custom-field-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE.
If optional ORIGINAL is non-nil, consider VALUE for the original value."
  (let ((inhibit-point-motion-hooks t))
    (funcall (custom-property (custom-field-custom field) 'accept) 
	     field value original)))

(defun custom-field-face (field)
  "The face used for highlighting FIELD."
  (let ((custom (custom-field-custom field)))
    (if (stringp custom)
	nil
      (let ((face (funcall (custom-property custom 'face) field)))
	(if (custom-facep face) face nil)))))

(defun custom-field-update (field)
  "Update the screen appearance of FIELD to correspond with the field's value."
  (let ((custom (custom-field-custom field)))
    (if (stringp custom)
	nil
      (funcall (custom-property custom 'update) field))))

;;; Types:
;;
;; The following functions defines type specific actions.

(defun custom-repeat-eval (custom value)
  "Non-nil if CUSTOM's VALUE needs to be evaluated."
  (if (eq value custom-nil)
      nil
    (let ((child (custom-data custom))
	  (found nil))
      (mapcar (lambda (v) (if (custom-eval child v) (setq found t)))
	      value))))

(defun custom-repeat-quote (custom value)
  "A list of CUSTOM's VALUEs quoted."
  (let ((child (custom-data custom)))
    (apply 'append (mapcar (lambda (v) (custom-quote child v))
			   value))))

  
(defun custom-repeat-import (custom value)
  "Modify CUSTOM's VALUE to match internal expectations."
  (let ((child (custom-data custom)))
    (apply 'append (mapcar (lambda (v) (custom-import child v))
			   value))))

(defun custom-repeat-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE."
  (let ((values (copy-sequence (custom-field-value field)))
	(all (custom-field-value field))
	(start (custom-field-start field))
	current new)
    (if original 
	(custom-field-original-set field value))
    (while (consp value)
      (setq new (car value)
	    value (cdr value))
      (if values
	  ;; Change existing field.
	  (setq current (car values)
		values (cdr values))
	;; Insert new field if series has grown.
	(goto-char start)
	(setq current (custom-repeat-insert-entry field))
	(setq all (custom-insert-before all nil current))
	(custom-field-value-set field all))
      (custom-field-accept current new original))
    (while (consp values)
      ;; Delete old field if series has scrunk.
      (setq current (car values)
	    values (cdr values))
      (let ((pos (custom-field-start current))
	    data)
	(while (not data)
	  (setq pos (previous-single-property-change pos 'custom-data))
	  (custom-assert 'pos)
	  (setq data (get-text-property pos 'custom-data))
	  (or (and (arrayp data)
		   (> (length data) 1)
		   (eq current (aref data 1)))
	      (setq data nil)))
	(custom-repeat-delete data)))))

(defun custom-repeat-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (let* ((field (custom-field-create custom nil))
	 (add-tag (custom-property custom 'add-tag))
	 (start (make-marker))
	 (data (vector field nil start nil)))
    (custom-text-insert "\n")
    (let ((pos (point)))
      (custom-text-insert (custom-property custom 'prefix))
      (custom-tag-insert add-tag 'custom-repeat-add data)
      (set-marker start pos))
    (custom-field-move field start (point))
    (custom-documentation-insert custom)
    field))

(defun custom-repeat-insert-entry (repeat)
  "Insert entry at point in the REPEAT field."
  (let* ((inhibit-point-motion-hooks t)
	 (inhibit-read-only t)
	 (before-change-functions nil)
	 (after-change-functions nil)
	 (custom (custom-field-custom repeat))
	 (add-tag (custom-property custom 'add-tag))
	 (del-tag (custom-property custom 'del-tag))
	 (start (make-marker))
	 (end (make-marker))
	 (data (vector repeat nil start end))
	 field)
    (custom-extent-start-open)
    (insert-before-markers "\n")
    (backward-char 1)
    (set-marker start (point))
    (custom-text-insert " ")
    (aset data 1 (setq field (custom-insert (custom-data custom) nil)))
    (custom-text-insert " ")
    (set-marker end (point))
    (goto-char start)
    (custom-text-insert (custom-property custom 'prefix))
    (custom-tag-insert add-tag 'custom-repeat-add data)
    (custom-text-insert " ")
    (custom-tag-insert del-tag 'custom-repeat-delete data)
    (forward-char 1)
    field))

(defun custom-repeat-add (data)
  "Add list entry."
  (let ((parent (aref data 0))
	(field (aref data 1))
	(at (aref data 2))
	new)
    (goto-char at)
    (setq new (custom-repeat-insert-entry parent))
    (custom-field-value-set parent
			    (custom-insert-before (custom-field-value parent)
						  field new))))

(defun custom-repeat-delete (data)
  "Delete list entry."
  (let ((inhibit-point-motion-hooks t)
	(inhibit-read-only t)
	(before-change-functions nil)
	(after-change-functions nil)
	(parent (aref data 0))
	(field (aref data 1)))
    (delete-region (aref data 2) (1+ (aref data 3)))
    (custom-field-untouch (aref data 1))
    (custom-field-value-set parent 
			    (delq field (custom-field-value parent)))))

(defun custom-repeat-match (custom values)
  "Match CUSTOM with VALUES."
  (let* ((child (custom-data custom))
	 (match (custom-match child values))
	 matches)
    (while (not (eq (car match) custom-nil))
      (setq matches (cons (car match) matches)
	    values (cdr match)
	    match (custom-match child values)))
    (cons (nreverse matches) values)))

(defun custom-repeat-extract (custom field)
  "Extract list of children's values."
  (let ((values (custom-field-value field))
	(data (custom-data custom))
	result)
    (if (eq values custom-nil)
	()
      (while values
	(setq result (append result (custom-field-extract data (car values)))
	      values (cdr values))))
    result))

(defun custom-repeat-validate (custom field)
  "Validate children."
  (let ((values (custom-field-value field))
	(data (custom-data custom))
	result)
    (if (eq values custom-nil)
	(setq result (cons (custom-field-start field) "Uninitialized list")))
    (while (and values (not result))
      (setq result (custom-field-validate data (car values))
	    values (cdr values)))
    result))

(defun custom-pair-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE."
  (custom-group-accept field (list (car value) (cdr value)) original))

(defun custom-pair-eval (custom value)
  "Non-nil if CUSTOM's VALUE needs to be evaluated."
  (custom-group-eval custom (list (car value) (cdr value))))

(defun custom-pair-import (custom value)
  "Modify CUSTOM's VALUE to match internal expectations."
  (let ((result (car (custom-group-import custom 
					  (list (car value) (cdr value))))))
    (custom-assert '(eq (length result) 2))
    (list (cons (nth 0 result) (nth 1 result)))))

(defun custom-pair-quote (custom value)
  "Quote CUSTOM's VALUE if necessary."
  (if (custom-eval custom value)
      (let ((v (car (custom-group-quote custom 
					(list (car value) (cdr value))))))
	(list (list 'cons (nth 0 v) (nth 1 v))))
    (custom-default-quote custom value)))

(defun custom-pair-extract (custom field)
  "Extract cons of children's values."
  (let ((values (custom-field-value field))
	(data (custom-data custom))
	result)
    (custom-assert '(eq (length values) (length data)))
    (while values
      (setq result (append result
			   (custom-field-extract (car data) (car values)))
	    data (cdr data)
	    values (cdr values)))
    (custom-assert '(null data))
    (list (cons (nth 0 result) (nth 1 result)))))

(defun custom-list-quote (custom value)
  "Quote CUSTOM's VALUE if necessary."
  (if (custom-eval custom value)
      (let ((v (car (custom-group-quote custom value))))
	(list (cons 'list v)))
    (custom-default-quote custom value)))

(defun custom-list-extract (custom field)
  "Extract list of children's values."
  (let ((values (custom-field-value field))
	(data (custom-data custom))
	result)
    (custom-assert '(eq (length values) (length data)))
    (while values
      (setq result (append result
			   (custom-field-extract (car data) (car values)))
	    data (cdr data)
	    values (cdr values)))
    (custom-assert '(null data))
    (list result)))

(defun custom-group-validate (custom field)
  "Validate children."
  (let ((values (custom-field-value field))
	(data (custom-data custom))
	result)
    (if (eq values custom-nil)
	(setq result (cons (custom-field-start field) "Uninitialized list"))
      (custom-assert '(eq (length values) (length data))))
    (while (and values (not result))
      (setq result (custom-field-validate (car data) (car values))
	    data (cdr data)
	    values (cdr values)))
    result))

(defun custom-group-eval (custom value)
  "Non-nil if CUSTOM's VALUE needs to be evaluated."
  (let ((found nil))
    (mapcar (lambda (c)
	      (or (stringp c)
		  (let ((match (custom-match c value)))
		    (if (custom-eval c (car match))
			(setq found t))
		    (setq value (cdr match)))))
	    (custom-data custom))
    found))

(defun custom-group-quote (custom value)
  "A list of CUSTOM's VALUE members, quoted."
  (list (apply 'append 
	       (mapcar (lambda (c)
			 (if (stringp c)
			     ()
			   (let ((match (custom-match c value)))
			     (prog1 (custom-quote c (car match))
			       (setq value (cdr match))))))
		       (custom-data custom)))))

(defun custom-group-import (custom value)
  "Modify CUSTOM's VALUE to match internal expectations."
  (list (apply 'append 
	       (mapcar (lambda (c)
			 (if (stringp c)
			     ()
			   (let ((match (custom-match c value)))
			     (prog1 (custom-import c (car match))
			       (setq value (cdr match))))))
		       (custom-data custom)))))

(defun custom-group-initialize (custom)
  "Initialize `doc' and `default' entries in CUSTOM."
  (if (custom-name custom)
      (custom-default-initialize custom)
    (mapcar 'custom-initialize (custom-data custom))))

(defun custom-group-apply (field)
  "Reset `value' in FIELD to `original'."
  (let ((custom (custom-field-custom field))
	(values (custom-field-value field)))
    (if (custom-name custom)
	(custom-default-apply field)
      (mapcar 'custom-field-apply values))))

(defun custom-group-reset (field)
  "Reset `value' in FIELD to `original'."
  (let ((custom (custom-field-custom field))
	(values (custom-field-value field)))
    (if (custom-name custom)
	(custom-default-reset field)
      (mapcar 'custom-field-reset values))))

(defun custom-group-factory-reset (field)
  "Reset `value' in FIELD to `default'."
  (let ((custom (custom-field-custom field))
	(values (custom-field-value field)))
    (if (custom-name custom)
	(custom-default-factory-reset field)
      (mapcar 'custom-field-factory-reset values))))

(defun custom-group-find (custom tag)
  "Find child in CUSTOM with `tag' TAG."
  (let ((data (custom-data custom))
	(result nil))
    (while (not result)
      (custom-assert 'data)
      (if (equal (custom-tag (car data)) tag)
	  (setq result (car data))
	(setq data (cdr data))))))

(defun custom-group-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE."
  (let* ((values (custom-field-value field))
	 (custom (custom-field-custom field))
	 (from (custom-field-start field))
	 (face-tag (custom-face-tag custom))
	 current)
    (if face-tag 
	(custom-put-text-property from (+ from (length (custom-tag custom)))
			   'face (funcall face-tag field value)))
    (if original 
	(custom-field-original-set field value))
    (while values
      (setq current (car values)
	    values (cdr values))
      (if current
	  (let* ((custom (custom-field-custom current))
		 (match (custom-match custom value)))
	    (setq value (cdr match))
	    (custom-field-accept current (car match) original))))))

(defun custom-group-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (let* ((field (custom-field-create custom nil))
	 fields hidden
	 (from (point))
	 (compact (custom-compact custom))
	 (tag (custom-tag custom))
	 (face-tag (custom-face-tag custom)))
    (cond (face-tag (custom-text-insert tag))
	  (tag (custom-tag-insert tag field)))
    (or compact (custom-documentation-insert custom))
    (or compact (custom-text-insert "\n"))
    (let ((data (custom-data custom)))
      (while data
	(setq fields (cons (custom-insert (car data) (if level (1+ level)))
			   fields))
	(setq hidden (or (stringp (car data))
			 (custom-property (car data) 'hidden)))
	(setq data (cdr data))
	(if data (custom-text-insert (cond (hidden "")
					   (compact " ")
					   (t "\n"))))))
    (if compact (custom-documentation-insert custom))
    (custom-field-value-set field (nreverse fields))
    (custom-field-move field from (point))
    field))

(defun custom-choice-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (let* ((field (custom-field-create custom nil))
	 (from (point)))
    (custom-text-insert "lars er en nisse")
    (custom-field-move field from (point))
    (custom-documentation-insert custom)
    (custom-field-reset field)
    field))

(defun custom-choice-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE."
  (let ((custom (custom-field-custom field))
	(start (custom-field-start field))
	(end (custom-field-end field))
	(inhibit-read-only t)
	(before-change-functions nil)
	(after-change-functions nil)
	from)
    (cond (original 
	   (setq custom-modified-list (delq field custom-modified-list))
	   (custom-field-original-set field value))
	  ((equal value (custom-field-original field))
	   (setq custom-modified-list (delq field custom-modified-list)))
	  (t
	   (add-to-list 'custom-modified-list field)))
    (custom-field-untouch (custom-field-value field))
    (delete-region start end)
    (goto-char start)
    (setq from (point))
    (insert-before-markers " ")
    (backward-char 1)
    (custom-category-set (point) (1+ (point)) 'custom-hidden-properties)
    (custom-tag-insert (custom-tag custom) field)
    (custom-text-insert ": ")
    (let ((data (custom-data custom))
	  found begin)
      (while (and data (not found))
	(if (not (custom-valid (car data) value))
	    (setq data (cdr data))
	  (setq found (custom-insert (car data) nil))
	  (setq data nil)))
      (if found 
	  ()
	(setq begin (point)
	      found (custom-insert (custom-property custom 'none) nil))
	(custom-add-text-properties 
	 begin (point)
	 (list rear-nonsticky t
	       'face custom-field-uninitialized-face)))
      (or original
	  (custom-field-original-set found (custom-field-original field)))
      (custom-field-accept found value original)
      (custom-field-value-set field found)
      (custom-field-move field from end))))

(defun custom-choice-extract (custom field)
  "Extract child's value."
  (let ((value (custom-field-value field)))
    (custom-field-extract (custom-field-custom value) value)))

(defun custom-choice-validate (custom field)
  "Validate child's value."
  (let ((value (custom-field-value field))
	(custom (custom-field-custom field)))
    (if (or (eq value custom-nil)
	    (eq (custom-field-custom value) (custom-property custom 'none)))
	(cons (custom-field-start field) "Make a choice")
      (custom-field-validate (custom-field-custom value) value))))

(defun custom-choice-query (field)
  "Choose a child."
  (let* ((custom (custom-field-custom field))
	 (old (custom-field-custom (custom-field-value field)))
	 (default (custom-prompt old))
	 (tag (custom-prompt custom))
	 (data (custom-data custom))
	 current alist)
    (if (eq (length data) 2)
	(custom-field-accept field (custom-default (if (eq (nth 0 data) old)
						       (nth 1 data)
						     (nth 0 data))))
      (while data
	(setq current (car data)
	      data (cdr data))
	(setq alist (cons (cons (custom-prompt current) current) alist)))
      (let ((answer (cond ((and (fboundp 'button-press-event-p)
				(fboundp 'popup-menu)
				(button-press-event-p last-input-event))
			   (cdr (assoc (car (custom-x-really-popup-menu 
					     last-input-event tag 
					     (reverse alist)))
				       alist)))
			  ((listp last-input-event)
			   (x-popup-menu last-input-event
					 (list tag (cons "" (reverse alist)))))
			  (t 
			   (let ((choice (completing-read (concat tag
								  " (default "
								  default 
								  "): ") 
							  alist nil t)))
			     (if (or (null choice) (string-equal choice ""))
				 (setq choice default))
			     (cdr (assoc choice alist)))))))
	(if answer
	    (custom-field-accept field (custom-default answer)))))))

(defun custom-file-query (field)
  "Prompt for a file name"
  (let* ((value (custom-field-value field))
	 (custom (custom-field-custom field))
	 (valid (custom-valid custom value))
	 (directory (custom-property custom 'directory))
	 (default (and (not valid)
		       (custom-property custom 'default-file)))
	 (tag (custom-tag custom))
	 (prompt (if default
		     (concat tag " (" default "): ")
		   (concat tag ": "))))
    (custom-field-accept field 
			 (if (custom-valid custom value)
			     (read-file-name prompt 
					     (if (file-name-absolute-p value)
						 ""
					       directory)
					     default nil value)
			   (read-file-name prompt directory default)))))

(defun custom-face-eval (custom value)
  "Return non-nil if CUSTOM's VALUE needs to be evaluated."
  (not (symbolp value)))

(defun custom-face-import (custom value)
  "Modify CUSTOM's VALUE to match internal expectations."
  (let ((name (or (and (facep value) (symbol-name (face-name value)))
		  (symbol-name value))))
    (list (if (string-match "\
custom-face-\\(.*\\)-\\(.*\\)-\\(.*\\)-\\(.*\\)-\\(.*\\)-\\(.*\\)"
			    name)
	      (list 'custom-face-lookup 
		    (match-string 1 name)
		    (match-string 2 name)
		    (match-string 3 name)
		    (intern (match-string 4 name))
		    (intern (match-string 5 name))
		    (intern (match-string 6 name)))
	    value))))

(defun custom-face-lookup (&optional fg bg stipple bold italic underline)
  "Lookup or create a face with specified attributes."
  (let ((name (intern (format "custom-face-%s-%s-%s-%S-%S-%S"
			      (or fg "default")
			      (or bg "default")
			      (or stipple "default")
			      bold italic underline))))
    (if (and (custom-facep name)
	     (fboundp 'make-face))
	()
      (copy-face 'default name)
      (when (and fg
		 (not (string-equal fg "default")))
	(condition-case ()
	    (set-face-foreground name fg)
	  (error nil)))
      (when (and bg
		 (not (string-equal bg "default")))
	(condition-case ()
	    (set-face-background name bg)
	  (error nil)))
      (when (and stipple
		 (not (string-equal stipple "default"))
		 (not (eq stipple 'custom:asis))
		 (fboundp 'set-face-stipple))
	(set-face-stipple name stipple))
      (when (and bold
		 (not (eq bold 'custom:asis)))
	(condition-case ()
	    (make-face-bold name)
	  (error nil)))
      (when (and italic
		 (not (eq italic 'custom:asis)))
	(condition-case ()
	    (make-face-italic name)
	  (error nil)))
      (when (and underline
		 (not (eq underline 'custom:asis)))
	(condition-case ()
	    (set-face-underline-p name t)
	  (error nil))))
    name))

(defun custom-face-hack (field value)
  "Face that should be used for highlighting FIELD containing VALUE."
  (let* ((custom (custom-field-custom field))
	 (form (funcall (custom-property custom 'export) custom value))
	 (face (apply (car form) (cdr form))))
    (if (custom-facep face) face nil)))

(defun custom-const-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (let* ((field (custom-field-create custom custom-nil))
	 (face (custom-field-face field))
	 (from (point)))
    (custom-text-insert (custom-tag custom))
    (custom-add-text-properties from (point) 
			 (list 'face face
			       rear-nonsticky t))
    (custom-documentation-insert custom)
    (custom-field-move field from (point))
    field))

(defun custom-const-update (field)
  "Update face of FIELD."
  (let ((from (custom-field-start field))
	(custom (custom-field-custom field)))
    (custom-put-text-property from (+ from (length (custom-tag custom)))
		       'face (custom-field-face field))))

(defun custom-const-valid (custom value)
  "Non-nil if CUSTOM can validly have the value VALUE."
  (equal (custom-default custom) value))

(defun custom-const-face (field)
  "Face used for a FIELD."
  (custom-default (custom-field-custom field)))

(defun custom-sexp-read (custom string)
  "Read from CUSTOM an STRING."
  (save-match-data
    (save-excursion
      (set-buffer (get-buffer-create " *Custom Scratch*"))
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (prog1 (read (current-buffer))
	(or (looking-at
	     (concat (regexp-quote (char-to-string
				    (custom-padding custom)))
		     "*\\'"))
	    (error "Junk at end of expression"))))))

(autoload 'pp-to-string "pp")

(defun custom-sexp-write (custom sexp)
  "Write CUSTOM SEXP as string."
  (let ((string (prin1-to-string sexp)))
    (if (<= (length string) (custom-width custom))
	string
      (setq string (pp-to-string sexp))
      (string-match "[ \t\n]*\\'" string)
      (concat "\n" (substring string 0 (match-beginning 0))))))

(defun custom-string-read (custom string)
  "Read string by ignoring trailing padding characters."
  (let ((last (length string))
	(padding (custom-padding custom)))
    (while (and (> last 0)
		(eq (aref string (1- last)) padding))
      (setq last (1- last)))
    (substring string 0 last)))

(defun custom-string-write (custom string)
  "Write raw string."
  string)

(defun custom-button-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (custom-tag-insert (concat "[" (custom-tag custom) "]") 
		     (custom-property custom 'query))
  (custom-documentation-insert custom)
  nil)

(defun custom-default-export (custom value)
  ;; Convert CUSTOM's VALUE to external representation.
  ;; See `custom-import'.
  (if (custom-eval custom value)
      (eval (car (custom-quote custom value)))
    value))

(defun custom-default-quote (custom value)
  "Quote CUSTOM's VALUE if necessary."
  (list (if (and (not (custom-eval custom value))
		 (or (and (symbolp value)
			  value 
			  (not (eq t value)))
		     (and (listp value)
			  value
			  (not (memq (car value) '(quote function lambda))))))
	    (list 'quote value)
	  value)))

(defun custom-default-initialize (custom)
  "Initialize `doc' and `default' entries in CUSTOM."
  (let ((name (custom-name custom)))
    (if (null name)
	()
      (let ((default (custom-default custom))
	    (doc (custom-documentation custom))
	    (vdoc (documentation-property name 'variable-documentation t)))
	(if doc
	    (or vdoc (put name 'variable-documentation doc))
	  (if vdoc (custom-property-set custom 'doc vdoc)))
	(if (eq default custom-nil)
	    (if (boundp name)
		(custom-property-set custom 'default (symbol-value name)))
	  (or (boundp name)
	      (set name default)))))))

(defun custom-default-insert (custom level)
  "Insert field for CUSTOM at nesting LEVEL in customization buffer."
  (let ((field (custom-field-create custom custom-nil))
	(tag (custom-tag custom)))
    (if (null tag)
	()
      (custom-tag-insert tag field)
      (custom-text-insert ": "))
    (custom-field-insert field)
    (custom-documentation-insert custom)
    field))

(defun custom-default-accept (field value &optional original)
  "Store a new value into field FIELD, taking it from VALUE."
  (if original 
      (custom-field-original-set field value))
  (custom-field-value-set field value)
  (custom-field-update field))
  
(defun custom-default-apply (field)
  "Apply any changes in FIELD since the last apply."
  (let* ((custom (custom-field-custom field))
	 (name (custom-name custom)))
    (if (null name)
	(error "This field cannot be applied alone"))
    (custom-external-set name (custom-name-value name))
    (custom-field-reset field)))

(defun custom-default-reset (field)
  "Reset content of editing FIELD to `original'."
  (custom-field-accept field (custom-field-original field) t))

(defun custom-default-factory-reset (field)
  "Reset content of editing FIELD to `default'."
  (let* ((custom (custom-field-custom field))
	 (default (car (custom-import custom (custom-default custom)))))
    (or (eq default custom-nil)
	(custom-field-accept field default nil))))

(defun custom-default-query (field)
  "Prompt for a FIELD"
  (let* ((custom (custom-field-custom field))
	 (value (custom-field-value field))
	 (initial (custom-write custom value))
	 (prompt (concat (custom-prompt custom) ": ")))
    (custom-field-accept field 
			 (custom-read custom 
				      (if (custom-valid custom value)
					  (read-string prompt (cons initial 1))
					(read-string prompt))))))

(defun custom-default-match (custom values)
  "Match CUSTOM with VALUES."
  values)

(defun custom-default-extract (custom field)
  "Extract CUSTOM's content in FIELD."
  (list (custom-field-value field)))

(defun custom-default-validate (custom field)
  "Validate FIELD."
  (let ((value (custom-field-value field))
	(start (custom-field-start field)))
    (cond ((eq value custom-nil)
	   (cons start "Uninitialized field"))
	  ((and (consp value) (eq (car value) custom-invalid))
	   (cons start "Unparsable field content"))
	  ((custom-valid custom value)
	   nil)
	  (t
	   (cons start "Wrong type of field content")))))

(defun custom-default-face (field)
  "Face used for a FIELD."
  (let ((value (custom-field-value field)))
    (cond ((eq value custom-nil)
	   custom-field-uninitialized-face)
	  ((not (custom-valid (custom-field-custom field) value))
	   custom-field-invalid-face)
	  ((not (equal (custom-field-original field) value))
	   custom-field-modified-face)
	  (t
	   custom-field-face))))

(defun custom-default-update (field)
  "Update the content of FIELD."
  (let ((inhibit-point-motion-hooks t)
	(before-change-functions nil)
	(after-change-functions nil)
	(start (custom-field-start field))
	(end (custom-field-end field)) 
	(pos (point)))
    ;; Keep track of how many modified fields we have.
    (cond ((equal (custom-field-value field) (custom-field-original field))
	   (setq custom-modified-list (delq field custom-modified-list)))
	  ((memq field custom-modified-list))
	  (t
	   (setq custom-modified-list (cons field custom-modified-list))))
    ;; Update the field.
    (goto-char end)
    (insert-before-markers " ")
    (delete-region start (1- end))
    (goto-char start)
    (custom-field-insert field)
    (goto-char end)
    (delete-char 1)
    (goto-char pos)
    (and (<= start pos) 
	 (<= pos end)
	 (custom-field-enter field))))

;;; Create Buffer:
;;
;; Public functions to create a customization buffer and to insert
;; various forms of text, fields, and buttons in it.

(defun customize ()
  "Customize GNU Emacs.
Create a *Customize* buffer with editable customization information
about GNU Emacs." 
  (interactive)
  (custom-buffer-create "*Customize*")
  (custom-reset-all))

(defun custom-buffer-create (name &optional custom types set get save)
  "Create a customization buffer named NAME.
If the optional argument CUSTOM is non-nil, use that as the custom declaration.
If the optional argument TYPES is non-nil, use that as the local types.
If the optional argument SET is non-nil, use that to set external data.
If the optional argument GET is non-nil, use that to get external data.
If the optional argument SAVE is non-nil, use that for saving changes."
  (switch-to-buffer name)
  (buffer-disable-undo (current-buffer))
  (custom-mode)
  (setq custom-local-type-properties types)
  (if (null custom)
      ()
    (make-local-variable 'custom-data)
    (setq custom-data custom))
  (if (null set)
      ()
    (make-local-variable 'custom-external-set)
    (setq custom-external-set set))
  (if (null get)
      ()
    (make-local-variable 'custom-external)
    (setq custom-external get))
  (if (null save)
      ()
    (make-local-variable 'custom-save)
    (setq custom-save save))
  (let ((inhibit-point-motion-hooks t)
	(before-change-functions nil)
	(after-change-functions nil))
    (erase-buffer)
    (insert "\n")
    (goto-char (point-min))
    (custom-text-insert "This is a customization buffer.\n")
    (custom-help-insert "\n")
    (custom-help-button 'custom-forward-field)
    (custom-help-button 'custom-backward-field)
    (custom-help-button 'custom-enter-value)
    (custom-help-button 'custom-field-factory-reset)
    (custom-help-button 'custom-field-reset)
    (custom-help-button 'custom-field-apply)
    (custom-help-button 'custom-save-and-exit)
    (custom-help-button 'custom-toggle-documentation)
    (custom-help-insert "\nClick mouse-2 on any button to activate it.\n")
    (custom-text-insert "\n")
    (custom-insert custom-data 0)
    (goto-char (point-min))))

(defun custom-insert (custom level)
  "Insert custom declaration CUSTOM in current buffer at level LEVEL."
  (if (stringp custom)
      (progn 
	(custom-text-insert custom)
	nil)
    (and level (null (custom-property custom 'header))
	 (setq level nil))
    (and level 
	 (> level 0)
	 (custom-text-insert (concat "\n" (make-string level ?*) " ")))
    (let ((field (funcall (custom-property custom 'insert) custom level)))
      (custom-name-enter (custom-name custom) field)
      field)))

(defun custom-text-insert (text)
  "Insert TEXT in current buffer." 
  (insert text))

(defun custom-tag-insert (tag field &optional data)
  "Insert TAG for FIELD in current buffer."
  (let ((from (point)))
    (insert tag)
    (custom-category-set from (point) 'custom-button-properties)
    (custom-put-text-property from (point) 'custom-tag field)
    (if data
	(custom-add-text-properties from (point) (list 'custom-data data)))))

(defun custom-documentation-insert (custom &rest ignore)
  "Insert documentation from CUSTOM in current buffer."
  (let ((doc (custom-documentation custom)))
    (if (null doc)
	()
      (custom-help-insert "\n" doc))))

(defun custom-help-insert (&rest args)
  "Insert ARGS as documentation text."
  (let ((from (point)))
    (apply 'insert args)
    (custom-category-set from (point) 'custom-documentation-properties)))

(defun custom-help-button (command)
  "Describe how to execute COMMAND."
  (let ((from (point)))
    (insert "`" (key-description (where-is-internal command nil t)) "'")
    (custom-set-text-properties from (point)
				(list 'face custom-button-face
				      mouse-face custom-mouse-face
				      'custom-jump t ;Make TAB jump over it.
				      'custom-tag command
				      'start-open t
				      'end-open t))
    (custom-category-set from (point) 'custom-documentation-properties))
  (custom-help-insert ": " (custom-first-line (documentation command)) "\n"))

;;; Mode:
;;
;; The Customization major mode and interactive commands. 

(defvar custom-mode-map nil
  "Keymap for Custom Mode.")
(if custom-mode-map
    nil
  (setq custom-mode-map (make-sparse-keymap))
  (define-key custom-mode-map (if (string-match "XEmacs" emacs-version) [button2] [mouse-2]) 'custom-push-button)
  (define-key custom-mode-map "\t" 'custom-forward-field)
  (define-key custom-mode-map "\M-\t" 'custom-backward-field)
  (define-key custom-mode-map "\r" 'custom-enter-value)
  (define-key custom-mode-map "\C-k" 'custom-kill-line)
  (define-key custom-mode-map "\C-c\C-r" 'custom-field-reset)
  (define-key custom-mode-map "\C-c\M-\C-r" 'custom-reset-all)
  (define-key custom-mode-map "\C-c\C-z" 'custom-field-factory-reset)
  (define-key custom-mode-map "\C-c\M-\C-z" 'custom-factory-reset-all)
  (define-key custom-mode-map "\C-c\C-a" 'custom-field-apply)
  (define-key custom-mode-map "\C-c\M-\C-a" 'custom-apply-all)
  (define-key custom-mode-map "\C-c\C-c" 'custom-save-and-exit)
  (define-key custom-mode-map "\C-c\C-d" 'custom-toggle-documentation))

;; C-c keymap ideas: C-a field-beginning, C-e field-end, C-f
;; forward-field, C-b backward-field, C-n next-field, C-p
;; previous-field, ? describe-field.

(defun custom-mode ()
  "Major mode for doing customizations.

\\{custom-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (make-local-variable 'before-change-functions)
  (setq before-change-functions '(custom-before-change))
  (make-local-variable 'after-change-functions)
  (setq after-change-functions '(custom-after-change))
  (if (not (fboundp 'make-local-hook))
      ;; Emacs 19.28 and earlier.
      (add-hook 'post-command-hook 
		(lambda ()
		  (if (eq major-mode 'custom-mode)
		      (custom-post-command))))
    ;; Emacs 19.29.
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'custom-post-command nil t)))

(defun custom-forward-field (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (while (> arg 0)
    (let ((next (if (get-text-property (point) 'custom-tag)
		    (next-single-property-change (point) 'custom-tag)
		  (point))))
      (setq next (or (next-single-property-change next 'custom-tag)
		     (next-single-property-change (point-min) 'custom-tag)))
      (if next
	  (goto-char next)
	(error "No customization fields in this buffer.")))
    (or (get-text-property (point) 'custom-jump)
	(setq arg (1- arg))))
  (while (< arg 0)
    (let ((previous (if (get-text-property (1- (point)) 'custom-tag)
			(previous-single-property-change (point) 'custom-tag)
		      (point))))
      (setq previous
	    (or (previous-single-property-change previous 'custom-tag)
		(previous-single-property-change (point-max) 'custom-tag)))
      (if previous
	  (goto-char previous)
	(error "No customization fields in this buffer.")))
    (or (get-text-property (1- (point)) 'custom-jump)
	(setq arg (1+ arg)))))

(defun custom-backward-field (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (custom-forward-field (- arg)))

(defun custom-toggle-documentation (&optional arg)
  "Toggle display of documentation text.
If the optional argument is non-nil, show text iff the argument is positive."
  (interactive "P")
  (let ((hide (or (and (null arg) 
		       (null (custom-category-get 
			      'custom-documentation-properties 'invisible)))
		  (<= (prefix-numeric-value arg) 0))))
    (custom-category-put 'custom-documentation-properties 'invisible hide)
    (custom-category-put 'custom-documentation-properties intangible hide))
  (redraw-display))

(defun custom-enter-value (field data)
  "Enter value for current customization field or push button."
  (interactive (list (get-text-property (point) 'custom-tag)
		     (get-text-property (point) 'custom-data)))
  (cond (data
	 (funcall field data))
	((eq field 'custom-enter-value)
	 (error "Don't be silly"))
	((and (symbolp field) (fboundp field))
	 (call-interactively field))
	(field
	 (custom-field-query field))
	(t
	 (message "Nothing to enter here"))))

(defun custom-kill-line ()
  "Kill to end of field or end of line, whichever is first."
  (interactive)
  (let ((field (get-text-property (point) 'custom-field))
	(newline (save-excursion (search-forward "\n")))
	(next (next-single-property-change (point) 'custom-field)))
    (if (and field (> newline next))
	(kill-region (point) next)
      (call-interactively 'kill-line))))

(defun custom-push-button (event)
  "Activate button below mouse pointer."
  (interactive "@e")
  (let* ((pos (event-point event))
         (field (get-text-property pos 'custom-field))
         (tag (get-text-property pos 'custom-tag))
	 (data (get-text-property pos 'custom-data)))
    (cond (data
	    (funcall tag data))
	  ((and (symbolp tag) (fboundp tag))
	   (call-interactively tag))
	  (field
	   (call-interactively (lookup-key global-map (this-command-keys))))
	  (tag
	   (custom-enter-value tag data))
	  (t 
	   (error "Nothing to click on here.")))))

(defun custom-reset-all ()
  "Undo any changes since the last apply in all fields."
  (interactive (and custom-modified-list
		    (not (y-or-n-p "Discard all changes? "))
		    (error "Reset aborted")))
  (let ((all custom-name-fields)
	current field)
    (while all
      (setq current (car all)
	    field (cdr current)
	    all (cdr all))
      (custom-field-reset field))))

(defun custom-field-reset (field)
  "Undo any changes in FIELD since the last apply."
  (interactive (list (or (get-text-property (point) 'custom-field)
			 (get-text-property (point) 'custom-tag))))
  (if (arrayp field)
      (let* ((custom (custom-field-custom field))
	     (name (custom-name custom)))
	(save-excursion
	  (if name
	      (custom-field-original-set 
	       field (car (custom-import custom (custom-external name)))))
	  (if (not (custom-valid custom (custom-field-original field)))
	      (error "This field cannot be reset alone")
	    (funcall (custom-property custom 'reset) field)
	    (funcall (custom-property custom 'synchronize) field))))))

(defun custom-factory-reset-all ()
  "Reset all field to their default values."
  (interactive (and custom-modified-list
		    (not (y-or-n-p "Discard all changes? "))
		    (error "Reset aborted")))
  (let ((all custom-name-fields)
	field)
    (while all
      (setq field (cdr (car all))
	    all (cdr all))
      (custom-field-factory-reset field))))

(defun custom-field-factory-reset (field)
  "Reset FIELD to its default value."
  (interactive (list (or (get-text-property (point) 'custom-field)
			 (get-text-property (point) 'custom-tag))))
  (if (arrayp field)
      (save-excursion
	(funcall (custom-property (custom-field-custom field) 'factory-reset)
		 field))))

(defun custom-apply-all ()
  "Apply any changes since the last reset in all fields."
  (interactive (if custom-modified-list
		   nil
		 (error "No changes to apply.")))
  (custom-field-parse custom-field-last)
  (let ((all custom-name-fields)
	field)
    (while all
      (setq field (cdr (car all))
	    all (cdr all))
      (let ((error (custom-field-validate (custom-field-custom field) field)))
	(if (null error)
	    ()
	  (goto-char (car error))
	  (error (cdr error))))))
  (let ((all custom-name-fields)
	field)
    (while all
      (setq field (cdr (car all))
	    all (cdr all))
      (custom-field-apply field))))

(defun custom-field-apply (field)
  "Apply any changes in FIELD since the last apply."
  (interactive (list (or (get-text-property (point) 'custom-field)
			 (get-text-property (point) 'custom-tag))))
  (custom-field-parse custom-field-last)
  (if (arrayp field)
      (let* ((custom (custom-field-custom field))
	     (error (custom-field-validate custom field)))
	(if error
	    (error (cdr error)))
	(funcall (custom-property custom 'apply) field))))

(defun custom-toggle-hide (&rest ignore)
  "Hide or show entry."
  (interactive)
  (error "This button is not yet implemented"))

(defun custom-save-and-exit ()
  "Save and exit customization buffer."
  (interactive "@")
  (save-excursion
   (funcall custom-save))
  (kill-buffer (current-buffer)))

(defun custom-save ()
  "Save customization information."
  (interactive)
  (custom-apply-all)
  (let ((new custom-name-fields))
    (set-buffer (find-file-noselect custom-file))
    (goto-char (point-min))
    (save-excursion
      (let ((old (condition-case nil
		     (read (current-buffer))
		   (end-of-file (append '(setq custom-dummy
					       'custom-dummy) ())))))
	(or (eq (car old) 'setq)
	    (error "Invalid customization file: %s" custom-file))
	(while new
	  (let* ((field (cdr (car new)))
		 (custom (custom-field-custom field))
		 (value (custom-field-original field))
		 (default (car (custom-import custom (custom-default custom))))
		 (name (car (car new))))
	    (setq new (cdr new))
	    (custom-assert '(eq name (custom-name custom)))
	    (if (equal default value)
		(setcdr old (custom-plist-delq name (cdr old)))
	      (setcdr old (plist-put (cdr old) name 
				     (car (custom-quote custom value)))))))
	(erase-buffer)
	(insert ";; " custom-file "\
 --- Automatically generated customization information.
;; 
;; Feel free to edit by hand, but the entire content should consist of
;; a single setq.  Any other lisp expressions will confuse the
;; automatic configuration engine.

\(setq ")
	(setq old (cdr old))
	(while old
	  (prin1 (car old) (current-buffer))
	  (setq old (cdr old))
	  (insert " ")
	  (pp (car old) (current-buffer))
	  (setq old (cdr old))
	  (if old (insert "\n      ")))
	(insert ")\n")
	(save-buffer)
	(kill-buffer (current-buffer))))))

(defun custom-load ()
  "Save customization information."
  (interactive (and custom-modified-list
		    (not (equal (list (custom-name-field 'custom-file))
				custom-modified-list))
		    (not (y-or-n-p "Discard all changes? "))
		    (error "Load aborted")))
  (load-file (custom-name-value 'custom-file))
  (custom-reset-all))

;;; Field Editing:
;;
;; Various internal functions for implementing the direct editing of
;; fields in the customization buffer.

(defun custom-field-untouch (field)
  ;; Remove FIELD and its children from `custom-modified-list'.
  (setq custom-modified-list (delq field custom-modified-list))
  (if (arrayp field)
      (let ((value (custom-field-value field)))
	(cond ((null (custom-data (custom-field-custom field))))
	      ((arrayp value)
	       (custom-field-untouch value))
	      ((listp value)
	       (mapcar 'custom-field-untouch value))))))


(defun custom-field-insert (field)
  ;; Insert editing FIELD in current buffer.
  (let ((from (point))
	(custom (custom-field-custom field))
	(value (custom-field-value field)))
    (insert (custom-write custom value))
    (insert-char (custom-padding custom)
		 (- (custom-width custom) (- (point) from)))
    (custom-field-move field from (point))
    (custom-set-text-properties 
     from (point)
     (list 'custom-field field
	   'custom-tag field
	   'face (custom-field-face field)
	   'start-open t
	   'end-open t))))

(defun custom-field-read (field)
  ;; Read the screen content of FIELD.
  (custom-read (custom-field-custom field)
	       (custom-buffer-substring-no-properties (custom-field-start field)
					       (custom-field-end field))))

;; Fields are shown in a special `active' face when point is inside
;; it.  You activate the field by moving point inside (entering) it
;; and deactivate the field by moving point outside (leaving) it.

(defun custom-field-leave (field)
  ;; Deactivate FIELD.
  (let ((before-change-functions nil)
	(after-change-functions nil))
    (custom-put-text-property (custom-field-start field) (custom-field-end field)
		       'face (custom-field-face field))))

(defun custom-field-enter (field)
  ;; Activate FIELD.
  (let* ((start (custom-field-start field)) 
	 (end (custom-field-end field))
	 (custom (custom-field-custom field))
	 (padding (custom-padding custom))
	 (before-change-functions nil)
	 (after-change-functions nil))
    (or (eq this-command 'self-insert-command)
	(let ((pos end))
	  (while (and (< start pos)
		      (eq (char-after (1- pos)) padding))
	    (setq pos (1- pos)))
	  (if (< pos (point))
	      (goto-char pos))))
    (custom-put-text-property start end 'face custom-field-active-face)))

(defun custom-field-resize (field)
  ;; Resize FIELD after change.
  (let* ((custom (custom-field-custom field))
	 (begin (custom-field-start field))
	 (end (custom-field-end field))
	 (pos (point))
	 (padding (custom-padding custom))
	 (width (custom-width custom))
	 (size (- end begin)))
    (cond ((< size width)
	   (goto-char end)
	   (if (fboundp 'insert-before-markers-and-inherit)
	       ;; Emacs 19.
	       (insert-before-markers-and-inherit
		(make-string (- width size) padding))
	     ;; XEmacs:  BUG:  Doesn't work!
	     (insert-before-markers (make-string (- width size) padding)))
	   (goto-char pos))
	  ((> size width)
	   (let ((start (if (and (< (+ begin width) pos) (<= pos end))
			    pos
			  (+ begin width))))
	     (goto-char end)
	     (while (and (< start (point)) (= (preceding-char) padding))
	       (backward-delete-char 1))
	     (goto-char pos))))))

(defvar custom-field-changed nil)
;; List of fields changed on the screen but whose VALUE attribute has
;; not yet been updated to reflect the new screen content.
(make-variable-buffer-local 'custom-field-changed)

(defun custom-field-parse (field)
  ;; Parse FIELD content iff changed.
  (if (memq field custom-field-changed)
      (progn 
	(setq custom-field-changed (delq field custom-field-changed))
	(custom-field-value-set field (custom-field-read field))
	(custom-field-update field))))

(defun custom-post-command ()
  ;; Keep track of their active field.
  (custom-assert '(eq major-mode 'custom-mode))
  (let ((field (custom-field-property (point))))
    (if (eq field custom-field-last)
	(if (memq field custom-field-changed)
	    (custom-field-resize field))
      (custom-field-parse custom-field-last)
      (if custom-field-last
	  (custom-field-leave custom-field-last))
      (if field
	  (custom-field-enter field))
      (setq custom-field-last field))
    (set-buffer-modified-p (or custom-modified-list
			       custom-field-changed))))

(defvar custom-field-was nil)
;; The custom data before the change.
(make-variable-buffer-local 'custom-field-was)

(defun custom-before-change (begin end)
  ;; Check that we the modification is allowed.
  (if (not (eq major-mode 'custom-mode))
      (message "Aargh! Why is custom-before-change called here?")
    (let ((from (custom-field-property begin))
	  (to (custom-field-property end)))
      (cond ((or (null from) (null to))
	     (error "You can only modify the fields"))
	    ((not (eq from to))
	     (error "Changes must be limited to a single field."))
	    (t
	     (setq custom-field-was from))))))

(defun custom-after-change (begin end length)
  ;; Keep track of field content.
  (if (not (eq major-mode 'custom-mode))
      (message "Aargh! Why is custom-after-change called here?")
    (let ((field custom-field-was))
      (custom-assert '(prog1 field (setq custom-field-was nil)))
      ;; Prevent mixing fields properties.
      (custom-put-text-property begin end 'custom-field field)
      ;; Update the field after modification.
      (if (eq (custom-field-property begin) field)
	  (let ((field-end (custom-field-end field)))
	    (if (> end field-end)
		(set-marker field-end end))
	    (add-to-list 'custom-field-changed field))
	;; We deleted the entire field, reinsert it.
	(custom-assert '(eq begin end))
	(save-excursion
	  (goto-char begin)
	  (custom-field-value-set field
				  (custom-read (custom-field-custom field) ""))
	  (custom-field-insert field))))))

(defun custom-field-property (pos)
  ;; The `custom-field' text property valid for POS.
  (or (get-text-property pos 'custom-field)
      (and (not (eq pos (point-min)))
	   (get-text-property (1- pos) 'custom-field))))

;;; Generic Utilities:
;;
;; Some utility functions that are not really specific to custom.

(defun custom-assert (expr)
  "Assert that EXPR evaluates to non-nil at this point"
  (or (eval expr)
      (error "Assertion failed: %S" expr)))

(defun custom-first-line (string)
  "Return the part of STRING before the first newline."
  (let ((pos 0)
	(len (length string)))
    (while (and (< pos len) (not (eq (aref string pos) ?\n)))
      (setq pos (1+ pos)))
    (if (eq pos len)
	string
    (substring string 0 pos))))

(defun custom-insert-before (list old new)
  "In LIST insert before OLD a NEW element."
  (cond ((null list)
	 (list new))
	((null old)
	 (nconc list (list new)))
	((eq old (car list))
	 (cons new list))
	(t
	 (let ((list list))
	   (while (not (eq old (car (cdr list))))
	     (setq list (cdr list))
	     (custom-assert '(cdr list)))
	   (setcdr list (cons new (cdr list))))
	 list)))

(defun custom-strip-padding (string padding)
  "Remove padding from STRING."
  (let ((regexp (concat (regexp-quote (char-to-string padding)) "+")))
    (while (string-match regexp string)
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (match-end 0))))))
  string)

(defun custom-plist-memq (prop plist)
  "Return non-nil if PROP is a property of PLIST.  Comparison done with EQ."
  (let (result)
    (while plist
      (if (eq (car plist) prop)
	  (setq result plist
		plist nil)
	(setq plist (cdr (cdr plist)))))
    result))

(defun custom-plist-delq (prop plist)
  "Delete property PROP from property list PLIST."
  (while (eq (car plist) prop)
    (setq plist (cdr (cdr plist))))
  (let ((list plist)
	(next (cdr (cdr plist))))
    (while next
      (if (eq (car next) prop)
	  (progn 
	    (setq next (cdr (cdr next)))
	    (setcdr (cdr list) next))
	(setq list next
	      next (cdr (cdr next))))))
  plist)

;;; Meta Customization:

(custom-declare '()
  '((tag . "Meta Customization")
    (doc . "Customization of the customization support.")
    (type . group)
    (data ((type . face-doc))
	  ((tag . "Button Face")
	   (default . bold)
	   (doc . "Face used for tags in customization buffers.")
	   (name . custom-button-face)
	   (synchronize . (lambda (f)
			    (custom-category-put 'custom-button-properties 
						 'face custom-button-face)))
	   (type . face))
	  ((tag . "Mouse Face")
	   (default . highlight)
	   (doc . "\
Face used when mouse is above a button in customization buffers.")
	   (name . custom-mouse-face)
	   (synchronize . (lambda (f)
			    (custom-category-put 'custom-button-properties 
						 mouse-face 
						 custom-mouse-face)))
	   (type . face))
	  ((tag . "Field Face")
	   (default . italic)
	   (doc . "Face used for customization fields.")
	   (name . custom-field-face)
	   (type . face))
	  ((tag . "Uninitialized Face")
	   (default . modeline)
	   (doc . "Face used for uninitialized customization fields.")
	   (name . custom-field-uninitialized-face)
	   (type . face))
	  ((tag . "Invalid Face")
	   (default . highlight)
	   (doc . "\
Face used for customization fields containing invalid data.")
	   (name . custom-field-invalid-face)
	   (type . face))
	  ((tag . "Modified Face")
	   (default . bold-italic)
	   (doc . "Face used for modified customization fields.")
	   (name . custom-field-modified-face)
	   (type . face))
	  ((tag . "Active Face")
	   (default . underline)
	   (doc . "\
Face used for customization fields while they are being edited.")
	   (name . custom-field-active-face)
	   (type . face)))))

;; custom.el uses two categories.

(custom-category-create 'custom-documentation-properties)
(custom-category-put 'custom-documentation-properties rear-nonsticky t)

(custom-category-create 'custom-button-properties)
(custom-category-put 'custom-button-properties 'face custom-button-face)
(custom-category-put 'custom-button-properties mouse-face custom-mouse-face)
(custom-category-put 'custom-button-properties rear-nonsticky t)

(custom-category-create 'custom-hidden-properties)
(custom-category-put 'custom-hidden-properties 'invisible
		     (not (string-match "XEmacs" emacs-version)))
(custom-category-put 'custom-hidden-properties intangible t)

(if (file-readable-p custom-file)
    (load-file custom-file))

(provide 'custom)

;;; custom.el ends here
