;;; custom.el --- tools for declaring and initializing options
;;
;; Copyright (C) 1996, 1997, 1999, 2001, 2002 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: FSF
;; Keywords: help, faces

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
;;
;; This file only contain the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `cus-edit.el' and is documented in the Emacs Lisp Reference manual.

;; The code implementing face declarations is in `cus-face.el'

;;; Code:

(require 'widget)

(defvar custom-define-hook nil
  ;; Customize information for this option is in `cus-edit.el'.
  "Hook called after defining each customize option.")

(defvar custom-current-group-alist nil
  "Alist of (FILE . GROUP) indicating the current group to use for FILE.")

;;; The `defcustom' Macro.

(defun custom-initialize-default (symbol value)
  "Initialize SYMBOL with VALUE.
This will do nothing if symbol already has a default binding.
Otherwise, if symbol has a `saved-value' property, it will evaluate
the car of that and used as the default binding for symbol.
Otherwise, VALUE will be evaluated and used as the default binding for
symbol."
  (unless (default-boundp symbol)
    ;; Use the saved value if it exists, otherwise the standard setting.
    (set-default symbol (if (get symbol 'saved-value)
			    (eval (car (get symbol 'saved-value)))
			  (eval value)))))

(defun custom-initialize-set (symbol value)
  "Initialize SYMBOL based on VALUE.
If the symbol doesn't have a default binding already,
then set it using its `:set' function (or `set-default' if it has none).
The value is either the value in the symbol's `saved-value' property,
if any, or VALUE."
  (unless (default-boundp symbol)
    (funcall (or (get symbol 'custom-set) 'set-default)
	     symbol
	     (if (get symbol 'saved-value)
		 (eval (car (get symbol 'saved-value)))
	       (eval value)))))

(defun custom-initialize-reset (symbol value)
  "Initialize SYMBOL based on VALUE.
Set the symbol, using its `:set' function (or `set-default' if it has none).
The value is either the symbol's current value
 \(as obtained using the `:get' function), if any,
or the value in the symbol's `saved-value' property if any,
or (last of all) VALUE."
    (funcall (or (get symbol 'custom-set) 'set-default)
	     symbol
	     (cond ((default-boundp symbol)
		    (funcall (or (get symbol 'custom-get) 'default-value)
			     symbol))
		   ((get symbol 'saved-value)
		    (eval (car (get symbol 'saved-value))))
		   (t
		    (eval value)))))

(defun custom-initialize-changed (symbol value)
  "Initialize SYMBOL with VALUE.
Like `custom-initialize-reset', but only use the `:set' function if
not using the standard setting.
For the standard setting, use `set-default'."
  (cond ((default-boundp symbol)
	 (funcall (or (get symbol 'custom-set) 'set-default)
		  symbol
		  (funcall (or (get symbol 'custom-get) 'default-value)
			   symbol)))
	((get symbol 'saved-value)
	 (funcall (or (get symbol 'custom-set) 'set-default)
		  symbol
		  (eval (car (get symbol 'saved-value)))))
	(t
	 (set-default symbol (eval value)))))

(defun custom-declare-variable (symbol default doc &rest args)
  "Like `defcustom', but SYMBOL and DEFAULT are evaluated as normal arguments.
DEFAULT should be an expression to evaluate to compute the default value,
not the default value itself."
  ;; Remember the standard setting.
  (put symbol 'standard-value (list default))
  ;; Maybe this option was rogue in an earlier version.  It no longer is.
  (when (get symbol 'force-value)
    (put symbol 'force-value nil))
  (when doc
    (put symbol 'variable-documentation doc))
  (let ((initialize 'custom-initialize-reset)
	(requests nil))
    (unless (memq :group args)
      (custom-add-to-group (custom-current-group) symbol 'custom-variable))
    (while args
      (let ((arg (car args)))
	(setq args (cdr args))
	(unless (symbolp arg)
	  (error "Junk in args %S" args))
	(let ((keyword arg)
	      (value (car args)))
	  (unless args
	    (error "Keyword %s is missing an argument" keyword))
	  (setq args (cdr args))
	  (cond ((eq keyword :initialize)
		 (setq initialize value))
		((eq keyword :set)
		 (put symbol 'custom-set value))
		((eq keyword :get)
		 (put symbol 'custom-get value))
		((eq keyword :require)
		 (setq requests (cons value requests)))
		((eq keyword :type)
		 (put symbol 'custom-type (purecopy value)))
		((eq keyword :options)
		 (if (get symbol 'custom-options)
		     ;; Slow safe code to avoid duplicates.
		     (mapc (lambda (option)
			     (custom-add-option symbol option))
			     value)
		   ;; Fast code for the common case.
		   (put symbol 'custom-options (copy-sequence value))))
		(t
		 (custom-handle-keyword symbol keyword value
					'custom-variable))))))
    (put symbol 'custom-requests requests)
    ;; Do the actual initialization.
    (funcall initialize symbol default))
  (setq current-load-list (cons symbol current-load-list))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE needs to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]...

The following keywords are meaningful:

:type	VALUE should be a widget type for editing the symbols value.
:options VALUE should be a list of valid members of the widget type.
:group  VALUE should be a customization group.
        Add SYMBOL to that group.
:link LINK-DATA
        Include an external link after the documentation string for this
        item.  This is a sentence containing an active field which
        references some other documentation.
   
        There are three alternatives you can use for LINK-DATA:
   
        (custom-manual INFO-NODE)
             Link to an Info node; INFO-NODE is a string which specifies
             the node name, as in \"(emacs)Top\".  The link appears as
             `[manual]' in the customization buffer.
   
        (info-link INFO-NODE)
             Like `custom-manual' except that the link appears in the
             customization buffer with the Info node name.
   
        (url-link URL)
             Link to a web page; URL is a string which specifies the URL.
             The link appears in the customization buffer as URL.
   
        You can specify the text to use in the customization buffer by
        adding `:tag NAME' after the first element of the LINK-DATA; for
        example, (info-link :tag \"foo\" \"(emacs)Top\") makes a link to the
        Emacs manual which appears in the buffer as `foo'.
   
        An item can have more than one external link; however, most items
        have none at all.
:initialize
	VALUE should be a function used to initialize the
	variable.  It takes two arguments, the symbol and value
	given in the `defcustom' call.  The default is
	`custom-initialize-reset'.
:set	VALUE should be a function to set the value of the symbol.
	It takes two arguments, the symbol to set and the value to
	give it.  The default choice of function is `custom-set-default'.
:get	VALUE should be a function to extract the value of symbol.
	The function takes one argument, a symbol, and should return
	the current value for that symbol.  The default choice of function
	is `custom-default-value'.
:require
	VALUE should be a feature symbol.  If you save a value
	for this option, then when your `.emacs' file loads the value,
	it does (require VALUE) first.
:version
        VALUE should be a string specifying that the variable was
        first introduced, or its default value was changed, in Emacs
        version VERSION.
:tag LABEL
        Use LABEL, a string, instead of the item's name, to label the item
        in customization menus and buffers.
:load FILE
        Load file FILE (a string) before displaying this customization
        item.  Loading is done with `load-library', and only if the file is
        not already loaded.
:set-after VARIABLE
	Specifies that SYMBOL should be set after VARIABLE when
	both have been customized.

Read the section about customization in the Emacs Lisp manual for more
information."
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-variable
	       (list 'quote symbol)
	       (list 'quote value)
	       doc)
	 args))

;;; The `defface' Macro.

(defmacro defface (face spec doc &rest args)
  "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

Third argument DOC is the face documentation.

If FACE has been set with `custom-set-face', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORDs are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

The first element of SPEC where the DISPLAY matches the frame
is the one that takes effect in that frame.  The ATTRs in this
element take effect; the other elements are ignored, on that frame.

ATTS is a list of face attributes followed by their values:
  (ATTR VALUE ATTR VALUE...)

The possible attributes are `:family', `:width', `:height', `:weight',
`:slant', `:underline', `:overline', `:strike-through', `:box',
`:foreground', `:background', `:stipple', `:inverse-video', and `:inherit'.

DISPLAY can either be the symbol t, which will match all frames, or an
alist of the form \((REQ ITEM...)...).  For the DISPLAY to match a
FRAME, the REQ property of the frame must match one of the ITEM.  The
following REQ are defined:

`type' (the value of `window-system')
  Under X, in addition to the values `window-system' can take,
  `motif', `lucid' and `x-toolkit' are allowed, and match when
  the Motif toolkit, Lucid toolkit, or any X toolkit is in use.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

Read the section about customization in the Emacs Lisp manual for more
information."
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-face (list 'quote face) spec doc) args))

;;; The `defgroup' Macro.

(defun custom-current-group ()
  (cdr (assoc load-file-name custom-current-group-alist)))

(defun custom-declare-group (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (while members
    (apply 'custom-add-to-group symbol (car members))
    (setq members (cdr members)))
  (put symbol 'custom-group (nconc members (get symbol 'custom-group)))
  (when doc
    ;; This text doesn't get into DOC.
    (put symbol 'group-documentation (purecopy doc)))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(cond ((eq keyword :prefix)
	       (put symbol 'custom-prefix value))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-group))))))
  ;; Record the group on the `current' list.
  (let ((elt (assoc load-file-name custom-current-group-alist)))
    (if elt (setcdr elt symbol)
      (push (cons load-file-name symbol) custom-current-group-alist)))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third arg DOC is the group documentation.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where
NAME is a symbol and WIDGET is a widget for editing that symbol.
Useful widgets are `custom-variable' for editing variables,
`custom-face' for edit faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORDs are defined:

:group   VALUE should be a customization group.
         Add SYMBOL to that group.

:version VALUE should be a string specifying that the group was introduced
         in Emacs version VERSION.

Read the section about customization in the Emacs Lisp manual for more
information."
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-group (list 'quote symbol) members doc) args))

(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for OPTION and WIDGET, nothing is done."
  (let ((members (get group 'custom-group))
	(entry (list option widget)))
    (unless (member entry members)
      (put group 'custom-group (nconc members (list entry))))))

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
  (unless (memq :group args)
    (custom-add-to-group (custom-current-group) symbol 'custom-face))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(custom-handle-keyword symbol keyword value type)))))

(defun custom-handle-keyword (symbol keyword value type)
  "For customization option SYMBOL, handle KEYWORD with VALUE.
Fourth argument TYPE is the custom option type."
  (if purify-flag
      (setq value (purecopy value)))
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :version)
	 (custom-add-version symbol value))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
	((eq keyword :set-after)
	 (custom-add-dependencies symbol value))
	(t
	 (error "Unknown keyword %s" keyword))))

(defun custom-add-dependencies (symbol value)
  "To the custom option SYMBOL, add dependencies specified by VALUE.
VALUE should be a list of symbols.  For each symbol in that list,
this specifies that SYMBOL should be set after the specified symbol, if
both appear in constructs like `custom-set-variables'."
  (unless (listp value)
    (error "Invalid custom dependency `%s'" value))
  (let* ((deps (get symbol 'custom-dependencies))
	 (new-deps deps))
    (while value
      (let ((dep (car value)))
	(unless (symbolp dep)
	  (error "Invalid custom dependency `%s'" dep))
	(unless (memq dep new-deps)
	  (setq new-deps (cons dep new-deps)))
	(setq value (cdr value))))
    (unless (eq deps new-deps)
      (put symbol 'custom-dependencies new-deps))))
  
(defun custom-add-option (symbol option)
  "To the variable SYMBOL add OPTION.

If SYMBOL is a hook variable, OPTION should be a hook member.
For other types variables, the effect is undefined."
  (let ((options (get symbol 'custom-options)))
    (unless (member option options)
      (put symbol 'custom-options (cons option options)))))

(defun custom-add-link (symbol widget)
  "To the custom option SYMBOL add the link WIDGET."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (cons (purecopy widget) links)))))

(defun custom-add-version (symbol version)
  "To the custom option SYMBOL add the version VERSION."
  (put symbol 'custom-version (purecopy version)))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons (purecopy load) loads)))))

;;; Loading files needed to customize a symbol.
;;; This is in custom.el because menu-bar.el needs it for toggle cmds.

(defvar custom-load-recursion nil
  "Hack to avoid recursive dependencies.")

(defun custom-load-symbol (symbol)
  "Load all dependencies for SYMBOL."
  (unless custom-load-recursion
    (let ((custom-load-recursion t)
	  (loads (get symbol 'custom-loads))
	  load)
      (while loads
	(setq load (car loads)
	      loads (cdr loads))
	(cond ((symbolp load)
	       (condition-case nil
		   (require load)
		 (error nil)))
	      ;; Don't reload a file already loaded.
	      ((and (boundp 'preloaded-file-list)
		    (member load preloaded-file-list)))
	      ((assoc load load-history))
	      ;; This was just (assoc (locate-library load) load-history)
	      ;; but has been optimized not to load locate-library
	      ;; if not necessary.
	      ((let (found (regexp (regexp-quote load)))
		 (dolist (loaded load-history)
		   (and (string-match regexp (car loaded))
			(eq (locate-library load) (car loaded))
			(setq found t)))
		 found))
	      ;; Without this, we would load cus-edit recursively.
	      ;; We are still loading it when we call this,
	      ;; and it is not in load-history yet.
	      ((equal load "cus-edit"))
	      (t
	       (condition-case nil
		   (load-library load)
		 (error nil))))))))

;;; Initializing.

(defvar custom-local-buffer nil
  "Non-nil, in a Customization buffer, means customize a specific buffer.
If this variable is non-nil, it should be a buffer,
and it means customize the local bindings of that buffer.
This variable is a permanent local, and it normally has a local binding
in every Customization buffer.")
(put 'custom-local-buffer 'permanent-local t)

(defun custom-set-variables (&rest args)
  "Initialize variables according to user preferences.

The arguments should be a list where each entry has the form:

  (SYMBOL VALUE [NOW [REQUEST [COMMENT]]])

The unevaluated VALUE is stored as the saved value for SYMBOL.
If NOW is present and non-nil, VALUE is also evaluated and bound as
the default value for the SYMBOL.
REQUEST is a list of features we must require for SYMBOL.
COMMENT is a comment string about SYMBOL."
  (setq args
	(sort args
	      (lambda (a1 a2)
		(let* ((sym1 (car a1))
		       (sym2 (car a2))
		       (1-then-2 (memq sym1 (get sym2 'custom-dependencies)))
		       (2-then-1 (memq sym2 (get sym1 'custom-dependencies))))
		  (cond ((and 1-then-2 2-then-1)
			 (error "Circular custom dependency between `%s' and `%s'"
				sym1 sym2))
			(1-then-2 t)
			(2-then-1 nil)
			;; Put symbols with :require last.  The macro
			;; define-minor-mode generates a defcustom
			;; with a :require and a :set, where the
			;; setter function calls the mode function.
			;; Putting symbols with :require last ensures
			;; that the mode function will see other
			;; customized values rather than default
			;; values.
			(t (nth 3 a2)))))))
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (now (nth 2 entry))
		 (requests (nth 3 entry))
		 (comment (nth 4 entry))
		 set)
	    (when requests
	      (put symbol 'custom-requests requests)
	      (mapc 'require requests))
	    (setq set (or (get symbol 'custom-set) 'custom-set-default))
	    (put symbol 'saved-value (list value))
	    (put symbol 'saved-variable-comment comment)
	    ;; Allow for errors in the case where the setter has
	    ;; changed between versions, say, but let the user know.
	    (condition-case data
		(cond (now
		       ;; Rogue variable, set it now.
		       (put symbol 'force-value t)
		       (funcall set symbol (eval value)))
		      ((default-boundp symbol)
		       ;; Something already set this, overwrite it.
		       (funcall set symbol (eval value))))
	      (error 
	       (message "Error setting %s: %s" symbol data)))
	    (setq args (cdr args))
	    (and (or now (default-boundp symbol))
		 (put symbol 'variable-comment comment)))
	;; Old format, a plist of SYMBOL VALUE pairs.
	(message "Warning: old format `custom-set-variables'")
	(ding)
	(sit-for 2)
	(let ((symbol (nth 0 args))
	      (value (nth 1 args)))
	  (put symbol 'saved-value (list value)))
	(setq args (cdr (cdr args)))))))

(defun custom-set-default (variable value)
  "Default :set function for a customizable variable.
Normally, this sets the default value of VARIABLE to VALUE,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(set variable value))
    (set-default variable value)))

(defun custom-quote (sexp)
  "Quote SEXP iff it is not self quoting."
  (if (or (memq sexp '(t nil))
	  (keywordp sexp)
	  (and (listp sexp)
	       (memq (car sexp) '(lambda)))
	  (stringp sexp)
	  (numberp sexp)
	  (vectorp sexp)
;;;  	  (and (fboundp 'characterp)
;;;  	       (characterp sexp))
	  )
      sexp
    (list 'quote sexp)))

(defun customize-mark-to-save (symbol)
  "Mark SYMBOL for later saving.

If the default value of SYMBOL is different from the standard value, 
set the `saved-value' property to a list whose car evaluates to the
default value. Otherwise, set it til nil.

To actually save the value, call `custom-save-all'.

Return non-nil iff the `saved-value' property actually changed."
  (let* ((get (or (get symbol 'custom-get) 'default-value))
	 (value (funcall get symbol))
	 (saved (get symbol 'saved-value))
	 (standard (get symbol 'standard-value))
	 (comment (get symbol 'customized-variable-comment)))
    ;; Save default value iff different from standard value.
    (if (or (null standard)
	    (not (equal value (condition-case nil
				  (eval (car standard))
				(error nil)))))
	(put symbol 'saved-value (list (custom-quote value)))
      (put symbol 'saved-value nil))
    ;; Clear customized information (set, but not saved).
    (put symbol 'customized-value nil)
    ;; Save any comment that might have been set.
    (when comment
      (put symbol 'saved-variable-comment comment))
    (not (equal saved (get symbol 'saved-value)))))

(defun customize-mark-as-set (symbol)
  "Mark current value of SYMBOL as being set from customize.

If the default value of SYMBOL is different from the saved value if any, 
or else if it is different from the standard value, set the
`customized-value' property to a list whose car evaluates to the 
default value. Otherwise, set it til nil.

Return non-nil iff the `customized-value' property actually changed."
  (let* ((get (or (get symbol 'custom-get) 'default-value))
	 (value (funcall get symbol))
	 (customized (get symbol 'customized-value))
	 (old (or (get symbol 'saved-value) (get symbol 'standard-value))))
    ;; Mark default value as set iff different from old value.
    (if (or (null old)
	    (not (equal value (condition-case nil 
				  (eval (car old))
				(error nil)))))
	(put symbol 'customized-value (list (custom-quote value)))
      (put symbol 'customized-value nil))
    ;; Changed?
    (not (equal customized (get symbol 'customized-value)))))

;;; The End.

;; Process the defcustoms for variables loaded before this file.
(while custom-declare-variable-list
  (apply 'custom-declare-variable (car custom-declare-variable-list))
  (setq custom-declare-variable-list (cdr custom-declare-variable-list)))

(provide 'custom)

;;; custom.el ends here
