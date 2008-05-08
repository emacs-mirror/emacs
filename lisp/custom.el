;;; custom.el --- tools for declaring and initializing options
;;
;; Copyright (C) 1996, 1997, 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: FSF
;; Keywords: help, faces

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
;; This file only contains the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `cus-edit.el' and is documented in the Emacs Lisp Reference manual.

;; The code implementing face declarations is in `cus-face.el'.

;;; Code:

(require 'widget)

(defvar custom-define-hook nil
  ;; Customize information for this option is in `cus-edit.el'.
  "Hook called after defining each customize option.")

(defvar custom-dont-initialize nil
  "Non-nil means `defcustom' should not initialize the variable.
That is used for the sake of `custom-make-dependencies'.
Users should not set it.")

(defvar custom-current-group-alist nil
  "Alist of (FILE . GROUP) indicating the current group to use for FILE.")

;;; The `defcustom' Macro.

(defun custom-initialize-default (symbol value)
  "Initialize SYMBOL with VALUE.
This will do nothing if symbol already has a default binding.
Otherwise, if symbol has a `saved-value' property, it will evaluate
the car of that and use it as the default binding for symbol.
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

(defun custom-initialize-safe-set (symbol value)
  "Like `custom-initialize-set', but catches errors.
If an error occurs during initialization, SYMBOL is set to nil
and no error is thrown.  This is meant for use in pre-loaded files
where some variables or functions used to compute VALUE may not yet
be defined.  You can then re-evaluate VALUE in startup.el, for instance
using `custom-reevaluate-setting'."
  (condition-case nil
      (custom-initialize-set symbol value)
    (error (set-default symbol nil))))

(defun custom-initialize-safe-default (symbol value)
  "Like `custom-initialize-default', but catches errors.
If an error occurs during initialization, SYMBOL is set to nil
and no error is thrown.  This is meant for use in pre-loaded files
where some variables or functions used to compute VALUE may not yet
be defined.  You can then re-evaluate VALUE in startup.el, for instance
using `custom-reevaluate-setting'."
  (condition-case nil
      (custom-initialize-default symbol value)
    (error (set-default symbol nil))))

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
not the default value itself.

DEFAULT is stored as SYMBOL's standard value, in SYMBOL's property
`standard-value'.  At the same time, SYMBOL's property `force-value' is
set to nil, as the value is no longer rogue."
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
		 (push value requests))
		((eq keyword :risky)
		 (put symbol 'risky-local-variable value))
		((eq keyword :safe)
		 (put symbol 'safe-local-variable value))
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
    (unless custom-dont-initialize
      (funcall initialize symbol default)))
  (push symbol current-load-list)
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE need to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]...

The following keywords are meaningful:

:type	VALUE should be a widget type for editing the symbol's value.
:options VALUE should be a list of valid members of the widget type.
:initialize
	VALUE should be a function used to initialize the
	variable.  It takes two arguments, the symbol and value
	given in the `defcustom' call.  The default is
	`custom-initialize-reset'.
:set	VALUE should be a function to set the value of the symbol.
	It takes two arguments, the symbol to set and the value to
	give it.  The default choice of function is `set-default'.
:get	VALUE should be a function to extract the value of symbol.
	The function takes one argument, a symbol, and should return
	the current value for that symbol.  The default choice of function
	is `default-value'.
:require
	VALUE should be a feature symbol.  If you save a value
	for this option, then when your `.emacs' file loads the value,
	it does (require VALUE) first.
:risky	Set SYMBOL's `risky-local-variable' property to VALUE.
:safe	Set SYMBOL's `safe-local-variable' property to VALUE.

The following common keywords are also meaningful.

:group  VALUE should be a customization group.
        Add SYMBOL (or FACE with `defface') to that group.
:link LINK-DATA
        Include an external link after the documentation string for this
        item.  This is a sentence containing an active field which
        references some other documentation.

        There are several alternatives you can use for LINK-DATA:

        (custom-manual INFO-NODE)
             Link to an Info node; INFO-NODE is a string which specifies
             the node name, as in \"(emacs)Top\".

        (info-link INFO-NODE)
             Like `custom-manual' except that the link appears in the
             customization buffer with the Info node name.

        (url-link URL)
             Link to a web page; URL is a string which specifies the URL.

        (emacs-commentary-link LIBRARY)
             Link to the commentary section of LIBRARY.

        (emacs-library-link LIBRARY)
             Link to an Emacs Lisp LIBRARY file.

        (file-link FILE)
             Link to FILE.

        (function-link FUNCTION)
             Link to the documentation of FUNCTION.

        (variable-link VARIABLE)
             Link to the documentation of VARIABLE.

        (custom-group-link GROUP)
             Link to another customization GROUP.

        You can specify the text to use in the customization buffer by
        adding `:tag NAME' after the first element of the LINK-DATA; for
        example, (info-link :tag \"foo\" \"(emacs)Top\") makes a link to the
        Emacs manual which appears in the buffer as `foo'.

        An item can have more than one external link; however, most items
        have none at all.
:version
        VALUE should be a string specifying that the variable was
        first introduced, or its default value was changed, in Emacs
        version VERSION.
:package-version
        VALUE should be a list with the form (PACKAGE . VERSION)
        specifying that the variable was first introduced, or its
        default value was changed, in PACKAGE version VERSION.  This
        keyword takes priority over :version.  The PACKAGE and VERSION
        must appear in the alist `customize-package-emacs-version-alist'.
        Since PACKAGE must be unique and the user might see it in an
        error message, a good choice is the official name of the
        package, such as MH-E or Gnus.
:tag LABEL
        Use LABEL, a string, instead of the item's name, to label the item
        in customization menus and buffers.
:load FILE
        Load file FILE (a string) before displaying this customization
        item.  Loading is done with `load', and only if the file is
        not already loaded.
:set-after VARIABLES
	Specifies that SYMBOL should be set after the list of variables
        VARIABLES when both have been customized.

If SYMBOL has a local binding, then this form affects the local
binding.  This is normally not what you want.  Thus, if you need
to load a file defining variables with this form, or with
`defvar' or `defconst', you should always load that file
_outside_ any bindings for these variables.  \(`defvar' and
`defconst' behave similarly in this respect.)

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."
  (declare (doc-string 3))
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

If FACE has been set with `custom-set-faces', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

For a list of valid keywords, see the common keywords listed in
`defcustom'.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

In the first element, DISPLAY can be `default'.  The ATTS in that
element then act as defaults for all the following elements.

Aside from that, DISPLAY specifies conditions to match some or
all frames.  For each frame, the first element of SPEC where the
DISPLAY conditions are satisfied is the one that applies to that
frame.  The ATTRs in this element take effect, and the following
elements are ignored, on that frame.

In the last element, DISPLAY can be t.  That element applies to a
frame if none of the previous elements (except the `default' if
any) did.

ATTS is a list of face attributes followed by their values:
  (ATTR VALUE ATTR VALUE...)

The possible attributes are `:family', `:width', `:height', `:weight',
`:slant', `:underline', `:overline', `:strike-through', `:box',
`:foreground', `:background', `:stipple', `:inverse-video', and `:inherit'.

DISPLAY can be `default' (only in the first element), the symbol
t (only in the last element) to match all frames, or an alist of
conditions of the form \(REQ ITEM...).  For such an alist to
match a frame, each of the conditions must be satisfied, meaning
that the REQ property of the frame must match one of the
corresponding ITEMs.  These are the defined REQ values:

`type' (the value of `window-system')
  Under X, in addition to the values `window-system' can take,
  `motif', `lucid', `gtk' and `x-toolkit' are allowed, and match when
  the Motif toolkit, Lucid toolkit, GTK toolkit or any X toolkit is in use.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

`min-colors' (the minimum number of colors the frame should support)
  Should be an integer, it is compared with the result of
  `display-color-cells'.

`supports' (only match frames that support the specified face attributes)
  Should be a list of face attributes.  See the documentation for
  the function `display-supports-face-attributes-p' for more
  information on exactly how testing is done.

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."
  (declare (doc-string 3))
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

For a list of valid keywords, see the common keywords listed in
`defcustom'.

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."
  (declare (doc-string 3))
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

(defun custom-group-of-mode (mode)
  "Return the custom group corresponding to the major or minor MODE.
If no such group is found, return nil."
  (or (get mode 'custom-mode-group)
      (if (or (get mode 'custom-group)
	      (and (string-match "-mode\\'" (symbol-name mode))
		   (get (setq mode (intern (substring (symbol-name mode)
						      0 (match-beginning 0))))
			'custom-group)))
	  mode)))

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
  (unless (memq :group args)
    (custom-add-to-group (custom-current-group) symbol type))
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
	((eq keyword :package-version)
	 (custom-add-package-version symbol value))
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

If SYMBOL's custom type is a hook, OPTION should be a hook member.
If SYMBOL's custom type is an alist, OPTION specifies a symbol
to offer to the user as a possible key in the alist.
For other custom types, this has no effect."
  (let ((options (get symbol 'custom-options)))
    (unless (member option options)
      (put symbol 'custom-options (cons option options)))))
(defalias 'custom-add-frequent-value 'custom-add-option)

(defun custom-add-link (symbol widget)
  "To the custom option SYMBOL add the link WIDGET."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (cons (purecopy widget) links)))))

(defun custom-add-version (symbol version)
  "To the custom option SYMBOL add the version VERSION."
  (put symbol 'custom-version (purecopy version)))

(defun custom-add-package-version (symbol version)
  "To the custom option SYMBOL add the package version VERSION."
  (put symbol 'custom-package-version (purecopy version)))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons (purecopy load) loads)))))

(defun custom-autoload (symbol load &optional noset)
  "Mark SYMBOL as autoloaded custom variable and add dependency LOAD.
If NOSET is non-nil, don't bother autoloading LOAD when setting the variable."
  (put symbol 'custom-autoload (if noset 'noset t))
  (custom-add-load symbol load))

;; This test is also in the C code of `user-variable-p'.
(defun custom-variable-p (variable)
  "Return non-nil if VARIABLE is a custom variable.
This recursively follows aliases."
  (setq variable (indirect-variable variable))
  (or (get variable 'standard-value)
      (get variable 'custom-autoload)))

(defun custom-note-var-changed (variable)
  "Inform Custom that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option.
The result is that the change is treated as having been made through Custom."
  (put variable 'customized-value (list (custom-quote (eval variable)))))


;;; Custom Themes

;;; Loading files needed to customize a symbol.
;;; This is in custom.el because menu-bar.el needs it for toggle cmds.

(defvar custom-load-recursion nil
  "Hack to avoid recursive dependencies.")

(defun custom-load-symbol (symbol)
  "Load all dependencies for SYMBOL."
  (unless custom-load-recursion
    (let ((custom-load-recursion t))
      ;; Load these files if not already done,
      ;; to make sure we know all the dependencies of SYMBOL.
      (condition-case nil
	  (require 'cus-load)
	(error nil))
      (condition-case nil
	  (require 'cus-start)
	(error nil))
      (dolist (load (get symbol 'custom-loads))
	(cond ((symbolp load) (condition-case nil (require load) (error nil)))
	      ;; This is subsumed by the test below, but it's much faster.
	      ((assoc load load-history))
	      ;; This was just (assoc (locate-library load) load-history)
	      ;; but has been optimized not to load locate-library
	      ;; if not necessary.
	      ((let ((regexp (concat "\\(\\`\\|/\\)" (regexp-quote load)
				     "\\(\\'\\|\\.\\)"))
		     (found nil))
		 (dolist (loaded load-history)
		   (and (stringp (car loaded))
			(string-match regexp (car loaded))
			(setq found t)))
		 found))
	      ;; Without this, we would load cus-edit recursively.
	      ;; We are still loading it when we call this,
	      ;; and it is not in load-history yet.
	      ((equal load "cus-edit"))
	      (t (condition-case nil (load load) (error nil))))))))

(defvar custom-local-buffer nil
  "Non-nil, in a Customization buffer, means customize a specific buffer.
If this variable is non-nil, it should be a buffer,
and it means customize the local bindings of that buffer.
This variable is a permanent local, and it normally has a local binding
in every Customization buffer.")
(put 'custom-local-buffer 'permanent-local t)

(defun custom-set-default (variable value)
  "Default :set function for a customizable variable.
Normally, this sets the default value of VARIABLE to VALUE,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(set variable value))
    (set-default variable value)))

(defun custom-set-minor-mode (variable value)
  ":set function for minor mode variables.
Normally, this sets the default value of VARIABLE to nil if VALUE
is nil and to t otherwise,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(funcall variable (if value 1 0)))
    (funcall variable (if value 1 0))))

(defun custom-quote (sexp)
  "Quote SEXP if it is not self quoting."
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
default value.  Otherwise, set it to nil.

To actually save the value, call `custom-save-all'.

Return non-nil if the `saved-value' property actually changed."
  (custom-load-symbol symbol)
  (let* ((get (or (get symbol 'custom-get) 'default-value))
	 (value (funcall get symbol))
	 (saved (get symbol 'saved-value))
	 (standard (get symbol 'standard-value))
	 (comment (get symbol 'customized-variable-comment)))
    ;; Save default value if different from standard value.
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
default value.  Otherwise, set it to nil.

Return non-nil if the `customized-value' property actually changed."
  (custom-load-symbol symbol)
  (let* ((get (or (get symbol 'custom-get) 'default-value))
	 (value (funcall get symbol))
	 (customized (get symbol 'customized-value))
	 (old (or (get symbol 'saved-value) (get symbol 'standard-value))))
    ;; Mark default value as set if different from old value.
    (if (not (and old
                  (equal value (condition-case nil
                                   (eval (car old))
                                 (error nil)))))
	(progn (put symbol 'customized-value (list (custom-quote value)))
	       (custom-push-theme 'theme-value symbol 'user 'set
				  (custom-quote value)))
      (put symbol 'customized-value nil))
    ;; Changed?
    (not (equal customized (get symbol 'customized-value)))))

(defun custom-reevaluate-setting (symbol)
  "Reset the value of SYMBOL by re-evaluating its saved or standard value.
Use the :set function to do so.  This is useful for customizable options
that are defined before their standard value can really be computed.
E.g. dumped variables whose default depends on run-time information."
  (funcall (or (get symbol 'custom-set) 'set-default)
	   symbol
	   (eval (car (or (get symbol 'saved-value) (get symbol 'standard-value))))))


;;; Custom Themes

;; Custom themes are collections of settings that can be enabled or
;; disabled as a unit.

;; Each Custom theme is defined by a symbol, called the theme name.
;; The `theme-settings' property of the theme name records the
;; variable and face settings of the theme.  This property is a list
;; of elements, each of the form
;;
;;     (PROP SYMBOL THEME VALUE)
;;
;;  - PROP is either `theme-value' or `theme-face'
;;  - SYMBOL is the face or variable name
;;  - THEME is the theme name (redundant, but simplifies the code)
;;  - VALUE is an expression that gives the theme's setting for SYMBOL.
;;
;; The theme name also has a `theme-feature' property, whose value is
;; specified when the theme is defined (see `custom-declare-theme').
;; Usually, this is just a symbol named THEME-theme.  This lets
;; external libraries call (require 'foo-theme).

;; In addition, each symbol (either a variable or a face) affected by
;; an *enabled* theme has a `theme-value' or `theme-face' property,
;; which is a list of elements each of the form
;;
;;     (THEME VALUE)
;;
;; which have the same meanings as in `theme-settings'.
;;
;; The `theme-value' and `theme-face' lists are ordered by decreasing
;; theme precedence.  Thus, the first element is always the one that
;; is in effect.

;; Each theme is stored in a theme file, with filename THEME-theme.el.
;; Loading a theme basically involves calling (load "THEME-theme")
;; This is done by the function `load-theme'.  Loading a theme
;; automatically enables it.
;;
;; When a theme is enabled, the `theme-value' and `theme-face'
;; properties for the affected symbols are set.  When a theme is
;; disabled, its settings are removed from the `theme-value' and
;; `theme-face' properties, but the theme's own `theme-settings'
;; property remains unchanged.

(defvar custom-known-themes '(user changed)
   "Themes that have been defined with `deftheme'.
The default value is the list (user changed).  The theme `changed'
contains the settings before custom themes are applied.  The
theme `user' contains all the settings the user customized and saved.
Additional themes declared with the `deftheme' macro will be added to
the front of this list.")

(defsubst custom-theme-p (theme)
  "Non-nil when THEME has been defined."
  (memq theme custom-known-themes))

(defsubst custom-check-theme (theme)
  "Check whether THEME is valid, and signal an error if it is not."
  (unless (custom-theme-p theme)
    (error "Unknown theme `%s'" theme)))

(defun custom-push-theme (prop symbol theme mode &optional value)
  "Record VALUE for face or variable SYMBOL in custom theme THEME.
PROP is `theme-face' for a face, `theme-value' for a variable.

MODE can be either the symbol `set' or the symbol `reset'.  If it is the
symbol `set', then VALUE is the value to use.  If it is the symbol
`reset', then SYMBOL will be removed from THEME (VALUE is ignored).

See `custom-known-themes' for a list of known themes."
  (unless (memq prop '(theme-value theme-face))
    (error "Unknown theme property"))
  (let* ((old (get symbol prop))
	 (setting (assq theme old))  ; '(theme value)
	 (theme-settings             ; '(prop symbol theme value)
	  (get theme 'theme-settings)))
    (if (eq mode 'reset)
	;; Remove a setting.
	(when setting
	  (let (res)
	    (dolist (theme-setting theme-settings)
	      (if (and (eq (car  theme-setting) prop)
		       (eq (cadr theme-setting) symbol))
		  (setq res theme-setting)))
	    (put theme 'theme-settings (delq res theme-settings)))
	  (put symbol prop (delq setting old)))
      (if setting
	  ;; Alter an existing setting.
	  (let (res)
	    (dolist (theme-setting theme-settings)
	      (if (and (eq (car  theme-setting) prop)
		       (eq (cadr theme-setting) symbol))
		  (setq res theme-setting)))
	    (put theme 'theme-settings
		 (cons (list prop symbol theme value)
		       (delq res theme-settings)))
	    (setcar (cdr setting) value))
	;; Add a new setting.
	;; If the user changed the value outside of Customize, we
	;; first save the current value to a fake theme, `changed'.
	;; This ensures that the user-set value comes back if the
	;; theme is later disabled.
	(if (null old)
	    (if (and (eq prop 'theme-value)
		     (boundp symbol))
		(let ((sv (get symbol 'standard-value)))
		  (unless (and sv
                               (equal (eval (car sv)) (symbol-value symbol)))
                    (setq old (list (list 'changed (symbol-value symbol))))))
	      (if (and (facep symbol)
		       (not (face-spec-match-p symbol (get symbol 'face-defface-spec))))
		  (setq old (list (list 'changed (list
		    (append '(t) (custom-face-attributes-get symbol nil)))))))))
	(put symbol prop (cons (list theme value) old))
	(put theme 'theme-settings
	     (cons (list prop symbol theme value)
		   theme-settings))))))


(defun custom-set-variables (&rest args)
  "Install user customizations of variable values specified in ARGS.
These settings are registered as theme `user'.
The arguments should each be a list of the form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

This stores EXP (without evaluating it) as the saved value for SYMBOL.
If NOW is present and non-nil, then also evaluate EXP and set
the default value for the SYMBOL to the value of EXP.

REQUEST is a list of features we must require in order to
handle SYMBOL properly.
COMMENT is a comment string about SYMBOL."
  (apply 'custom-theme-set-variables 'user args))

(defun custom-theme-set-variables (theme &rest args)
  "Initialize variables for theme THEME according to settings in ARGS.
Each of the arguments in ARGS should be a list of this form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

This stores EXP (without evaluating it) as the saved value for SYMBOL.
If NOW is present and non-nil, then also evaluate EXP and set
the default value for the SYMBOL to the value of EXP.

REQUEST is a list of features we must require in order to
handle SYMBOL properly.
COMMENT is a comment string about SYMBOL.

EXP itself is saved unevaluated as SYMBOL property `saved-value' and
in SYMBOL's list property `theme-value' \(using `custom-push-theme')."
  (custom-check-theme theme)
 
  ;; Process all the needed autoloads before anything else, so that the
  ;; subsequent code has all the info it needs (e.g. which var corresponds
  ;; to a minor mode), regardless of the ordering of the variables.
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry))))
      (unless (or (get symbol 'standard-value)
                  (memq (get symbol 'custom-autoload) '(nil noset)))
        ;; This symbol needs to be autoloaded, even just for a `set'.
        (custom-load-symbol symbol))))

  ;; Move minor modes and variables with explicit requires to the end.
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
			(2-then-1 nil)
			;; 1 is a dependency of 2, so needs to be set first.
			(1-then-2)
			;; Put minor modes and symbols with :require last.
			;; Putting minor modes last ensures that the mode
			;; function will see other customized values rather
			;; than default values.
			(t (or (nth 3 a2)
                               (eq (get sym2 'custom-set)
                                   'custom-set-minor-mode))))))))
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (indirect-variable (nth 0 entry)))
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
	    (custom-push-theme 'theme-value symbol theme 'set value)
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
        ;; I believe this is dead-code, because the `sort' code above would
        ;; have burped before we could get here.  --Stef
	;; Old format, a plist of SYMBOL VALUE pairs.
	(message "Warning: old format `custom-set-variables'")
	(ding)
	(sit-for 2)
	(let ((symbol (indirect-variable (nth 0 args)))
	      (value (nth 1 args)))
	  (put symbol 'saved-value (list value))
	  (custom-push-theme 'theme-value symbol theme 'set value))
	(setq args (cdr (cdr args)))))))


;;; Defining themes.

;; A theme file should be named `THEME-theme.el' (where THEME is the theme
;; name), and found in either `custom-theme-directory' or the load path.
;; It has the following format:
;;
;;   (deftheme THEME
;;     DOCSTRING)
;;
;;   (custom-theme-set-variables
;;    'THEME
;;    [THEME-VARIABLES])
;;
;;   (custom-theme-set-faces
;;    'THEME
;;    [THEME-FACES])
;;
;;   (provide-theme 'THEME)


;; The IGNORED arguments to deftheme come from the XEmacs theme code, where
;; they were used to supply keyword-value pairs like `:immediate',
;; `:variable-reset-string', etc.  We don't use any of these, so ignore them.

(defmacro deftheme (theme &optional doc &rest ignored)
  "Declare THEME to be a Custom theme.
The optional argument DOC is a doc string describing the theme.

Any theme `foo' should be defined in a file called `foo-theme.el';
see `custom-make-theme-feature' for more information."
  (let ((feature (custom-make-theme-feature theme)))
    ;; It is better not to use backquote in this file,
    ;; because that makes a bootstrapping problem
    ;; if you need to recompile all the Lisp files using interpreted code.
    (list 'custom-declare-theme (list 'quote theme) (list 'quote feature) doc)))

(defun custom-declare-theme (theme feature &optional doc &rest ignored)
  "Like `deftheme', but THEME is evaluated as a normal argument.
FEATURE is the feature this theme provides.  Normally, this is a symbol
created from THEME by `custom-make-theme-feature'."
  (if (memq theme '(user changed))
      (error "Custom theme cannot be named %S" theme))
  (add-to-list 'custom-known-themes theme)
  (put theme 'theme-feature feature)
  (when doc (put theme 'theme-documentation doc)))

(defun custom-make-theme-feature (theme)
  "Given a symbol THEME, create a new symbol by appending \"-theme\".
Store this symbol in the `theme-feature' property of THEME.
Calling `provide-theme' to provide THEME actually puts `THEME-theme'
into `features'.

This allows for a file-name convention for autoloading themes:
Every theme X has a property `provide-theme' whose value is \"X-theme\".
\(load-theme X) then attempts to load the file `X-theme.el'."
  (intern (concat (symbol-name theme) "-theme")))

;;; Loading themes.

(defcustom custom-theme-directory
  user-emacs-directory
  "Directory in which Custom theme files should be written.
`load-theme' searches this directory in addition to load-path.
The command `customize-create-theme' writes the files it produces
into this directory."
  :type 'string
  :group 'customize
  :version "22.1")

(defun provide-theme (theme)
  "Indicate that this file provides THEME.
This calls `provide' to provide the feature name stored in THEME's
property `theme-feature' (which is usually a symbol created by
`custom-make-theme-feature')."
  (if (memq theme '(user changed))
      (error "Custom theme cannot be named %S" theme))
  (custom-check-theme theme)
  (provide (get theme 'theme-feature))
  ;; Loading a theme also enables it.
  (push theme custom-enabled-themes)
  ;; `user' must always be the highest-precedence enabled theme.
  ;; Make that remain true.  (This has the effect of making user settings
  ;; override the ones just loaded, too.)
  (let ((custom-enabling-themes t))
    (enable-theme 'user)))

(defun load-theme (theme)
  "Load a theme's settings from its file.
This also enables the theme; use `disable-theme' to disable it."
  ;; Note we do no check for validity of the theme here.
  ;; This allows to pull in themes by a file-name convention
  (interactive "SCustom theme name: ")
  ;; If reloading, clear out the old theme settings.
  (when (custom-theme-p theme)
    (disable-theme theme)
    (put theme 'theme-settings nil)
    (put theme 'theme-feature nil)
    (put theme 'theme-documentation nil))
  (let ((load-path (if (file-directory-p custom-theme-directory)
		       (cons custom-theme-directory load-path)
		     load-path)))
    (load (symbol-name (custom-make-theme-feature theme)))))

;;; Enabling and disabling loaded themes.

(defvar custom-enabling-themes nil)

(defun enable-theme (theme)
  "Reenable all variable and face settings defined by THEME.
The newly enabled theme gets the highest precedence (after `user').
If it is already enabled, just give it highest precedence (after `user').

If THEME does not specify any theme settings, this tries to load
the theme from its theme file, by calling `load-theme'."
  (interactive "SEnable Custom theme: ")
  (if (not (custom-theme-p theme))
      (load-theme theme)
    ;; This could use a bit of optimization -- cyd
    (let ((settings (get theme 'theme-settings)))
      (dolist (s settings)
	(let* ((prop (car s))
	       (symbol (cadr s))
	       (spec-list (get symbol prop)))
	  (put symbol prop (cons (cddr s) (assq-delete-all theme spec-list)))
	  (if (eq prop 'theme-value)
	      (custom-theme-recalc-variable symbol)
	    (custom-theme-recalc-face symbol)))))
    (unless (eq theme 'user)
      (setq custom-enabled-themes
	    (cons theme (delq theme custom-enabled-themes)))
      (unless custom-enabling-themes
	(enable-theme 'user)))))

(defcustom custom-enabled-themes nil
  "List of enabled Custom Themes, highest precedence first.

This does not include the `user' theme, which is set by Customize,
and always takes precedence over other Custom Themes."
  :group 'customize
  :type  '(repeat symbol)
  :set-after '(custom-theme-directory)  ; so we can find the themes
  :set (lambda (symbol themes)
	 ;; Avoid an infinite loop when custom-enabled-themes is
	 ;; defined in a theme (e.g. `user').  Enabling the theme sets
	 ;; custom-enabled-themes, which enables the theme...
	 (unless custom-enabling-themes
	   (let ((custom-enabling-themes t) failures)
	     (setq themes (delq 'user (delete-dups themes)))
	     (if (boundp symbol)
		 (dolist (theme (symbol-value symbol))
		   (if (not (memq theme themes))
		       (disable-theme theme))))
	     (dolist (theme (reverse themes))
	       (condition-case nil
		   (enable-theme theme)
		 (error (progn (push theme failures)
			       (setq themes (delq theme themes))))))
	     (enable-theme 'user)
	     (custom-set-default symbol themes)
	     (if failures
		 (message "Failed to enable themes: %s"
			  (mapconcat 'symbol-name failures " ")))))))

(defsubst custom-theme-enabled-p (theme)
  "Return non-nil if THEME is enabled."
  (memq theme custom-enabled-themes))

(defun disable-theme (theme)
  "Disable all variable and face settings defined by THEME.
See `custom-enabled-themes' for a list of enabled themes."
  (interactive (list (intern
		      (completing-read
		       "Disable Custom theme: "
		       (mapcar 'symbol-name custom-enabled-themes)
		       nil t))))
  (when (custom-theme-enabled-p theme)
    (let ((settings (get theme 'theme-settings)))
      (dolist (s settings)
	(let* ((prop (car s))
	       (symbol (cadr s))
	       (spec-list (get symbol prop)))
	  (put symbol prop (assq-delete-all theme spec-list))
	  (if (eq prop 'theme-value)
	      (custom-theme-recalc-variable symbol)
	    (custom-theme-recalc-face symbol)))))
    (setq custom-enabled-themes
	  (delq theme custom-enabled-themes))))

(defun custom-variable-theme-value (variable)
  "Return (list VALUE) indicating the custom theme value of VARIABLE.
That is to say, it specifies what the value should be according to
currently enabled custom themes.

This function returns nil if no custom theme specifies a value for VARIABLE."
  (let* ((theme-value (get variable 'theme-value)))
    (if theme-value
	(cdr (car theme-value)))))

(defun custom-theme-recalc-variable (variable)
  "Set VARIABLE according to currently enabled custom themes."
  (let ((valspec (custom-variable-theme-value variable)))
    (if valspec
	(put variable 'saved-value valspec)
      (setq valspec (get variable 'standard-value)))
    (if (and valspec
	     (or (get variable 'force-value)
		 (default-boundp variable)))
	(funcall (or (get variable 'custom-set) 'set-default) variable
		 (eval (car valspec))))))

(defun custom-theme-recalc-face (face)
  "Set FACE according to currently enabled custom themes."
  (if (facep face)
      (face-spec-set face
                     (get (or (get face 'face-alias) face)
                          'face-override-spec))))

;;; XEmacs compability functions

;; In XEmacs, when you reset a Custom Theme, you have to specify the
;; theme to reset it to.  We just apply the next available theme, so
;; just ignore the IGNORED arguments.

(defun custom-theme-reset-variables (theme &rest args)
  "Reset some variable settings in THEME to their values in other themes.
Each of the arguments ARGS has this form:

    (VARIABLE IGNORED)

This means reset VARIABLE.  (The argument IGNORED is ignored)."
  (custom-check-theme theme)
  (dolist (arg args)
    (custom-push-theme 'theme-value (car arg) theme 'reset)))

(defun custom-reset-variables (&rest args)
  "Reset the specs of some variables to their values in other themes.
This creates settings in the `user' theme.

Each of the arguments ARGS has this form:

    (VARIABLE IGNORED)

This means reset VARIABLE.  (The argument IGNORED is ignored)."
    (apply 'custom-theme-reset-variables 'user args))

;;; The End.

;; Process the defcustoms for variables loaded before this file.
(while custom-declare-variable-list
  (apply 'custom-declare-variable (car custom-declare-variable-list))
  (setq custom-declare-variable-list (cdr custom-declare-variable-list)))

(provide 'custom)

;; arch-tag: 041b6116-aabe-4f9a-902d-74092bc3dab2
;;; custom.el ends here
