;;; custom.el --- tools for declaring and initializing options
;;
;; Copyright (C) 1996, 1997, 1999, 2001, 2002, 2004, 2005
;;  Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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

DEFAULT is stored as SYMBOL's value in the standard theme.  See
`custom-known-themes' for a list of known themes.  For backwards
compatibility, DEFAULT is also stored in SYMBOL's property
`standard-value'.  At the same time, SYMBOL's property `force-value' is
set to nil, as the value is no longer rogue."
  ;; Remember the standard setting.  The value should be in the standard
  ;; theme, not in this property.  However, this would require changing
  ;; the C source of defvar and others as well...
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

If FACE has been set with `custom-set-faces', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORDs are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

In the first element, DISPLAY can be :default.  The ATTS in that
element then act as defaults for all the following elements.

Aside from that, DISPLAY specifies conditions to match some or
all frames.  For each frame, the first element of SPEC where the
DISPLAY conditions are satisfied is the one that applies to that
frame.  The ATTRs in this element take effect, and the following
elements are ignored, on that frame.

In the last element, DISPLAY can be t.  That element applies to a
frame if none of the previous elements (except the :default if
any) did.

ATTS is a list of face attributes followed by their values:
  (ATTR VALUE ATTR VALUE...)

The possible attributes are `:family', `:width', `:height', `:weight',
`:slant', `:underline', `:overline', `:strike-through', `:box',
`:foreground', `:background', `:stipple', `:inverse-video', and `:inherit'.

DISPLAY can be `:default' (only in the first element), the symbol
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

(defun custom-autoload (symbol load)
  "Mark SYMBOL as autoloaded custom variable and add dependency LOAD."
  (put symbol 'custom-autoload t)
  (custom-add-load symbol load))

;; This test is also in the C code of `user-variable-p'.
(defun custom-variable-p (variable)
  "Return non-nil if VARIABLE is a custom variable."
  (or (get variable 'standard-value)
      (get variable 'custom-autoload)))

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

(defvar custom-known-themes '(user standard)
   "Themes that have been defined with `deftheme'.
The default value is the list (user standard).  The theme `standard'
contains the Emacs standard settings from the original Lisp files.  The
theme `user' contains all the the settings the user customized and saved.
Additional themes declared with the `deftheme' macro will be added to
the front of this list.")

(defun custom-declare-theme (theme feature &optional doc &rest args)
  "Like `deftheme', but THEME is evaluated as a normal argument.
FEATURE is the feature this theme provides.  This symbol is created
from THEME by `custom-make-theme-feature'."
  (add-to-list 'custom-known-themes theme)
  (put theme 'theme-feature feature)
  (when doc
    (put theme 'theme-documentation doc))
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
	(cond ((eq keyword :short-description)
	       (put theme 'theme-short-description value))
	      ((eq keyword :immediate)
	       (put theme 'theme-immediate value))
	      ((eq keyword :variable-set-string)
	       (put theme 'theme-variable-set-string value))
	      ((eq keyword :variable-reset-string)
	       (put theme 'theme-variable-reset-string value))
	      ((eq keyword :face-set-string)
	       (put theme 'theme-face-set-string value))
	      ((eq keyword :face-reset-string)
	       (put theme 'theme-face-reset-string value)))))))

(defmacro deftheme (theme &optional doc &rest args)
  "Declare custom theme THEME.
The optional argument DOC is a doc string describing the theme.
The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORD's are defined:

:short-description
	VALUE is a short (one line) description of the theme.  If not
	given, DOC is used.
:immediate
	If VALUE is non-nil, variables specified in this theme are set
	immediately when loading the theme.
:variable-set-string
	VALUE is a string used to indicate that a variable takes its
	setting from this theme.  It is passed to FORMAT with the name
	of the theme as an additional argument.  If not given, a
	generic description is used.
:variable-reset-string
	VALUE is a string used in the case a variable has been forced
	to its value in this theme.  It is passed to FORMAT with the
	name of the theme as an additional argument.  If not given, a
	generic description is used.
:face-set-string
	VALUE is a string used to indicate that a face takes its
	setting from this theme.  It is passed to FORMAT with the name
	of the theme as an additional argument.  If not given, a
	generic description is used.
:face-reset-string
	VALUE is a string used in the case a face has been forced to
	its value in this theme.  It is passed to FORMAT with the name
	of the theme as an additional argument.  If not given, a
	generic description is used.

Any theme `foo' should be defined in a file called `foo-theme.el';
see `custom-make-theme-feature' for more information."
  (let ((feature (custom-make-theme-feature theme)))
    ;; It is better not to use backquote in this file,
    ;; because that makes a bootstrapping problem
    ;; if you need to recompile all the Lisp files using interpreted code.
    (nconc (list 'custom-declare-theme
		 (list 'quote theme)
		 (list 'quote feature)
		 doc) args)))

(defun custom-make-theme-feature (theme)
  "Given a symbol THEME, create a new symbol by appending \"-theme\".
Store this symbol in the `theme-feature' property of THEME.
Calling `provide-theme' to provide THEME actually puts `THEME-theme'
into `features'.

This allows for a file-name convention for autoloading themes:
Every theme X has a property `provide-theme' whose value is \"X-theme\".
\(require-theme X) then attempts to load the file `X-theme.el'."
  (intern (concat (symbol-name theme) "-theme")))

(defsubst custom-theme-p (theme)
  "Non-nil when THEME has been defined."
  (memq theme custom-known-themes))

(defsubst custom-check-theme (theme)
  "Check whether THEME is valid, and signal an error if it is not."
  (unless (custom-theme-p theme)
    (error "Unknown theme `%s'" theme)))

;;; Initializing.

(defun custom-push-theme (prop symbol theme mode value)
  "Add (THEME MODE VALUE) to the list in property PROP of SYMBOL.
If the first element in that list is already (THEME ...),
discard it first.

MODE can be either the symbol `set' or the symbol `reset'.  If it is the
symbol `set', then VALUE is the value to use.  If it is the symbol
`reset', then VALUE is the mode to query instead.

In the following example for the variable `goto-address-url-face', the
theme `subtle-hacker' uses the same value for the variable as the theme
`gnome2':

  \((standard set bold)
   \(gnome2 set info-xref)
   \(jonadab set underline)
   \(subtle-hacker reset gnome2))


If a value has been stored for themes A B and C, and a new value
is to be stored for theme C, then the old value of C is discarded.
If a new value is to be stored for theme B, however, the old value
of B is not discarded because B is not the car of the list.

For variables, list property PROP is `theme-value'.
For faces, list property PROP is `theme-face'.
This is used in `custom-do-theme-reset', for example.

The list looks the same in any case; the examples shows a possible
value of the `theme-face' property for the face `region':

  \((gnome2 set ((t (:foreground \"cyan\" :background \"dark cyan\"))))
   \(standard set ((((class color) (background dark))
		   \(:background \"blue\"))
		  \(t (:background \"gray\")))))

This records values for the `standard' and the `gnome2' themes.
The user has not customized the face; had he done that,
the list would contain an entry for the `user' theme, too.
See `custom-known-themes' for a list of known themes."
  (let ((old (get symbol prop)))
    (if (eq (car-safe (car-safe old)) theme)
        (setq old (cdr old)))
    (put symbol prop (cons (list theme mode value) old))))

(defvar custom-local-buffer nil
  "Non-nil, in a Customization buffer, means customize a specific buffer.
If this variable is non-nil, it should be a buffer,
and it means customize the local bindings of that buffer.
This variable is a permanent local, and it normally has a local binding
in every Customization buffer.")
(put 'custom-local-buffer 'permanent-local t)

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

(defun custom-reevaluate-setting (symbol)
  "Reset the value of SYMBOL by re-evaluating its saved or default value.
This is useful for variables that are defined before their default value
can really be computed.  E.g. dumped variables whose default depends on
run-time information."
  (funcall (or (get symbol 'custom-set) 'set-default)
	   symbol
	   (eval (car (or (get symbol 'saved-value) (get symbol 'standard-value))))))

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

Several properties of THEME and SYMBOL are used in the process:

If THEME's property `theme-immediate' is non-nil, this is equivalent of
providing the NOW argument to all symbols in the argument list:
evaluate each EXP and set the corresponding SYMBOL.  However,
there's a difference in the handling of SYMBOL's property
`force-value': if NOW is non-nil, SYMBOL's property `force-value' is set to
the symbol `rogue', else if THEME's property `theme-immediate' is non-nil,
SYMBOL's property `force-value' is set to the symbol `immediate'.

EXP itself is saved unevaluated as SYMBOL property `saved-value' and
in SYMBOL's list property `theme-value' \(using `custom-push-theme')."
  (custom-check-theme theme)
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
	;; Old format, a plist of SYMBOL VALUE pairs.
	(message "Warning: old format `custom-set-variables'")
	(ding)
	(sit-for 2)
	(let ((symbol (indirect-variable (nth 0 args)))
	      (value (nth 1 args)))
	  (put symbol 'saved-value (list value))
	  (custom-push-theme 'theme-value symbol theme 'set value))
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
default value.  Otherwise, set it to nil.

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
default value.  Otherwise, set it to nil.

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

;;; Theme Manipulation

(defvar custom-loaded-themes nil
  "Themes in the order they are loaded.")

(defcustom custom-theme-directory
  (if (eq system-type 'ms-dos)
	 ;; MS-DOS cannot have initial dot.
	 "~/_emacs.d/"
      "~/.emacs.d/")
  "Directory in which Custom theme files should be written.
`require-theme' searches this directory in addition to load-path.
The command `customize-create-theme' writes the files it produces
into this directory."
  :type 'string
  :group 'customize
  :version "22.1")

(defun custom-theme-loaded-p (theme)
  "Return non-nil when THEME has been loaded."
  (memq theme custom-loaded-themes))

(defun provide-theme (theme)
  "Indicate that this file provides THEME.
Add THEME to `custom-loaded-themes' and `provide' whatever
is stored in THEME's property `theme-feature'.

Usually the theme-feature property contains a symbol created
by `custom-make-theme-feature'."
  (custom-check-theme theme)
  (provide (get theme 'theme-feature))
  (setq custom-loaded-themes (nconc (list theme) custom-loaded-themes)))

(defun require-theme (theme)
  "Try to load a theme by requiring its feature.
THEME's feature is stored in THEME's `theme-feature' property.

Usually the `theme-feature' property contains a symbol created
by `custom-make-theme-feature'."
  ;; Note we do no check for validity of the theme here.
  ;; This allows to pull in themes by a file-name convention
  (let ((load-path (if (file-directory-p custom-theme-directory)
		       (cons custom-theme-directory load-path)
		     load-path)))
    (require (or (get theme 'theme-feature)
		 (custom-make-theme-feature theme)))))

(defun custom-remove-theme (spec-alist theme)
  "Delete all elements from SPEC-ALIST whose car is THEME."
  (let ((elt (assoc theme spec-alist)))
    (while elt
	(setq spec-alist (delete elt spec-alist)
	      elt (assoc theme spec-alist))))
  spec-alist)

(defun custom-do-theme-reset (theme)
  "Undo all settings defined by THEME.

A variable remains unchanged if its property `theme-value' does not
contain a value for THEME.  A face remains unchanged if its property
`theme-face' does not contain a value for THEME.  In either case, all
settings for THEME are removed from the property and the variable or
face is set to the `user' theme.

See `custom-known-themes' for a list of known themes."
  (let (spec-list)
    (mapatoms (lambda (symbol)
		;; This works even if symbol is both a variable and a
		;; face.
                (setq spec-list (get symbol 'theme-value))
                (when spec-list
                  (put symbol 'theme-value (custom-remove-theme spec-list theme))
                  (custom-theme-reset-internal symbol 'user))
                (setq spec-list (get symbol 'theme-face))
                (when spec-list
                  (put symbol 'theme-face (custom-remove-theme spec-list theme))
                  (custom-theme-reset-internal-face symbol 'user))))))

(defun custom-theme-load-themes (by-theme &rest body)
  "Load the themes specified by BODY.
Record them as required by theme BY-THEME.  BODY is a sequence of either

THEME
	BY-THEME requires THEME
\(reset THEME)
	Undo all the settings made by THEME
\(hidden THEME)
	Require THEME but hide it from the user

All the themes loaded for BY-THEME are recorded in BY-THEME's property
`theme-loads-themes'.  Any theme loaded with the hidden predicate will
be given the property `theme-hidden' unless it has been loaded before.
Whether a theme has been loaded before is determined by the function
`custom-theme-loaded-p'."
  (custom-check-theme by-theme)
  (let ((theme)
	(themes-loaded (get by-theme 'theme-loads-themes)))
    (while theme
      (setq theme (car body)
	    body (cdr body))
      (cond ((and (consp theme) (eq (car theme) 'reset))
	     (custom-do-theme-reset (cadr theme)))
	    ((and (consp theme) (eq (car theme) 'hidden))
	     (require-theme (cadr theme))
	     (unless (custom-theme-loaded-p (cadr theme))
	       (put (cadr theme) 'theme-hidden t)))
	    (t
	     (require-theme theme)
	     (put theme 'theme-hidden nil)))
      (setq themes-loaded (nconc (list theme) themes-loaded)))
    (put by-theme 'theme-loads-themes themes-loaded)))

(defun custom-load-themes (&rest body)
  "Load themes for the USER theme as specified by BODY.

See `custom-theme-load-themes' for more information on BODY."
  (apply 'custom-theme-load-themes 'user body))

; (defsubst copy-upto-last (elt list)
;   "Copy all the elements of the list upto the last occurence of elt"
;   ;; Is it faster to do more work in C than to do less in elisp?
;   (nreverse (cdr (member elt (reverse list)))))

(defun custom-theme-value (theme theme-spec-list)
  "Determine the value for THEME defined by THEME-SPEC-LIST.
Returns a list with the original value if found; nil otherwise.

THEME-SPEC-LIST is an alist with themes as its key.  As new themes are
installed, these are added to the front of THEME-SPEC-LIST.
Each element has the form

  \(THEME MODE VALUE)

MODE is either the symbol `set' or the symbol `reset'.  See
`custom-push-theme' for more information on the format of
THEME-SPEC-LIST."
  ;; Note we do _NOT_ signal an error if the theme is unknown
  ;; it might have gone away without the user knowing.
  (let ((value (cdr (assoc theme theme-spec-list))))
    (if value
        (if (eq (car value) 'set)
            (cdr value)
          (custom-theme-value (cadr value) theme-spec-list)))))

(defun custom-theme-variable-value (variable theme)
  "Return (list value) indicating value of VARIABLE in THEME.
If THEME does not define a value for VARIABLE, return nil.  The value
definitions per theme are stored in VARIABLE's property `theme-value'.
The actual work is done by function `custom-theme-value', which see.
See `custom-push-theme' for more information on how these definitions
are stored."
  (custom-theme-value theme (get variable 'theme-value)))

(defun custom-theme-reset-internal (symbol to-theme)
  "Reset SYMBOL to the value defined by TO-THEME.
If SYMBOL is not defined in TO-THEME, reset SYMBOL to the standard
value.  See `custom-theme-variable-value'.  The standard value is
stored in SYMBOL's property `standard-value'."
  (let ((value (custom-theme-variable-value symbol to-theme))
        was-in-theme)
    (setq was-in-theme value)
    (setq value (or value (get symbol 'standard-value)))
    (when value
      (put symbol 'saved-value was-in-theme)
      (if (or (get 'force-value symbol) (default-boundp symbol))
          (funcall (or (get symbol 'custom-set) 'set-default) symbol
                   (eval (car value)))))
    value))

(defun custom-theme-reset-variables (theme &rest args)
  "Reset the value of the variables to values previously defined.
Associate this setting with THEME.

ARGS is a list of lists of the form

    (VARIABLE TO-THEME)

This means reset VARIABLE to its value in TO-THEME."
  (custom-check-theme theme)
  (mapcar '(lambda (arg)
	     (apply 'custom-theme-reset-internal arg)
	     (custom-push-theme 'theme-value (car arg) theme 'reset (cadr arg)))
	  args))

(defun custom-reset-variables (&rest args)
    "Reset the value of the variables to values previously saved.
This is the setting associated the `user' theme.

ARGS is a list of lists of the form

    (VARIABLE TO-THEME)

This means reset VARIABLE to its value in TO-THEME."
    (apply 'custom-theme-reset-variables 'user args))

;;; The End.

;; Process the defcustoms for variables loaded before this file.
(while custom-declare-variable-list
  (apply 'custom-declare-variable (car custom-declare-variable-list))
  (setq custom-declare-variable-list (cdr custom-declare-variable-list)))

(provide 'custom)

;; arch-tag: 041b6116-aabe-4f9a-902d-74092bc3dab2
;;; custom.el ends here
