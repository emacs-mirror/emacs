;;; custom.el -- Tools for declaring and initializing options.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.71
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; If you want to use this code, please visit the URL above.
;;
;; This file only contain the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `cus-edit.el'. 

;; The code implementing face declarations is in `cus-face.el'

;;; Code:

(require 'widget)

(define-widget-keywords :prefix :tag :load :link :options :type :group)

;; These autoloads should be deleted when the file is added to Emacs

(unless (fboundp 'load-gc)
  ;; From cus-edit.el
  (autoload 'customize "cus-edit" nil t)
  (autoload 'customize-variable "cus-edit" nil t)
  (autoload 'customize-face "cus-edit" nil t)
  (autoload 'customize-apropos "cus-edit" nil t)
  (autoload 'customize-customized "cus-edit" nil t)
  (autoload 'custom-buffer-create "cus-edit")
  (autoload 'custom-menu-update "cus-edit")
  (autoload 'custom-make-dependencies "cus-edit")
  ;; From cus-face.el
  (autoload 'custom-declare-face "cus-face")
  (autoload 'custom-set-faces "cus-face"))

;;; The `defcustom' Macro.

(defun custom-declare-variable (symbol value doc &rest args)
  "Like `defcustom', but SYMBOL and VALUE are evaluated as normal arguments."
  (unless (and (default-boundp symbol)
	       (not (get symbol 'saved-value)))
    (set-default symbol (if (get symbol 'saved-value)
			    (eval (car (get symbol 'saved-value)))
			  (eval value))))
  (put symbol 'factory-value (list value))
  (when doc
    (put symbol 'variable-documentation doc))
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
	(cond ((eq keyword :type)
	       (put symbol 'custom-type value))
	      ((eq keyword :options)
	       (if (get symbol 'custom-options)
		   ;; Slow safe code to avoid duplicates.
		   (mapcar (lambda (option)
			     (custom-add-option symbol option))
			   value)
		 ;; Fast code for the common case.
		 (put symbol 'custom-options (copy-list value))))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-variable))))))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE needs to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]... 

The following KEYWORD's are defined:

:type	VALUE should be a widget type.
:options VALUE should be a list of valid members of the widget type.
:group  VALUE should be a customization group.  
        Add SYMBOL to that group.

Read the section about customization in the emacs lisp manual for more
information."
  `(eval-and-compile
     (custom-declare-variable (quote ,symbol) (quote ,value) ,doc ,@args)))

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

The following KEYWORD's are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

ATTS is a list of face attributes and their values.  The possible
attributes are defined in the variable `custom-face-attributes'.
Alternatively, ATTS can be a face in which case the attributes of that
face is used.

The ATTS of the first entry in SPEC where the DISPLAY matches the
frame should take effect in that frame.  DISPLAY can either be the
symbol t, which will match all frames, or an alist of the form
\((REQ ITEM...)...)

For the DISPLAY to match a FRAME, the REQ property of the frame must
match one of the ITEM.  The following REQ are defined:

`type' (the value of `window-system')
  Should be one of `x' or `tty'.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

Read the section about customization in the emacs lisp manual for more
information."
  `(custom-declare-face (quote ,face) ,spec ,doc ,@args))

;;; The `defgroup' Macro.

(defun custom-declare-group (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (put symbol 'custom-group (nconc members (get symbol 'custom-group)))
  (when doc
    (put symbol 'group-documentation doc))
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
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third arg DOC is the group documentation.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where
NAME is a symbol and WIDGET is a widget is a widget for editing that
symbol.  Useful widgets are `custom-variable' for editing variables,
`custom-face' for edit faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]... 

The following KEYWORD's are defined:

:group  VALUE should be a customization group.
        Add SYMBOL to that group.

Read the section about customization in the emacs lisp manual for more
information."
  `(custom-declare-group (quote ,symbol) ,members ,doc ,@args))

(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for that option, overwrite it."
  (let* ((members (get group 'custom-group))
	 (old (assq option members)))
    (if old
	(setcar (cdr old) widget)
      (put group 'custom-group (nconc members (list (list option widget)))))))

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
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
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
	(t
	 (error "Unknown keyword %s" symbol))))  

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
      (put symbol 'custom-links (cons widget links)))))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons load loads)))))

;;; Initializing.

(defun custom-set-variables (&rest args)
  "Initialize variables according to user preferences.  

The arguments should be a list where each entry has the form:

  (SYMBOL VALUE [NOW])

The unevaluated VALUE is stored as the saved value for SYMBOL.
If NOW is present and non-nil, VALUE is also evaluated and bound as
the default value for the SYMBOL."
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((symbol (nth 0 entry))
		(value (nth 1 entry))
		(now (nth 2 entry)))
	    (put symbol 'saved-value (list value))
	    (when now 
	      (put symbol 'force-value t)
	      (set-default symbol (eval value)))
	    (setq args (cdr args)))
	;; Old format, a plist of SYMBOL VALUE pairs.
	(let ((symbol (nth 0 args))
	      (value (nth 1 args)))
	  (put symbol 'saved-value (list value)))
	(setq args (cdr (cdr args)))))))

;;; Meta Customization

(defcustom custom-define-hook nil
  "Hook called after defining each customize option."
  :group 'customize
  :type 'hook)

;;; Menu support

(defconst custom-help-menu
  `("Customize"
    ,(if (string-match "XEmacs" emacs-version)
	 '("Emacs" :filter (lambda (&rest junk)
			     (cdr (custom-menu-create 'emacs))))
       ["Update menu..." custom-menu-update t])
    ["Group..." customize t]
    ["Variable..." customize-variable t]
    ["Face..." customize-face t]
    ["Saved..." customize-customized t]
    ["Apropos..." customize-apropos t])
  "Customize menu")

(defun custom-menu-reset ()
  "Reset customize menu."
  (remove-hook 'custom-define-hook 'custom-menu-reset)
  (if (string-match "XEmacs" emacs-version)
      (when (fboundp 'add-submenu)
	(add-submenu '("Options") custom-help-menu))
    (define-key global-map [menu-bar help-menu customize-menu]
      (cons (car custom-help-menu)
	    (easy-menu-create-keymaps (car custom-help-menu)
				      (cdr custom-help-menu))))))

(if (string-match "XEmacs" emacs-version)
    (autoload 'custom-menu-create "cus-edit")
  (custom-menu-reset))

;;; The End.

(provide 'custom)

;; custom.el ends here
