;;; cc-styles.el --- support for styles in CC Mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-langs)
(cc-require 'cc-vars)
(cc-require 'cc-align)
;; cc-align is only indirectly required: Styles added with
;; `c-add-style' often contains references to functions defined there.

;; Silence the compiler.
(cc-bytecomp-defvar adaptive-fill-first-line-regexp) ; Emacs


;; Warning: don't eval-defun this constant or you'll break style inheritance.
(defconst c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 (inline-open . 0)
			 (brace-list-open . +)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")
     )
    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("bsd"
     (c-basic-offset . 8)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +)
			 (inline-open . 0)
			 (inexpr-class . 0)
			 ))
     )
    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((knr-argdecl-intro . +)
			 (label . 0)
			 (statement-cont . +)
			 (substatement-open . +)
			 (substatement-label . +)
			 (block-open . +)
			 (statement-block-intro . c-lineup-whitesmith-in-block)
			 (block-close . c-lineup-whitesmith-in-block)
			 (inline-open . +)
			 (defun-open . +)
			 (defun-block-intro . c-lineup-whitesmith-in-block)
			 (defun-close . c-lineup-whitesmith-in-block)
			 (brace-list-open . +)
			 (brace-list-intro . c-lineup-whitesmith-in-block)
			 (brace-entry-open . c-indent-multi-line-block)
			 (brace-list-close . c-lineup-whitesmith-in-block)
			 (class-open . +)
			 (inclass . c-lineup-whitesmith-in-block)
			 (class-close . +)
			 (inexpr-class . 0)
			 (extern-lang-open . +)
			 (inextern-lang . c-lineup-whitesmith-in-block)
			 (extern-lang-close . +)
			 (namespace-open . +)
			 (innamespace . c-lineup-whitesmith-in-block)
			 (namespace-close . +)
			 ))
     )
    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)))
     (c-offsets-alist . ((topmost-intro        . 0)
			 (substatement         . +)
			 (substatement-open    . 0)
                         (case-label           . +)
                         (access-label         . -)
                         (inclass              . ++)
                         (inline-open          . 0)
                         ))
     )
    ("linux"
     (c-basic-offset  . 8)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)))
     (c-cleanup-list . (brace-else-brace))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro     . 0)
			 (substatement-open     . 0)
			 (substatement-label    . 0)
			 (label                 . 0)
			 (statement-cont        . +)
			 ))
     )
    ("python"
     (indent-tabs-mode . t)
     (fill-column      . 78)
     (c-basic-offset   . 8)
     (c-offsets-alist  . ((substatement-open . 0)
			  (inextern-lang . 0)
			  (arglist-intro . +)
			  (knr-argdecl-intro . +)
			  ))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)
				))
     (c-block-comment-prefix . "")
     )
    ("java"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     ;; the following preserves Javadoc starter lines
     (c-offsets-alist . ((inline-open . 0)
			 (topmost-intro-cont    . +)
			 (statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
			 (substatement-open     . +)
			 (substatement-label    . +)
 			 (label                 . +)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro  . c-lineup-arglist-intro-after-paren)
 			 (arglist-close  . c-lineup-arglist)
 			 (access-label   . 0)
			 (inher-cont     . c-lineup-java-inher)
			 (func-decl-cont . c-lineup-java-throws)
			 ))
     )
    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING [BASE-STYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.

Optional BASE-STYLE if present, is a string and must follow
STYLE-STRING.  BASE-STYLE names a style that this style inherits from.
By default, all styles inherit from the \"user\" style, which is
computed at run time.  Style loops generate errors.

Two variables are treated specially.  When VARIABLE is
`c-offsets-alist', the VALUE is a list containing elements of the
form:

  (SYNTACTIC-SYMBOL . OFFSET)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

When VARIABLE is `c-special-indent-hook', its VALUE is added to
`c-special-indent-hook' using `add-hook'.  If VALUE is a list, each
element of the list is added with `add-hook'.

Do not change this variable directly.  Use the function `c-add-style'
to add new styles or modify existing styles (it is not a good idea to
modify existing styles -- you should create a new style that inherits
the existing style.")


;; Functions that manipulate styles
(defun c-set-style-1 (conscell dont-override)
  ;; Set the style for one variable
  (let ((attr (car conscell))
	(val  (cdr conscell)))
    (cond
     ;; first special variable
     ((eq attr 'c-offsets-alist)
      (let ((offsets (cond ((eq dont-override t)
			    c-offsets-alist)
			   (dont-override
			    (default-value 'c-offsets-alist)))))
	(mapcar (lambda (langentry)
		  (let ((langelem (car langentry))
			(offset (cdr langentry)))
		    (unless (assq langelem offsets)
		      (c-set-offset langelem offset))))
		val)))
     ;; second special variable
     ((eq attr 'c-special-indent-hook)
      ;; Maybe we should ignore dont-override here and always add new
      ;; hooks?
      (unless (cond ((eq dont-override t)
		     c-special-indent-hook)
		    (dont-override
		     (default-value 'c-special-indent-hook)))
	(if (listp val)
	    (mapcar (lambda (func)
		      (add-hook 'c-special-indent-hook func t t))
		    val)
	  (add-hook 'c-special-indent-hook val t t))))
     ;; all other variables
     (t (when (or (not dont-override)
		  (not (memq attr c-style-variables))
		  (eq (if (eq dont-override t)
			  (symbol-value attr)
			(default-value attr))
		      'set-from-style))
	  (set attr val)
	  ;; Must update a number of other variables if
	  ;; c-comment-prefix-regexp is set.
	  (if (eq attr 'c-comment-prefix-regexp)
	      (c-setup-paragraph-variables)))))))

(defun c-get-style-variables (style basestyles)
  ;; Return all variables in a style by resolving inheritances.
  (if (not style)
      (copy-alist c-fallback-style)
    (let ((vars (cdr (or (assoc (downcase style) c-style-alist)
			 (assoc (upcase style) c-style-alist)
			 (assoc style c-style-alist)
			 (progn
			   (c-benign-error "Undefined style: %s" style)
			   nil)))))
      (let ((base (and (stringp (car-safe vars))
		       (prog1
			   (downcase (car vars))
			 (setq vars (cdr vars))))))
	(if (memq base basestyles)
	    (c-benign-error "Style loop detected: %s in %s" base basestyles)
	  (nconc (c-get-style-variables base (cons base basestyles))
		 (copy-alist vars)))))))

(defvar c-set-style-history nil)

;;;###autoload
(defun c-set-style (stylename &optional dont-override)
  "Set CC Mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles.

The variable `c-indentation-style' always contains the buffer's current
style name.

If the optional argument DONT-OVERRIDE is t, no style variables that
already have values will be overridden.  I.e. in the case of
`c-offsets-alist', syntactic symbols will only be added, and in the
case of all other style variables, only those set to `set-from-style'
will be reassigned.

If DONT-OVERRIDE is neither nil nor t, only those style variables that
have default (i.e. non-buffer local) values will keep their settings
while the rest will be overridden.  This is useful to avoid overriding
global settings done in ~/.emacs when setting a style from a mode hook
\(providing the style variables are buffer local, which is the
default).

Obviously, setting DONT-OVERRIDE to t is useful mainly when the
initial style is chosen for a CC Mode buffer by a major mode.  Since
that is done internally by CC Mode, it typically won't have any effect
when used elsewhere."
  (interactive
   (list (let ((completion-ignore-case t)
	       (prompt (format "Which %s indentation style? "
			       mode-name)))
	   (condition-case nil
	       ;; The default argument is preferred over
	       ;; initial-contents, but it only exists in Emacs >= 20
	       ;; and XEmacs >= 21.
	       (completing-read prompt c-style-alist nil t nil
				'c-set-style-history
				c-indentation-style)
	     (wrong-number-of-arguments
	      ;; If the call above failed, we fall back to the old way
	      ;; of specifying the default value.
	      (completing-read prompt c-style-alist nil t
			       (cons c-indentation-style 0)
			       'c-set-style-history))))))
  (c-initialize-builtin-style)
  (let ((vars (c-get-style-variables stylename nil)))
    (unless dont-override
      ;; Since we always add to c-special-indent-hook we must reset it
      ;; first, or else the hooks from the preceding style will
      ;; remain.  This is not necessary for c-offsets-alist, since
      ;; c-get-style-variables contains every valid offset type in the
      ;; fallback entry.
      (setq c-special-indent-hook
	    (default-value 'c-special-indent-hook)))
    (mapcar (lambda (elem)
	      (c-set-style-1 elem dont-override))
	    ;; Need to go through the variables backwards when we
	    ;; don't override any settings.
	    (if (eq dont-override t) (nreverse vars) vars)))
  (setq c-indentation-style stylename)
  (c-keep-region-active))

;;;###autoload
(defun c-add-style (style descrip &optional set-p)
  "Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " c-style-alist
				     nil nil nil 'c-set-style-history))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style c-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq c-style-alist (cons (cons style descrip) c-style-alist))))
  (and set-p (c-set-style style)))


(defvar c-read-offset-history nil)

(defun c-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let* ((oldoff  (cdr-safe (or (assq langelem c-offsets-alist)
				(assq langelem (get 'c-offsets-alist
						    'c-stylevar-fallback)))))
	 (symname (symbol-name langelem))
	 (defstr  (format "(default %s): " oldoff))
	 (errmsg  (concat "Offset must be int, func, var, vector, list, "
			  "or [+,-,++,--,*,/] "
			  defstr))
	 (prompt (concat symname " offset " defstr))
	 (keymap (make-sparse-keymap))
	 (minibuffer-completion-table obarray)
	 (minibuffer-completion-predicate 'fboundp)
	 offset input)
    ;; In principle completing-read is used here, but SPC is unbound
    ;; to make it less annoying to enter lists.
    (set-keymap-parent keymap minibuffer-local-completion-map)
    (define-key keymap " " 'self-insert-command)
    (while (not offset)
      (setq input (read-from-minibuffer prompt nil keymap t
					'c-read-offset-history
					(format "%s" oldoff)))
      (if (c-valid-offset input)
	  (setq offset input)
	;; error, but don't signal one, keep trying
	;; to read an input value
	(ding)
	(setq prompt errmsg)))
    offset))

;;;###autoload
(defun c-set-offset (symbol offset &optional ignored)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  The optional argument is not used
and exists only for compatibility reasons."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     #'(lambda (langelem)
			 (cons (format "%s" (car langelem)) nil))
		     (get 'c-offsets-alist 'c-stylevar-fallback))
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (c-guess-basic-syntax))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (cons ic 0))
		    )))
	  (offset (c-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (if (c-valid-offset offset)
      (let ((entry (assq symbol c-offsets-alist)))
	(if entry
	    (setcdr entry offset)
	  (if (assq symbol (get 'c-offsets-alist 'c-stylevar-fallback))
	      (setq c-offsets-alist (cons (cons symbol offset)
					  c-offsets-alist))
	    (c-benign-error "%s is not a valid syntactic symbol" symbol))))
    (c-benign-error "Invalid indentation setting for symbol %s: %s"
		    symbol offset))
  (c-keep-region-active))


(defun c-setup-paragraph-variables ()
  "Fix things up for paragraph recognition and filling inside comments by
incorporating the value of `c-comment-prefix-regexp' in the relevant
variables."
  (setq c-current-comment-prefix
	(if (listp c-comment-prefix-regexp)
	    (cdr-safe (or (assoc major-mode c-comment-prefix-regexp)
			  (assoc 'other c-comment-prefix-regexp)))
	  c-comment-prefix-regexp))
  (let ((comment-line-prefix
	 (concat "[ \t]*\\(" c-current-comment-prefix "\\)[ \t]*")))
    (setq paragraph-start (concat comment-line-prefix
				  (c-lang-var paragraph-start)
				  "\\|"
				  page-delimiter)
	  paragraph-separate (concat comment-line-prefix
				     (c-lang-var paragraph-separate)
				     "\\|"
				     page-delimiter)
	  paragraph-ignore-fill-prefix t
	  adaptive-fill-mode t
	  adaptive-fill-regexp
	  (concat comment-line-prefix
		  (if (default-value 'adaptive-fill-regexp)
		      (concat "\\("
			      (default-value 'adaptive-fill-regexp)
			      "\\)")
		    "")))
    (when (boundp 'adaptive-fill-first-line-regexp)
      ;; XEmacs (20.x) adaptive fill mode doesn't have this.
      (make-local-variable 'adaptive-fill-first-line-regexp)
      (setq adaptive-fill-first-line-regexp
	    (concat "\\`" comment-line-prefix
		    ;; Maybe we should incorporate the old value here,
		    ;; but then we have to do all sorts of kludges to
		    ;; deal with the \` and \' it probably contains.
		    "\\'")))))


;; Helper for setting up Filladapt mode.  It's not used by CC Mode itself.

(cc-bytecomp-defvar filladapt-token-table)
(cc-bytecomp-defvar filladapt-token-match-table)
(cc-bytecomp-defvar filladapt-token-conversion-table)

(defun c-setup-filladapt ()
  "Convenience function to configure Kyle E. Jones' Filladapt mode for
CC Mode by making sure the proper entries are present on
`filladapt-token-table', `filladapt-token-match-table', and
`filladapt-token-conversion-table'.  This is intended to be used on
`c-mode-common-hook' or similar."
  ;; This function is intended to be used explicitly by the end user
  ;; only.
  ;;
  ;; The default configuration already handles C++ comments, but we
  ;; need to add handling of C block comments.  A new filladapt token
  ;; `c-comment' is added for that.
  (let (p)
    (setq p filladapt-token-table)
    (while (and p (not (eq (car-safe (cdr-safe (car-safe p))) 'c-comment)))
      (setq p (cdr-safe p)))
    (if p
	(setcar (car p) c-current-comment-prefix)
      (setq filladapt-token-table
	    (append (list (car filladapt-token-table)
			  (list c-current-comment-prefix 'c-comment))
		    (cdr filladapt-token-table)))))
  (unless (assq 'c-comment filladapt-token-match-table)
    (setq filladapt-token-match-table
	  (append '((c-comment c-comment))
		  filladapt-token-match-table)))
  (unless (assq 'c-comment filladapt-token-conversion-table)
    (setq filladapt-token-conversion-table
	  (append '((c-comment . exact))
		  filladapt-token-conversion-table))))


(defun c-initialize-builtin-style ()
  ;; Dynamically append the default value of most variables. This is
  ;; crucial because future c-set-style calls will always reset the
  ;; variables first to the `cc-mode' style before instituting the new
  ;; style.  Only do this once!
  (unless (get 'c-initialize-builtin-style 'is-run)
    (put 'c-initialize-builtin-style 'is-run t)
    ;;(c-initialize-cc-mode)
    (unless (assoc "user" c-style-alist)
      (let ((vars c-style-variables) var val uservars)
	(while vars
	  (setq var (car vars)
		val (symbol-value var)
		vars (cdr vars))
	  (cond ((eq var 'c-offsets-alist)
		 (or (null val)
		     (setq uservars (cons (cons 'c-offsets-alist val)
					  uservars))))
		((not (eq val 'set-from-style))
		 (setq uservars (cons (cons var val)
				      uservars)))))
	(c-add-style "user" uservars)))
    (unless (assoc "cc-mode" c-style-alist)
      (c-add-style "cc-mode" '("user")))
    (if c-style-variables-are-local-p
	(c-make-styles-buffer-local))))

(defun c-make-styles-buffer-local (&optional this-buf-only-p)
  "Make all CC Mode style variables buffer local.
If you edit primarily one style of C (or C++, Objective-C, Java, etc)
code, you probably want style variables to be global.  This is the
default.

If you edit many different styles of C (or C++, Objective-C, Java,
etc) at the same time, you probably want the CC Mode style variables
to be buffer local.  If you do, it's advicable to set any CC Mode
style variables in a hook function (e.g. off of `c-mode-common-hook'),
instead of at the top level of your ~/.emacs file.

This function makes all the CC Mode style variables buffer local.
Call it after CC Mode is loaded into your Emacs environment.
Conversely, set the variable `c-style-variables-are-local-p' to t in
your .emacs file, before CC Mode is loaded, and this function will be
automatically called when CC Mode is loaded.

Optional argument, when non-nil, means use `make-local-variable'
instead of `make-variable-buffer-local'."
  ;; style variables
  (let ((func (if this-buf-only-p
		  'make-local-variable
		'make-variable-buffer-local))
	(varsyms (cons 'c-indentation-style (copy-alist c-style-variables))))
    (delq 'c-special-indent-hook varsyms)
    (mapcar func varsyms)
    ;; Hooks must be handled specially
    (if this-buf-only-p
	(make-local-hook 'c-special-indent-hook)
      (make-variable-buffer-local 'c-special-indent-hook)
      (setq c-style-variables-are-local-p t))
    ))



(cc-provide 'cc-styles)

;;; cc-styles.el ends here
