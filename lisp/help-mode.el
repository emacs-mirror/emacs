;;; help-mode.el --- `help-mode' used by *Help* buffers

;; Copyright (C) 1985, 1986, 1993, 1994, 1998, 1999, 2000, 2001
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal

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

;; Defines `help-mode', which is the mode used by *Help* buffers, and
;; associated support machinery, such as adding hyperlinks, etc.,

;;; Code:

(require 'button)
(eval-when-compile (require 'view))

(defvar help-mode-map (make-sparse-keymap)
  "Keymap for help mode.")

(set-keymap-parent help-mode-map button-buffer-map)

(define-key help-mode-map [mouse-2] 'help-follow-mouse)
(define-key help-mode-map "\C-c\C-b" 'help-go-back)
(define-key help-mode-map "\C-c\C-c" 'help-follow)
;; Documentation only, since we use minor-mode-overriding-map-alist.
(define-key help-mode-map "\r" 'help-follow)

(defvar help-xref-stack nil
  "A stack of ways by which to return to help buffers after following xrefs.
Used by `help-follow' and `help-xref-go-back'.
An element looks like (POSITION FUNCTION ARGS...).
To use the element, do (apply FUNCTION ARGS) then goto the point.")
(put 'help-xref-stack 'permanent-local t)
(make-variable-buffer-local 'help-xref-stack)

(defvar help-xref-stack-item nil
  "An item for `help-follow' in this buffer to push onto `help-xref-stack'.
The format is (FUNCTION ARGS...).")
(put 'help-xref-stack-item 'permanent-local t)
(make-variable-buffer-local 'help-xref-stack-item)

(setq-default help-xref-stack nil help-xref-stack-item nil)

(defcustom help-mode-hook nil
  "Hook run by `help-mode'."
  :type 'hook
  :group 'help)

;; Button types used by help

(define-button-type 'help-xref
  'action #'help-button-action)

(defun help-button-action (button)
  "Call BUTTON's help function."
  (help-do-xref (button-start button)
		(button-get button 'help-function)
		(button-get button 'help-args)))

;; These 6 calls to define-button-type were generated in a dolist
;; loop, but that is bad because it means these button types don't
;; have an easily found definition.

(define-button-type 'help-function
  :supertype 'help-xref
  'help-function 'describe-function
  'help-echo (purecopy "mouse-2, RET: describe this function"))

(define-button-type 'help-variable
  :supertype 'help-xref
  'help-function 'describe-variable
  'help-echo (purecopy "mouse-2, RET: describe this variable"))

(define-button-type 'help-face
  :supertype 'help-xref
  'help-function 'describe-face
  'help-echo (purecopy "mouse-2, RET: describe this face"))

(define-button-type 'help-coding-system
  :supertype 'help-xref
  'help-function 'describe-coding-system
  'help-echo (purecopy "mouse-2, RET: describe this coding system"))

(define-button-type 'help-input-method
  :supertype 'help-xref
  'help-function 'describe-input-method
  'help-echo (purecopy "mouse-2, RET: describe this input method"))

(define-button-type 'help-character-set
  :supertype 'help-xref
  'help-function 'describe-character-set
  'help-echo (purecopy "mouse-2, RET: describe this character set"))

;; make some more ideosyncratic button types

(define-button-type 'help-symbol
  :supertype 'help-xref
  'help-function #'help-xref-interned
  'help-echo (purecopy "mouse-2, RET: describe this symbol"))

(define-button-type 'help-back
  :supertype 'help-xref
  'help-function #'help-xref-go-back
  'help-echo (purecopy "mouse-2, RET: go back to previous help buffer"))

(define-button-type 'help-info
  :supertype 'help-xref
  'help-function #'info
  'help-echo (purecopy"mouse-2, RET: read this Info node"))

(define-button-type 'help-customize-variable
  :supertype 'help-xref
  'help-function (lambda (v)
		   (if help-xref-stack
		       (pop help-xref-stack))
		   (customize-variable v))
  'help-echo (purecopy "mouse-2, RET: customize variable"))

(define-button-type 'help-customize-face
  :supertype 'help-xref
  'help-function (lambda (v)
		   (if help-xref-stack
		       (pop help-xref-stack))
		   (customize-face v))
  'help-echo (purecopy "mouse-2, RET: customize face"))

(define-button-type 'help-function-def
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (require 'find-func)
		  ;; Don't use find-function-noselect because it follows
		   ;; aliases (which fails for built-in functions).
		   (let* ((location (find-function-search-for-symbol
				     fun nil file)))
		     (pop-to-buffer (car location))
		     (goto-char (cdr location))))
  'help-echo (purecopy "mouse-2, RET: find function's definition"))

(define-button-type 'help-variable-def
  :supertype 'help-xref
  'help-function (lambda (var &optional file)
		   (let ((location
			  (find-variable-noselect var file)))
		     (pop-to-buffer (car location))
		     (goto-char (cdr location))))
  'help-echo (purecopy"mouse-2, RET: find variable's definition"))


;;;###autoload
(defun help-mode ()
  "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map help-mode-map)
  (setq mode-name "Help")
  (setq major-mode 'help-mode)
  (view-mode)
  (make-local-variable 'view-no-disable-on-exit)
  (setq view-no-disable-on-exit t)
  (run-hooks 'help-mode-hook))

;;;###autoload
(defun help-mode-setup ()
  (help-mode)
  (setq buffer-read-only nil))

;;;###autoload
(defun help-mode-finish ()
  (when (eq major-mode 'help-mode)
    ;; View mode's read-only status of existing *Help* buffer is lost
    ;; by with-output-to-temp-buffer.
    (toggle-read-only 1)
    (help-make-xrefs (current-buffer)))
  (setq view-return-to-alist
	(list (cons (selected-window) help-return-method))))


;;; Grokking cross-reference information in doc strings and
;;; hyperlinking it.

;; This may have some scope for extension and the same or something
;; similar should be done for widget doc strings, which currently use
;; another mechanism.

(defvar help-back-label (purecopy "[back]")
  "Label to use by `help-make-xrefs' for the go-back reference.")

(defconst help-xref-symbol-regexp
  (purecopy (concat "\\(\\<\\(\\(variable\\|option\\)\\|"
		    "\\(function\\|command\\)\\|"
		    "\\(face\\)\\|"
		    "\\(symbol\\)\\|"
		    "\\(source \\(?:code \\)?\\(?:of\\|for\\)\\)\\)\\s-+\\)?"
		    ;; Note starting with word-syntax character:
		    "`\\(\\sw\\(\\sw\\|\\s_\\)+\\)'"))
  "Regexp matching doc string references to symbols.

The words preceding the quoted symbol can be used in doc strings to
distinguish references to variables, functions and symbols.")

(defconst help-xref-mule-regexp nil
  "Regexp matching doc string references to MULE-related keywords.

It is usually nil, and is temporarily bound to an appropriate regexp
when help commands related to multilingual environment (e.g.,
`describe-coding-system') are invoked.")


(defconst help-xref-info-regexp
  (purecopy "\\<[Ii]nfo[ \t\n]+node[ \t\n]+`\\([^']+\\)'")
  "Regexp matching doc string references to an Info node.")

;;;###autoload
(defun help-setup-xref (item interactive-p)
  "Invoked from commands using the \"*Help*\" buffer to install some xref info.

ITEM is a (FUNCTION . ARGS) pair appropriate for recreating the help
buffer after following a reference.  INTERACTIVE-P is non-nil if the
calling command was invoked interactively.  In this case the stack of
items for help buffer \"back\" buttons is cleared.

This should be called very early, before the output buffer is cleared,
because we want to record the \"previous\" position of point so we can
restore it properly when going back."
  (with-current-buffer (help-buffer)
    (if interactive-p
	;; Why do we want to prevent the user from going back ??  -stef
	(setq help-xref-stack nil)
      (when help-xref-stack-item
	(push (cons (point) help-xref-stack-item) help-xref-stack)))
    (setq help-xref-stack-item item)))

(defvar help-xref-following nil
  "Non-nil when following a help cross-reference.")

(defun help-buffer ()
  (buffer-name				;for with-output-to-temp-buffer
   (if help-xref-following
       (current-buffer)
     (get-buffer-create "*Help*"))))

;;;###autoload
(defun help-make-xrefs (&optional buffer)
  "Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`help-xref-symbol-regexp'.

If the variable `help-xref-mule-regexp' is non-nil, find also
cross-reference information related to multilingual environment
\(e.g., coding-systems).  This variable is also used to disambiguate
the type of reference as the same way as `help-xref-symbol-regexp'.

A special reference `back' is made to return back through a stack of
help buffers.  Variable `help-back-label' specifies the text for
that."
  (interactive "b")
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (goto-char (point-min))
    ;; Skip the header-type info, though it might be useful to parse
    ;; it at some stage (e.g. "function in `library'").
    (forward-paragraph)
    (let ((old-modified (buffer-modified-p)))
      (let ((stab (syntax-table))
            (case-fold-search t)
            (inhibit-read-only t))
        (set-syntax-table emacs-lisp-mode-syntax-table)
        ;; The following should probably be abstracted out.
        (unwind-protect
            (progn
              ;; Info references
              (save-excursion
                (while (re-search-forward help-xref-info-regexp nil t)
                  (let ((data (match-string 1)))
		    (save-match-data
		      (unless (string-match "^([^)]+)" data)
			(setq data (concat "(emacs)" data))))
		    (help-xref-button 1 'help-info data))))
	      ;; Mule related keywords.  Do this before trying
	      ;; `help-xref-symbol-regexp' because some of Mule
	      ;; keywords have variable or function definitions.
	      (if help-xref-mule-regexp
		  (save-excursion
		    (while (re-search-forward help-xref-mule-regexp nil t)
		      (let* ((data (match-string 7))
			     (sym (intern-soft data)))
			(cond
			 ((match-string 3) ; coding system
			  (and sym (coding-system-p sym)
			       (help-xref-button 6 'help-coding-system sym)))
			 ((match-string 4) ; input method
			  (and (assoc data input-method-alist)
			       (help-xref-button 7 'help-input-method data)))
			 ((or (match-string 5) (match-string 6)) ; charset
			  (and sym (charsetp sym)
			       (help-xref-button 7 'help-character-set sym)))
			 ((assoc data input-method-alist)
			  (help-xref-button 7 'help-character-set data))
			 ((and sym (coding-system-p sym))
			  (help-xref-button 7 'help-coding-system sym))
			 ((and sym (charsetp sym))
			  (help-xref-button 7 'help-character-set sym)))))))
              ;; Quoted symbols
              (save-excursion
                (while (re-search-forward help-xref-symbol-regexp nil t)
                  (let* ((data (match-string 8))
                         (sym (intern-soft data)))
                    (if sym
                        (cond
                         ((match-string 3) ; `variable' &c
                          (and (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                               (help-xref-button 8 'help-variable sym)))
                         ((match-string 4) ; `function' &c
                          (and (fboundp sym) ; similarly
                               (help-xref-button 8 'help-function sym)))
			 ((match-string 5) ; `face'
			  (and (facep sym)
			       (help-xref-button 8 'help-face sym)))
                         ((match-string 6)) ; nothing for `symbol'
			 ((match-string 7)
;; this used:
;; 			   #'(lambda (arg)
;; 			       (let ((location
;; 				      (find-function-noselect arg)))
;; 				 (pop-to-buffer (car location))
;; 				 (goto-char (cdr location))))
			  (help-xref-button 8 'help-function-def sym))
                         ((and (boundp sym) (fboundp sym))
                          ;; We can't intuit whether to use the
                          ;; variable or function doc -- supply both.
                          (help-xref-button 8 'help-symbol sym))
                         ((boundp sym)
			  (help-xref-button 8 'help-variable sym))
			 ((fboundp sym)
			  (help-xref-button 8 'help-function sym))
			 ((facep sym)
			  (help-xref-button 8 'help-face sym)))))))
              ;; An obvious case of a key substitution:
              (save-excursion
                (while (re-search-forward
			;; Assume command name is only word characters
			;; and dashes to get things like `use M-x foo.'.
                        "\\<M-x\\s-+\\(\\sw\\(\\sw\\|-\\)+\\)" nil t)
                  (let ((sym (intern-soft (match-string 1))))
                    (if (fboundp sym)
                        (help-xref-button 1 'help-function sym)))))
              ;; Look for commands in whole keymap substitutions:
              (save-excursion
		;; Make sure to find the first keymap.
		(goto-char (point-min))
                ;; Find a header and the column at which the command
                ;; name will be found.
                (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                                          nil t)
                  (let ((col (- (match-end 1) (match-beginning 1))))
                    (while
                        ;; Ignore single blank lines in table, but not
                        ;; double ones, which should terminate it.
                        (and (not (looking-at "\n\\s-*\n"))
                             (progn
			       (and (eolp) (forward-line))
			       (end-of-line)
			       (skip-chars-backward "^\t\n")
                               (if (and (>= (current-column) col)
					(looking-at "\\(\\sw\\|-\\)+$"))
                                   (let ((sym (intern-soft (match-string 0))))
                                     (if (fboundp sym)
                                         (help-xref-button 0 'help-function sym))))
			       (zerop (forward-line)))))))))
          (set-syntax-table stab))
	;; Delete extraneous newlines at the end of the docstring
	(goto-char (point-max))
	(while (and (not (bobp)) (bolp))
	  (delete-char -1))
        ;; Make a back-reference in this buffer if appropriate.
        (when help-xref-stack
	  (insert "\n\n")
	  (help-insert-xref-button help-back-label 'help-back
				   (current-buffer))))
      ;; View mode steals RET from us.
      (set (make-local-variable 'minor-mode-overriding-map-alist)
           (list (cons 'view-mode
                       (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map view-mode-map)
                         (define-key map "\r" 'help-follow)
                         map))))
      (set-buffer-modified-p old-modified))))

;;;###autoload
(defun help-xref-button (match-number type &rest args)
  "Make a hyperlink for cross-reference text previously matched.
MATCH-NUMBER is the subexpression of interest in the last matched
regexp.  TYPE is the type of button to use.  Any remaining arguments are
passed to the button's help-function when it is invoked.
See `help-make-xrefs'."
  ;; Don't mung properties we've added specially in some instances.
  (unless (button-at (match-beginning match-number))
    (make-text-button (match-beginning match-number)
		      (match-end match-number)
		      'type type 'help-args args)))

;;;###autoload
(defun help-insert-xref-button (string type &rest args)
  "Insert STRING and make a hyperlink from cross-reference text on it.
TYPE is the type of button to use.  Any remaining arguments are passed
to the button's help-function when it is invoked.
See `help-make-xrefs'."
  (unless (button-at (point))
    (insert-text-button string 'type type 'help-args args)))

;;;###autoload
(defun help-xref-on-pp (from to)
  "Add xrefs for symbols in `pp's output between FROM and TO."
  (let ((ost (syntax-table)))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (set-syntax-table emacs-lisp-mode-syntax-table)
	    (narrow-to-region from to)
	    (goto-char (point-min))
	    (condition-case nil
		(while (not (eobp))
		  (cond
		   ((looking-at "\"") (forward-sexp 1))
		   ((looking-at "#<") (search-forward ">" nil 'move))
		   ((looking-at "\\(\\(\\sw\\|\\s_\\)+\\)")
		    (let* ((sym (intern-soft (match-string 1)))
			   (type (cond ((fboundp sym) 'help-function)
				       ((or (memq sym '(t nil))
					    (keywordp sym))
					nil)
				       ((and sym (boundp sym))
					'help-variable))))
		      (when type (help-xref-button 1 type sym)))
		    (goto-char (match-end 1)))
		   (t (forward-char 1))))
	      (error nil))))
      (set-syntax-table ost))))


;; Additional functions for (re-)creating types of help buffers.
(defun help-xref-interned (symbol)
  "Follow a hyperlink which appeared to be an arbitrary interned SYMBOL.
Both variable, function and face documentation are extracted into a single
help buffer."
  (with-current-buffer (help-buffer)
    ;; Push the previous item on the stack before clobbering the output buffer.
    (help-setup-xref nil nil)
    (let ((facedoc (when (facep symbol)
		     ;; Don't record the current entry in the stack.
		     (setq help-xref-stack-item nil)
		     (describe-face symbol)))
	  (fdoc (when (fboundp symbol)
		  ;; Don't record the current entry in the stack.
		  (setq help-xref-stack-item nil)
		  (describe-function symbol)))
	  (sdoc (when (boundp symbol)
		  ;; Don't record the current entry in the stack.
		  (setq help-xref-stack-item nil)
		  (describe-variable symbol))))
      (cond
       (sdoc
	;; We now have a help buffer on the variable.
	;; Insert the function and face text before it.
	(when (or fdoc facedoc)
	  (goto-char (point-min))
	  (let ((inhibit-read-only t))
	    (when fdoc
	      (insert fdoc "\n\n")
	      (when facedoc
		(insert (make-string 30 ?-) "\n\n" (symbol-name symbol)
			" is also a " "face." "\n\n")))
	    (when facedoc
	      (insert facedoc "\n\n"))
	    (insert (make-string 30 ?-) "\n\n" (symbol-name symbol)
		    " is also a " "variable." "\n\n"))
	  ;; Don't record the `describe-variable' item in the stack.
	  (setq help-xref-stack-item nil)
	  (help-setup-xref (list #'help-xref-interned symbol) nil)))
       (fdoc
	;; We now have a help buffer on the function.
	;; Insert face text before it.
	(when facedoc
	  (goto-char (point-max))
	  (let ((inhibit-read-only t))
	    (insert "\n\n" (make-string 30 ?-) "\n\n" (symbol-name symbol)
		    " is also a " "face." "\n\n" facedoc))
	  ;; Don't record the `describe-function' item in the stack.
	  (setq help-xref-stack-item nil)
	  (help-setup-xref (list #'help-xref-interned symbol) nil)))))))


;;; Navigation/hyperlinking with xrefs

(defun help-follow-mouse (click)
  "Follow the cross-reference that you CLICK on."
  (interactive "e")
  (let* ((start (event-start click))
	 (window (car start))
	 (pos (car (cdr start))))
    (with-current-buffer (window-buffer window)
      (help-follow pos))))

(defun help-xref-go-back (buffer)
  "From BUFFER, go back to previous help buffer text using `help-xref-stack'."
  (let (item position method args)
    (with-current-buffer buffer
      (when help-xref-stack
	(setq item (pop help-xref-stack)
	      ;; Clear the current item so that it won't get pushed
	      ;; by the function we're about to call.  TODO: We could also
	      ;; push it onto a "forward" stack and add a `forw' button.
	      help-xref-stack-item nil
	      position (car item)
	      method (cadr item)
	      args (cddr item))))
    (apply method args)
    (with-current-buffer buffer
      (if (get-buffer-window buffer)
	  (set-window-point (get-buffer-window buffer) position)
	(goto-char position)))))

(defun help-go-back ()
  "Invoke the [back] button (if any) in the Help mode buffer."
  (interactive)
  (let ((back-button (button-at (1- (point-max)))))
    (if back-button
	(button-activate back-button)
      (error "No [back] button"))))

(defun help-do-xref (pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting help-buffer has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following t))
    (apply function args)))

(defun help-follow (&optional pos)
  "Follow cross-reference at POS, defaulting to point.

For the cross-reference format, see `help-make-xrefs'."
  (interactive "d")
  (unless pos
    (setq pos (point)))
  (unless (push-button pos)
    ;; check if the symbol under point is a function or variable
    (let ((sym
	   (intern
	    (save-excursion
	      (goto-char pos) (skip-syntax-backward "w_")
	      (buffer-substring (point)
				(progn (skip-syntax-forward "w_")
				       (point)))))))
      (when (or (boundp sym) (fboundp sym) (facep sym))
	(help-do-xref pos #'help-xref-interned (list sym))))))


(provide 'help-mode)

;;; help-mode.el ends here
