;;; help-mode.el --- `help-mode' used by *Help* buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1986, 1993-1994, 1998-2023 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help, internal
;; Package: emacs

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

;; Defines `help-mode', which is the mode used by *Help* buffers, and
;; associated support machinery, such as adding hyperlinks, etc.,

;;; Code:

(require 'cl-lib)

(defvar-keymap help-mode-map
  :doc "Keymap for Help mode."
  :parent (make-composed-keymap button-buffer-map
                                special-mode-map)
  "n"             #'help-goto-next-page
  "p"             #'help-goto-previous-page
  "l"             #'help-go-back
  "r"             #'help-go-forward
  "C-c C-b"       #'help-go-back
  "C-c C-f"       #'help-go-forward
  "<XF86Back>"    #'help-go-back
  "<XF86Forward>" #'help-go-forward
  "C-c C-c"       #'help-follow-symbol
  "s"             #'help-view-source
  "I"             #'help-goto-lispref-info
  "i"             #'help-goto-info
  "c"             #'help-customize)

(easy-menu-define help-mode-menu help-mode-map
  "Menu for Help mode."
  '("Help-Mode"
    ["Show Help for Symbol" help-follow-symbol
     :help "Show the docs for the symbol at point"]
    ["Previous Topic" help-go-back
     :help "Go back to previous topic in this help buffer"
     :active help-xref-stack]
    ["Next Topic" help-go-forward
     :help "Go back to next topic in this help buffer"
     :active help-xref-forward-stack]
    ["Move to Previous Button" backward-button
     :help "Move to the Previous Button in the help buffer"]
    ["Move to Next Button" forward-button
     :help "Move to the Next Button in the help buffer"]
    ["View Source" help-view-source
     :help "Go to the source file for the current help item"]
    ["Goto Info" help-goto-info
     :help "Go to the info node for the current help item"]
    ["Customize" help-customize
     :help "Customize variable or face"]))

(defun help-mode-context-menu (menu click)
  "Populate MENU with Help mode commands at CLICK."
  (define-key menu [help-mode-separator] menu-bar-separator)
  (let ((easy-menu (make-sparse-keymap "Help-Mode")))
    (easy-menu-define nil easy-menu nil
      '("Help-Mode"
        ["Previous Topic" help-go-back
         :help "Go back to previous topic in this help buffer"
         :active help-xref-stack]
        ["Next Topic" help-go-forward
         :help "Go back to next topic in this help buffer"
         :active help-xref-forward-stack]))
    (dolist (item (reverse (lookup-key easy-menu [menu-bar help-mode])))
      (when (consp item)
        (define-key menu (vector (car item)) (cdr item)))))

  (when (mouse-posn-property (event-start click) 'mouse-face)
    (define-key menu [help-mode-push-button]
      '(menu-item "Follow Link" (lambda (event)
                                  (interactive "e")
                                  (push-button event))
                  :help "Follow the link at click")))

  menu)

(defvar help-mode-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "close" 'quit-window 'quit map
                         :help "Quit help"
                         :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item "search" 'isearch-forward 'search map
                         :help "Search" :vert-only t)
    (tool-bar-local-item-from-menu 'help-go-back "left-arrow" map help-mode-map
                                   :rtl "right-arrow" :vert-only t)
    (tool-bar-local-item-from-menu 'help-go-forward "right-arrow" map help-mode-map
                                   :rtl "left-arrow" :vert-only t)
    map))

(defvar-local help-xref-stack nil
  "A stack of ways by which to return to help buffers after following xrefs.
Used by `help-follow-symbol' and `help-xref-go-back'.
An element looks like (POSITION FUNCTION ARGS...).
To use the element, do (apply FUNCTION ARGS) then goto the point.")
(put 'help-xref-stack 'permanent-local t)

(defvar-local help-xref-forward-stack nil
  "A stack used to navigate help forwards after using the back button.
Used by `help-follow-symbol' and `help-xref-go-forward'.
An element looks like (POSITION FUNCTION ARGS...).
To use the element, do (apply FUNCTION ARGS) then goto the point.")
(put 'help-xref-forward-stack 'permanent-local t)

(defvar-local help-xref-stack-item nil
  "An item for `help-follow-symbol' to push onto `help-xref-stack'.
The format is (FUNCTION ARGS...).")
(put 'help-xref-stack-item 'permanent-local t)

(defvar-local help-xref-stack-forward-item nil
  "An item for `help-go-back' to push onto `help-xref-forward-stack'.
The format is (FUNCTION ARGS...).")
(put 'help-xref-stack-forward-item 'permanent-local t)

(setq-default help-xref-stack nil help-xref-stack-item nil)
(setq-default help-xref-forward-stack nil help-xref-forward-stack-item nil)

(defvar help-mode-syntax-table
  (let ((table (make-syntax-table emacs-lisp-mode-syntax-table)))
    ;; Treat single quotes as parens so that forward-sexp does not
    ;; break when a quoted string contains punctuation.
    (modify-syntax-entry ?‘ "(’  " table)
    (modify-syntax-entry ?’ ")‘  " table)
    table)
  "Syntax table used in `help-mode'.")

(defcustom help-mode-hook nil
  "Hook run by `help-mode'."
  :type 'hook
  :group 'help)

;; Button types used by help

(define-button-type 'help-xref
  'follow-link t
  'action #'help-button-action)

(defun help-button-action (button)
  "Call BUTTON's help function."
  (help-do-xref nil
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

;; Make some more idiosyncratic button types.

(define-button-type 'help-symbol
  :supertype 'help-xref
  'help-function #'describe-symbol
  'help-echo (purecopy "mouse-2, RET: describe this symbol"))

(define-button-type 'help-back
  :supertype 'help-xref
  'help-function #'help-xref-go-back
  'help-echo (purecopy "mouse-2, RET: go back to previous help buffer"))

(define-button-type 'help-forward
  :supertype 'help-xref
  'help-function #'help-xref-go-forward
  'help-echo (purecopy "mouse-2, RET: move forward to next help buffer"))

(define-button-type 'help-info-variable
  :supertype 'help-xref
  ;; the name of the variable is put before the argument to Info
  'help-function (lambda (_a v) (info v))
  'help-echo (purecopy "mouse-2, RET: read this Info node"))

(define-button-type 'help-info
  :supertype 'help-xref
  'help-function #'info
  'help-echo (purecopy "mouse-2, RET: read this Info node"))

(define-button-type 'help-man
  :supertype 'help-xref
  'help-function #'man
  'help-echo (purecopy "mouse-2, RET: read this man page"))

(define-button-type 'help-customization-group
  :supertype 'help-xref
  'help-function #'customize-group
  'help-echo (purecopy "mouse-2, RET: display this customization group"))

(define-button-type 'help-url
  :supertype 'help-xref
  'help-function #'browse-url
  'help-echo (purecopy "mouse-2, RET: view this URL in a browser"))

(define-button-type 'help-customize-variable
  :supertype 'help-xref
  'help-function (lambda (v)
		   (customize-variable v))
  'help-echo (purecopy "mouse-2, RET: customize variable"))

(define-button-type 'help-customize-face
  :supertype 'help-xref
  'help-function (lambda (v)
		   (customize-face v))
  'help-echo (purecopy "mouse-2, RET: customize face"))

(defun help-function-def--button-function (fun &optional file type)
  (or file
      (setq file (find-lisp-object-file-name fun type)))
  (if (not file)
      (message "Unable to find defining file")
    (require 'find-func)
    (when (eq file 'C-source)
      (setq file
            (help-C-file-name (indirect-function fun) 'fun)))
    ;; Don't use find-function-noselect because it follows
    ;; aliases (which fails for built-in functions).
    (let* ((location
            (find-function-search-for-symbol fun type file))
           (position (cdr location)))
      (if help-window-keep-selected
          (pop-to-buffer-same-window (car location))
        (pop-to-buffer (car location)))
      (run-hooks 'find-function-after-hook)
      (if position
          (progn
            ;; Widen the buffer if necessary to go to this position.
            (when (or (< position (point-min))
                      (> position (point-max)))
              (widen))
            ;; Save mark for the old location, unless the point is not
            ;; actually going to move.
            (unless (= (point) position)
              (push-mark nil t))
            (goto-char position))
        (message "Unable to find location in file")))))

(define-button-type 'help-function-def
  :supertype 'help-xref
  'help-function #'help-function-def--button-function
  'help-echo (purecopy "mouse-2, RET: find function's definition"))

(define-button-type 'help-function-cmacro ; FIXME: Obsolete since 24.4.
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (setq file (locate-library file t))
		   (if (and file (file-readable-p file))
		       (progn
                         (if help-window-keep-selected
			     (pop-to-buffer-same-window
                              (find-file-noselect file))
                           (pop-to-buffer (find-file-noselect file)))
                         (widen)
			 (goto-char (point-min))
			 (if (re-search-forward
			      (format "^[ \t]*(\\(cl-\\)?define-compiler-macro[ \t]+%s"
				      (regexp-quote (symbol-name fun)))
                              nil t)
			     (forward-line 0)
			   (message "Unable to find location in file")))
		     (message "Unable to find file")))
  'help-echo (purecopy "mouse-2, RET: find function's compiler macro"))

(define-button-type 'help-variable-def
  :supertype 'help-xref
  'help-function (lambda (var &optional file)
		   (when (eq file 'C-source)
		     (setq file (help-C-file-name var 'var)))
		   (let* ((location (find-variable-noselect var file))
                          (position (cdr location)))
                     (if help-window-keep-selected
		         (pop-to-buffer-same-window (car location))
                       (pop-to-buffer (car location)))
		     (run-hooks 'find-function-after-hook)
                     (if position
                           (progn
                             ;; Widen the buffer if necessary to go to this position.
                             (when (or (< position (point-min))
                                       (> position (point-max)))
                               (widen))
                             (goto-char position))
                       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find variable's definition"))

(define-button-type 'help-face-def
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (require 'find-func)
		   ;; Don't use find-function-noselect because it follows
		   ;; aliases (which fails for built-in functions).
		   (let* ((location
			  (find-function-search-for-symbol fun 'defface file))
                         (position (cdr location)))
                     (if help-window-keep-selected
                         (pop-to-buffer-same-window (car location))
		       (pop-to-buffer (car location)))
                     (if position
                           (progn
                             ;; Widen the buffer if necessary to go to this position.
                             (when (or (< position (point-min))
                                       (> position (point-max)))
                               (widen))
                             (goto-char position))
                       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find face's definition"))

(define-button-type 'help-package
  :supertype 'help-xref
  'help-function 'describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))

(define-button-type 'help-package-def
  :supertype 'help-xref
  'help-function (lambda (file) (dired file))
  'help-echo (purecopy "mouse-2, RET: visit package directory"))

(define-button-type 'help-theme-def
  :supertype 'help-xref
  'help-function #'find-file
  'help-echo (purecopy "mouse-2, RET: visit theme file"))

(define-button-type 'help-theme-edit
  :supertype 'help-xref
  'help-function #'customize-create-theme
  'help-echo (purecopy "mouse-2, RET: edit this theme file"))

(define-button-type 'help-dir-local-var-def
  :supertype 'help-xref
  'help-function (lambda (_var &optional file)
		   ;; FIXME: this should go to the point where the
		   ;; local variable was defined.
		   (find-file file))
  'help-echo (purecopy "mouse-2, RET: open directory-local variables file"))
(define-button-type 'help-news
  :supertype 'help-xref
  'help-function
  (lambda (file pos)
    (if help-window-keep-selected
        (view-file file)
      (view-file-other-window file))
    (goto-char pos))
  'help-echo (purecopy "mouse-2, RET: show corresponding NEWS announcement"))

;;;###autoload
(defun help-mode--add-function-link (str fun)
  (make-text-button (copy-sequence str) nil
                    'type 'help-function
                    'help-args (list fun)))


(defvar bookmark-make-record-function)
(defvar help-mode--current-data nil)

;;;###autoload
(define-derived-mode help-mode special-mode "Help"
  "Major mode for viewing help text and navigating references in it.
Also see the `help-enable-variable-value-editing' variable.

Commands:
\\{help-mode-map}"
  (setq-local revert-buffer-function
              #'help-mode-revert-buffer)
  (add-hook 'context-menu-functions #'help-mode-context-menu 5 t)
  (setq-local tool-bar-map
              help-mode-tool-bar-map)
  (setq-local help-mode--current-data nil)
  (setq-local bookmark-make-record-function
              #'help-bookmark-make-record)
  (unless search-default-mode
    (isearch-fold-quotes-mode)))

;;;###autoload
(defun help-mode-setup ()
  "Enter Help mode in the current buffer."
  (declare (obsolete nil "29.1"))
  (help-mode)
  (setq buffer-read-only nil))

;;;###autoload
(defun help-mode-finish ()
  "Finalize Help mode setup in current buffer."
  (declare (obsolete nil "29.1"))
  (when (derived-mode-p 'help-mode)
    (setq buffer-read-only t)
    (help-make-xrefs (current-buffer))))

;; Grokking cross-reference information in doc strings and
;; hyperlinking it.

;; This may have some scope for extension and the same or something
;; similar should be done for widget doc strings, which currently use
;; another mechanism.

(defvar help-back-label (purecopy "[back]")
  "Label to use by `help-make-xrefs' for the go-back reference.")

(defvar help-forward-label (purecopy "[forward]")
  "Label to use by `help-make-xrefs' for the go-forward reference.")

(defconst help-xref-symbol-regexp
  (purecopy (concat "\\(\\<\\(\\(variable\\|option\\)\\|"  ; Link to var
 		    "\\(function\\|command\\|call\\)\\|"   ; Link to function
 		    "\\(face\\)\\|"			   ; Link to face
 		    "\\(symbol\\|program\\|property\\)\\|" ; Don't link
		    "\\(source \\(?:code \\)?\\(?:of\\|for\\)\\)\\)"
		    "[ \t\n]+\\)?"
                    "\\(\\\\\\+\\)?"
                    "['`‘]\\(\\(?:\\sw\\|\\s_\\)+\\|`\\)['’]"))
  "Regexp matching doc string references to symbols.

The words preceding the quoted symbol can be used in doc strings to
distinguish references to variables, functions and symbols.")

(defvar help-xref-mule-regexp nil
  "Regexp matching doc string references to MULE-related keywords.

It is usually nil, and is temporarily bound to an appropriate regexp
when help commands related to multilingual environment (e.g.,
`describe-coding-system') are invoked.")


(defconst help-xref-info-regexp
  (purecopy
   "\\<[Ii]nfo[ \t\n]+\\(node\\|anchor\\)[ \t\n]+['`‘]\\([^'’]+\\)['’]")
  "Regexp matching doc string references to an Info node.")

(defconst help-xref-man-regexp
  (purecopy
   "\\<[Mm]an[ \t\n]+page[ \t\n]+\\(?:for[ \t\n]+\\)?['`‘\"]\\([^'’\"]+\\)['’\"]")
  "Regexp matching doc string references to a man page.")

(defconst help-xref-customization-group-regexp
  (purecopy "\\<[Cc]ustomization[ \t\n]+[Gg]roup[ \t\n]+['`‘]\\([^'’]+\\)['’]")
  "Regexp matching doc string references to a customization group.")

(defconst help-xref-url-regexp
  (purecopy "\\<[Uu][Rr][Ll][ \t\n]+['`‘]\\([^'’]+\\)['’]")
  "Regexp matching doc string references to a URL.")

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
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when interactive-p
      (let ((tail (nthcdr 10 help-xref-stack)))
	;; Truncate the stack.
	(if tail (setcdr tail nil))))
    (setq help-xref-stack-item item)))

(defvar help-xref-following nil
  "Non-nil when following a help cross-reference.")

;;;###autoload
(defun help-buffer ()
  "Return the name of a buffer for inserting help.
If `help-xref-following' is non-nil and the current buffer is
derived from `help-mode', this is the name of the current buffer.

Otherwise, return \"*Help*\", creating a buffer with that name if
it does not already exist."
  (buffer-name                         ;for with-output-to-temp-buffer
   (if (and help-xref-following
            (derived-mode-p 'help-mode))
       (current-buffer)
     (get-buffer-create "*Help*"))))

(defvar describe-symbol-backends
  `((nil ,#'fboundp ,(lambda (s _b _f) (describe-function s)))
    (nil
     ,(lambda (symbol)
        (or (and (boundp symbol) (not (keywordp symbol)))
            (get symbol 'variable-documentation)))
     ,#'describe-variable)
    ("face" ,#'facep ,(lambda (s _b _f) (describe-face s))))
  "List of providers of information about symbols.
Each element has the form (NAME TESTFUN DESCFUN) where:
  NAME is a string naming a category of object, such as \"type\" or \"face\".
  TESTFUN is a predicate which takes a symbol and returns non-nil if the
    symbol is such an object.
  DESCFUN is a function which takes three arguments (a symbol, a buffer,
    and a frame), inserts the description of that symbol in the current buffer
    and returns that text as well.")

(defcustom help-clean-buttons nil
  "If non-nil, remove quotes around link buttons."
  :version "29.1"
  :type 'boolean
  :group 'help)

;;;###autoload
(defun help-make-xrefs (&optional buffer)
  "Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow-symbol'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`help-xref-symbol-regexp'.  Faces only get cross-referenced if
preceded or followed by the word `face'.  Variables without
variable documentation do not get cross-referenced, unless
preceded by the word `variable' or `option'.

If the variable `help-xref-mule-regexp' is non-nil, find also
cross-reference information related to multilingual environment
\(e.g., coding-systems).  This variable is also used to disambiguate
the type of reference as the same way as `help-xref-symbol-regexp'.

A special reference `back' is made to return back through a stack of
help buffers.  Variable `help-back-label' specifies the text for
that."
  (interactive "b")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Skip the first bit, which has already been buttonized.
      (forward-paragraph)
      (let ((old-modified (buffer-modified-p)))
        (let ((stab (syntax-table))
              (case-fold-search t)
              (inhibit-read-only t))
          (set-syntax-table help-mode-syntax-table)
          ;; The following should probably be abstracted out.
          (unwind-protect
              (progn
                ;; Info references
                (save-excursion
                  (while (re-search-forward help-xref-info-regexp nil t)
                    (let ((data (match-string 2)))
                      (save-match-data
                        (unless (string-match "^([^)]+)" data)
                          (setq data (concat "(emacs)" data)))
			(setq data ;; possible newlines if para filled
			      (replace-regexp-in-string "[ \t\n]+" " " data t t)))
                      (help-xref-button 2 'help-info data))))
                ;; Man references
                (save-excursion
                  (while (re-search-forward help-xref-man-regexp nil t)
                    (help-xref-button 1 'help-man (match-string 1))))
                ;; Customization groups.
                (save-excursion
                  (while (re-search-forward
                          help-xref-customization-group-regexp nil t)
                    (help-xref-button 1 'help-customization-group
                                      (intern (match-string 1)))))
                ;; URLs
                (save-excursion
                  (while (re-search-forward help-xref-url-regexp nil t)
                    (let ((data (match-string 1)))
                      (help-xref-button 1 'help-url data))))
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
                            (help-xref-button 7 'help-input-method data))
                           ((and sym (coding-system-p sym))
                            (help-xref-button 7 'help-coding-system sym))
                           ((and sym (charsetp sym))
                            (help-xref-button 7 'help-character-set sym)))))))
                ;; Quoted symbols
                (save-excursion
                  (while (re-search-forward help-xref-symbol-regexp nil t)
                    (when-let ((sym (intern-soft (match-string 9))))
                      (if (match-string 8)
                          (delete-region (match-beginning 8)
                                         (match-end 8))
                        (cond
                         ((match-string 3)        ; `variable' &c
                          (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                                   (get sym 'variable-documentation))
                               (help-xref-button 9 'help-variable sym)))
                         ((match-string 4)     ; `function' &c
                          (and (fboundp sym)   ; similarly
                               (help-xref-button 9 'help-function sym)))
                         ((match-string 5) ; `face'
                          (and (facep sym)
                               (help-xref-button 9 'help-face sym)))
                         ((match-string 6)) ; nothing for `symbol'
                         ((match-string 7)
                          (help-xref-button 9 'help-function-def sym))
                         ((cl-some (lambda (x) (funcall (nth 1 x) sym))
                                   describe-symbol-backends)
                          (help-xref-button 9 'help-symbol sym)))))))
                ;; An obvious case of a key substitution:
                (save-excursion
                  (while (re-search-forward
                          ;; Assume command name is only word and symbol
                          ;; characters to get things like `use M-x foo->bar'.
                          ;; Command required to end with word constituent
                          ;; to avoid `.' at end of a sentence.
                          "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
                    (let ((sym (intern-soft (match-string 1))))
                      (if (fboundp sym)
                          (help-xref-button 1 'help-function sym))))))
            (set-syntax-table stab))
          ;; Delete extraneous newlines at the end of the docstring
          (goto-char (point-max))
          (while (and (not (bobp)) (bolp))
            (delete-char -1))
          (insert "\n")
          (help-xref--navigation-buttons))
        (set-buffer-modified-p old-modified)))))

(defun help-xref--navigation-buttons ()
  (let ((inhibit-read-only t))
    (when (or help-xref-stack help-xref-forward-stack)
      (ensure-empty-lines 1))
    ;; Make a back-reference in this buffer if appropriate.
    (when help-xref-stack
      (help-insert-xref-button help-back-label 'help-back
                               (current-buffer)))
    ;; Make a forward-reference in this buffer if appropriate.
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert "\t"))
      (help-insert-xref-button help-forward-label 'help-forward
                               (current-buffer)))
    (unless (bolp)
      (insert "\n"))))

;;;###autoload
(defun help-xref-button (match-number type &rest args)
  "Make a hyperlink for cross-reference text previously matched.
MATCH-NUMBER is the subexpression of interest in the last matched
regexp.  TYPE is the type of button to use.  Any remaining arguments are
passed to the button's help-function when it is invoked.
See `help-make-xrefs'.

This function removes quotes surrounding the match if the
variable `help-clean-buttons' is non-nil."
  ;; Don't mung properties we've added specially in some instances.
  (let ((beg (match-beginning match-number))
        (end (match-end match-number)))
    (unless (button-at beg)
      (make-text-button beg end 'type type 'help-args args)
      (when (and help-clean-buttons
                 (> beg (point-min))
                 (save-excursion
                   (goto-char (1- beg))
                   (looking-at "['`‘]"))
                 (< end (point-max))
                 (save-excursion
                   (goto-char end)
                   (looking-at "['’]")))
        (delete-region end (1+ end))
        (delete-region (1- beg) beg)))))

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
  (if (> (- to from) 5000) nil
    (with-syntax-table help-mode-syntax-table
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (goto-char (point-min))
	  (ignore-errors
	    (while (not (eobp))
	      (cond
	       ((looking-at-p "\"") (forward-sexp 1))
	       ((looking-at-p "#<") (search-forward ">" nil 'move))
	       ((looking-at "\\(\\(\\sw\\|\\s_\\)+\\)")
		(let* ((sym (intern-soft (match-string 1)))
		       (type (cond ((fboundp sym) 'help-function)
				   ((or (memq sym '(t nil))
					(keywordp sym))
				    nil)
				   ((and sym
					 (or (boundp sym)
					     (get sym
						  'variable-documentation)))
				    'help-variable))))
		  (when type (help-xref-button 1 type sym)))
		(goto-char (match-end 1)))
	       (t (forward-char 1))))))))))


;; Additional functions for (re-)creating types of help buffers.

;;;###autoload
(define-obsolete-function-alias 'help-xref-interned #'describe-symbol "25.1")


;; Navigation/hyperlinking with xrefs

(defun help-xref-go-back (buffer)
  "From BUFFER, go back to previous help buffer text using `help-xref-stack'."
  (let (item position method args)
    (with-current-buffer buffer
      (push (cons (point) help-xref-stack-item) help-xref-forward-stack)
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

(defun help-xref-go-forward (buffer)
  "From BUFFER, go forward to next help buffer."
  (let (item position method args)
    (with-current-buffer buffer
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (when help-xref-forward-stack
	(setq item (pop help-xref-forward-stack)
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
  "Go back to previous topic in this help buffer."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (user-error "No previous help buffer")))

(defun help-go-forward ()
  "Go to the next topic in this help buffer."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (user-error "No next help buffer")))

(defun help-goto-next-page ()
  "Go to the next page (if any) in the current buffer.
The help buffers are divided into \"pages\" by the ^L character."
  (interactive nil help-mode)
  (push-mark)
  (forward-page)
  (unless (eobp)
    (forward-line 1)))

(defun help-goto-previous-page ()
  "Go to the previous page (if any) in the current buffer.
\(If not at the start of a page, go to the start of the current page.)

The help buffers are divided into \"pages\" by the ^L character."
  (interactive nil help-mode)
  (push-mark)
  (backward-page (if (looking-back "\f\n" (- (point) 5)) 2 1))
  (unless (bobp)
    (forward-line 1)))

(defun help-view-source ()
  "View the source of the current help item."
  (interactive nil help-mode)
  (unless (plist-get help-mode--current-data :file)
    (error "Source file for the current help item is not defined"))
  (help-function-def--button-function
   (plist-get help-mode--current-data :symbol)
   (plist-get help-mode--current-data :file)
   (plist-get help-mode--current-data :type)))

(defun help-goto-info ()
  "View the *info* node of the current help item."
  (interactive nil help-mode)
  (unless help-mode--current-data
    (error "No symbol to look up in the current buffer"))
  (info-lookup-symbol (plist-get help-mode--current-data :symbol)
                      'emacs-lisp-mode
                      help-window-keep-selected))

(defun help-goto-lispref-info ()
  "View the Emacs Lisp manual *info* node of the current help item."
  (interactive nil help-mode)
  (unless help-mode--current-data
    (error "No symbol to look up in the current buffer"))
  (info-lookup-symbol (plist-get help-mode--current-data :symbol)
                      'emacs-lisp-only))

(defun help-customize ()
  "Customize variable or face whose doc string is shown in the current buffer."
  (interactive nil help-mode)
  (let ((sym (plist-get help-mode--current-data :symbol)))
    (unless (or (boundp sym) (facep sym))
      (user-error "No variable or face to customize"))
    (cond
     ((boundp sym) (customize-variable sym))
     ((facep sym) (customize-face sym)))))

(defun help-do-xref (_pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting help buffer has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following t))
    (apply function (if (eq function 'info)
                        (append args (list (generate-new-buffer-name "*info*")))
                      args))))

;; The doc string is meant to explain what buttons do.
(defun help-follow-mouse ()
  "Follow the cross-reference that you click on."
  (declare (obsolete nil "28.1"))
  (interactive)
  (error "No cross-reference here"))

;; The doc string is meant to explain what buttons do.
(defun help-follow ()
  "Follow cross-reference at point.

For the cross-reference format, see `help-make-xrefs'."
  (declare (obsolete nil "28.1"))
  (interactive)
  (user-error "No cross-reference here"))

(defun help-follow-symbol (&optional pos)
  "In help buffer, show docs for symbol at POS, defaulting to point.
Show all docs for that symbol as either a variable, function or face."
  (interactive "d")
  (unless pos
    (setq pos (point)))
  ;; check if the symbol under point is a function, variable or face
  (let ((sym
	 (intern
	  (save-excursion
	    (goto-char pos) (skip-syntax-backward "w_")
	    (buffer-substring (point)
			      (progn (skip-syntax-forward "w_")
				     (point)))))))
    (if (or (boundp sym)
	    (get sym 'variable-documentation)
	    (fboundp sym) (facep sym))
        (help-do-xref pos #'describe-symbol (list sym))
      (user-error "No symbol here"))))

(defun help-mode-revert-buffer (_ignore-auto _noconfirm)
  (let ((pos (point))
	(item help-xref-stack-item)
	;; Pretend there is no current item to add to the history.
	(help-xref-stack-item nil)
	;; Use the current buffer.
	(help-xref-following t))
    (apply (car item) (cdr item))
    (goto-char pos)))

(defun help-insert-string (string)
  "Insert STRING to the help buffer and install xref info for it.
This function can be used to restore the old contents of the help buffer
when going back to the previous topic in the xref stack.  It is needed
in case when it is impossible to recompute the old contents of the
help buffer by other means."
  (setq help-xref-stack-item (list #'help-insert-string string))
  (with-output-to-temp-buffer (help-buffer)
    (insert string)))


;; Bookmark support

(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-make-record-default "bookmark"
                  (&optional no-file no-context posn))

(defun help-bookmark-make-record ()
  "Create and return a `help-mode' bookmark record.
Implements `bookmark-make-record-function' for `help-mode' buffers."
  (unless (car help-xref-stack-item)
    (error "Cannot create bookmark - help command not known"))
  `(,@(bookmark-make-record-default 'NO-FILE 'NO-CONTEXT)
      (help-fn     . ,(car help-xref-stack-item))
      (help-args   . ,(mapcar (lambda (a)
                                (if (bufferp a) (buffer-name a) a))
                              (cdr help-xref-stack-item)))
      (position    . ,(point))
      (handler     . help-bookmark-jump)))

;;;###autoload
(defun help-bookmark-jump (bookmark)
  "Jump to `help-mode' bookmark BOOKMARK.
Handler function for record returned by `help-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((help-fn    (bookmark-prop-get bookmark 'help-fn))
        (help-args  (bookmark-prop-get bookmark 'help-args))
        (position   (bookmark-prop-get bookmark 'position)))
    (apply help-fn help-args)
    (pop-to-buffer "*Help*")
    (goto-char position)))

(put 'help-bookmark-jump 'bookmark-handler-type "Help")

(provide 'help-mode)

;;; help-mode.el ends here
