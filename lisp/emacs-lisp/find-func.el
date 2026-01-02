;;; find-func.el --- find the definition of the Emacs Lisp function near point  -*- lexical-binding:t -*-

;; Copyright (C) 1997, 1999, 2001-2026 Free Software Foundation, Inc.

;; Author: Jens Petersen <petersen@kurims.kyoto-u.ac.jp>
;; Keywords: emacs-lisp, functions, variables
;; Created: 97/07/25

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
;; The funniest thing about this is that I can't imagine why a package
;; so obviously useful as this hasn't been written before!!
;; ;;; find-func
;; (find-function-mode 1)
;;
;; or just:
;;
;; (load "find-func")
;;
;; if you don't like the given keybindings and away you go!  It does
;; pretty much what you would expect, putting the cursor at the
;; definition of the function or variable at point.
;;
;; The code started out from `describe-function', `describe-key'
;; ("help.el") and `fff-find-loaded-emacs-lisp-function' (Noah Friedman's
;; "fff.el").

;;; Code:

;;; User variables:

(defgroup find-function nil
  "Finds the definition of the Emacs Lisp symbol near point."
;;   :prefix "find-function"
  :group 'lisp)

(defconst find-function-space-re "\\(?:\\s-\\|\n\\|;.*\n\\)+")

(defcustom find-function-regexp
  ;; Match things like (defun foo ...), (defmacro foo ...),
  ;; (define-skeleton foo ...), (define-generic-mode 'foo ...),
  ;;  (define-derived-mode foo ...), (define-minor-mode foo)
  (concat
   "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
ine\\(?:-global\\)?-minor-mode\\|ine-compilation-mode\\|un-cvs-mode\\|\
foo\\|\\(?:[^icfgv]\\|g[^r]\\)\\(\\w\\|\\s_\\)+\\*?\\)\\|easy-mmode-define-[a-z-]+\\|easy-menu-define\\|\
cl-\\(?:defun\\|defmethod\\|defgeneric\\)\\|\
transient-define-\\(?:prefix\\|suffix\\|infix\\|argument\\)\\|\
menu-bar-make-toggle\\|menu-bar-make-toggle-command\\)"
   find-function-space-re
   "\\('\\|(quote \\)?%s\\(\\s-\\|$\\|[()]\\)")
  "The regexp used by `find-function' to search for a function definition.
Note it must contain a `%s' at the place where `format'
should insert the function name.  The default value avoids `defconst',
`defgroup', `defvar', `defface'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "21.1")

(defcustom find-variable-regexp
  (concat
   "^\\s-*(\\(def[^fumag]\\(\\w\\|\\s_\\)+\\*?\\|\
easy-mmode-def\\(map\\|syntax\\)\\|easy-menu-define\\)"
   find-function-space-re
   "%s\\(\\s-\\|$\\)")
  "The regexp used by `find-variable' to search for a variable definition.
Note it must contain a `%s' at the place where `format'
should insert the variable name.  The default value
avoids `defun', `defmacro', `defalias', `defadvice', `defgroup', `defface'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "21.1")

(defcustom find-face-regexp
  (concat"^\\s-*(defface" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used by `find-face' to search for a face definition.
Note it must contain a `%s' at the place where `format'
should insert the face name.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(defcustom find-feature-regexp
  (concat ";;; Code:")
  "Regexp used by `xref-find-definitions' when searching for a feature definition.
Note it may contain up to one `%s' at the place where `format'
should insert the feature name."
  ;; We search for ";;; Code" rather than (feature '%s) because the
  ;; former is near the start of the code, and the latter is very
  ;; uninteresting. If the regexp is not found, just goes to
  ;; (point-min), which is acceptable in this case.
  :type 'regexp
  :group 'xref
  :version "25.1")

(defcustom find-alias-regexp
  "(defalias +'%s"
  "The regexp used by `xref-find-definitions' to search for an alias definition.
Note it must contain a `%s' at the place where `format'
should insert the feature name."
  :type 'regexp
  :group 'xref
  :version "25.1")

(defun find-function--defface (symbol)
  (catch 'found
    (while (re-search-forward (format find-face-regexp symbol) nil t)
      (unless (ppss-comment-or-string-start
               (save-excursion (syntax-ppss (match-beginning 0))))
        ;; We're not in a comment or a string.
        (throw 'found t)))))

(defvar find-function-regexp-alist
  '((nil . find-function-regexp)
    (defvar . find-variable-regexp)
    (defface . find-function--defface)
    (feature . find-feature-regexp)
    (defalias . find-alias-regexp))
  "Alist mapping definition types into regexp variables.
Each regexp variable's value should actually be a format string
to be used to substitute the desired symbol name into the regexp.
Instead of regexp variable, types can be mapped to functions as well,
in which case the function is called with one argument (the object
we're looking for) and it should search for it.

A value can also be a cons (REGEX . EXPANDED-FORM-MATCHER-FACTORY).
REGEX is as above; EXPANDED-FORM-MATCHER-FACTORY is a function of one
argument, the same object we'd pass to a REGEX function; it should return
another function of one argument that returns non-nil if we're looking at
a macroexpanded form that defines the object we're looking for.
If you want to use EXPANDED-FORM-MATCHER-FACTORY exclusively, you can
set REGEX to a never-match regexp, and force the fallback to
EXPANDED-FORM-MATCHER-FACTORY.  EXPANDED-FORM-MATCHER-FACTORY is
called with the buffer to search the current one.

Symbols can have their own version of this alist on
the property `find-function-type-alist'.
See the function `find-function-update-type-alist'.")
(put 'find-function-regexp-alist 'risky-local-variable t)

(define-obsolete-variable-alias 'find-function-source-path
  'find-library-source-path "28.1")
(defcustom find-library-source-path nil
  "The default list of directories where `find-library' searches.

If this variable is nil then `find-library' searches `load-path' by
default."
  :type '(repeat directory)
  :group 'find-function
  :version "28.1")

(defcustom find-function-recenter-line 1
  "The window line-number from which to start displaying a symbol definition.
A value of nil implies center the beginning of the definition.
See `find-function' and `find-variable'."
  :type '(choice (const :tag "Center" nil)
		 integer)
  :group 'find-function
  :version "20.3")

(defcustom find-function-after-hook nil
  "Hook run after finding symbol definition.

See the functions `find-function' and `find-variable'."
  :type 'hook
  :group 'find-function
  :version "20.3")

(defcustom find-library-include-other-files t
  "If non-nil, `read-library-name' will also include non-library files.
This affects commands like `read-library'.

If nil, only library files (i.e., \".el\" files) will be offered
for completion."
  :type 'boolean
  :version "29.1"
  :group 'find-function)

;; Compiler defvars.  The variable will be defined later with
;; `defcustom' when everything used in the :set functions is defined.
(defvar find-function-mode-lower-precedence)

;;; Functions:

(defun find-library-suffixes ()
  (let ((suffixes nil))
    (dolist (suffix (get-load-suffixes) (nreverse suffixes))
      (unless (string-match "elc" suffix) (push suffix suffixes)))))

(defun find-library--load-name (library)
  (let ((name library))
    (dolist (dir load-path)
      (let ((rel (file-relative-name library dir)))
        (if (and (not (string-match "\\`\\.\\./" rel))
                 (< (length rel) (length name)))
            (setq name rel))))
    (unless (equal name library) name)))

(defvar comp-eln-to-el-h)

(defun find-library-name (library)
  "Return the absolute file name of the Emacs Lisp source of LIBRARY.
LIBRARY should be a string (the name of the library)."
  ;; If the library is byte-compiled, try to find a source library by
  ;; the same name.
  (cond
   ((string-match "\\.el\\(c\\(\\..*\\)?\\)\\'" library)
    (setq library (replace-match "" t t library)))
   ((string-match "\\.eln\\'" library)
    (setq library (gethash (file-name-nondirectory library) comp-eln-to-el-h))))
  (or
   (locate-file library
                (or find-library-source-path load-path)
                (find-library-suffixes))
   (locate-file library
                (or find-library-source-path load-path)
                load-file-rep-suffixes)
   (when (file-name-absolute-p library)
     (let ((rel (find-library--load-name library)))
       (when rel
         (or
          (locate-file rel
                       (or find-library-source-path load-path)
                       (find-library-suffixes))
          (locate-file rel
                       (or find-library-source-path load-path)
                       load-file-rep-suffixes)))))
   (find-library--from-load-history library)
   (signal 'file-error (list "Can't find library" library))))

(defun find-library--from-load-history (library)
  ;; In `load-history', the file may be ".elc", ".el", ".el.gz", and
  ;; LIBRARY may be "foo.el" or "foo".
  (let ((load-re
         (concat "\\(" (regexp-quote (file-name-sans-extension library)) "\\)"
                 (regexp-opt (get-load-suffixes)) "\\'"))
        (alist load-history)
        elt file found)
    (while (and alist (null found))
      (setq elt (car alist)
            alist (cdr alist)
            file (car elt)
            found (and (stringp file) (string-match load-re file)
                       (let ((dir (substring file 0 (match-beginning 1)))
                             (basename (match-string 1 file)))
                         (locate-file basename (list dir)
                                      (find-library-suffixes))))))
    found))

(defvar find-function-C-source-directory
  (let ((dir (expand-file-name "src" source-directory)))
    (if (file-accessible-directory-p dir) dir))
  "Directory where the C source files of Emacs can be found.
If nil, do not try to find the source code of functions and variables
defined in C.")

(declare-function ad-get-advice-info "advice" (function))

(defun find-function-advised-original (func)
  "Return the original function definition of an advised function FUNC.
If FUNC is not a symbol, return it.  Else, if it's not advised,
return the symbol's function definition."
  (or (and (symbolp func)
           (advice--cd*r (symbol-function func)))
      func))

(defun find-function-C-source (fun-or-var file type)
  "Find the source location where FUN-OR-VAR is defined in FILE.
TYPE should be nil to find a function, or `defvar' to find a variable."
  (let ((dir (or find-function-C-source-directory
                 (read-directory-name "Emacs C source dir: " nil nil t))))
    (setq file (expand-file-name file dir))
    (if (file-readable-p file)
        (if (null find-function-C-source-directory)
            (setq find-function-C-source-directory dir))
      (error "The C source file %s is not available"
             (file-name-nondirectory file))))
  (unless type
    ;; Either or both an alias and its target might be advised.
    (setq fun-or-var (find-function-advised-original
		      (indirect-function
		       (find-function-advised-original fun-or-var)))))
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward
	     (if type
		 (concat "DEFVAR[A-Z_]*[ \t\n]*([ \t\n]*\""
			 (regexp-quote (symbol-name fun-or-var))
			 "\"")
	       (concat "DEFUN[ \t\n]*([ \t\n]*\""
		       (regexp-quote (subr-name (advice--cd*r fun-or-var)))
		       "\""))
	     nil t)
      (error "Can't find source for %s" fun-or-var))
    (cons (current-buffer) (match-beginning 0))))

;;;###autoload
(defun find-library (library)
  "Find the Emacs Lisp source of LIBRARY.

Interactively, prompt for LIBRARY using the one at or near point.

This function searches `find-library-source-path' if non-nil, and
`load-path' otherwise.

See the `find-library-include-other-files' user option for
customizing the candidate completions."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer (find-file-noselect (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

(defvar find-function--read-history-library nil)

;;;###autoload
(defun read-library-name ()
  "Read and return a library name, defaulting to the one near point.

A library name is the filename of an Emacs Lisp library located
in a directory under `load-path' (or `find-library-source-path',
if non-nil)."
  (let* ((dirs (or find-library-source-path load-path))
         (suffixes (find-library-suffixes))
         (def (if (eq (function-called-at-point) 'require)
                  ;; `function-called-at-point' may return 'require
                  ;; with `point' anywhere on this line.  So wrap the
                  ;; `save-excursion' below in a `condition-case' to
                  ;; avoid reporting a scan-error here.
                  (condition-case nil
                      (save-excursion
                        (backward-up-list)
                        (forward-char)
                        (forward-sexp 2)
                        (thing-at-point 'symbol))
                    (error nil))
                (thing-at-point 'symbol))))
    (if find-library-include-other-files
        (let ((table (apply-partially #'locate-file-completion-table
                                      dirs suffixes)))
          (when (and def (not (test-completion def table)))
            (setq def nil))
          (completing-read (format-prompt "Library name" def)
                           table nil nil nil
                           'find-function--read-history-library def))
      (let ((files (read-library-name--find-files dirs suffixes)))
        (when (and def (not (member def files)))
          (setq def nil))
        (completing-read (format-prompt "Library name" def)
                         files nil t nil
                         'find-function--read-history-library def)))))

(defun read-library-name--find-files (dirs suffixes)
  "Return a list of all files in DIRS that match SUFFIXES."
  (let ((files nil)
        (regexp (concat (regexp-opt suffixes) "\\'")))
    (dolist (dir dirs)
      (dolist (file (ignore-errors (directory-files dir nil regexp t)))
        (and (string-match regexp file)
             (push (substring file 0 (match-beginning 0)) files))))
    files))

;;;###autoload
(defun find-library-other-window (library)
  "Find the Emacs Lisp source of LIBRARY in another window.

See `find-library' for more details."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer-other-window (find-file-noselect
                                      (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

;;;###autoload
(defun find-library-other-frame (library)
  "Find the Emacs Lisp source of LIBRARY in another frame.

See `find-library' for more details."
  (interactive (list (read-library-name)))
  (prog1
      (switch-to-buffer-other-frame (find-file-noselect
                                     (find-library-name library)))
    (run-hooks 'find-function-after-hook)))

;;;###autoload
(defun find-function-search-for-symbol (symbol type library)
  "Search for SYMBOL's definition of type TYPE in LIBRARY.
Visit the library in a buffer, and return a cons cell (BUFFER . POSITION),
or just (BUFFER . nil) if the definition can't be found in the file.

If TYPE is nil, look for a function definition,
otherwise, TYPE specifies the kind of definition.
TYPE is looked up in SYMBOL's property `find-function-type-alist'
(which can be maintained with `find-function-update-type-alist')
or the variable `find-function-regexp-alist'.

The search is done in the source for library LIBRARY."
  (if (null library)
      (error "Don't know where `%s' is defined" symbol))
  ;; Some functions are defined as part of the construct
  ;; that defines something else.
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))
  (if (string-match "\\`src/\\(.*\\.\\(c\\|m\\)\\)\\'" library)
      (find-function-C-source symbol (match-string 1 library) type)
    (when (string-match "\\.el\\(c\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    ;; Strip extension from .emacs.el to make sure symbol is searched in
    ;; .emacs too.
    (when (string-match "\\.emacs\\(.el\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    (let* ((filename (find-library-name library))
	   (regexp-symbol
            (or (and (symbolp symbol)
                     (alist-get type (get symbol 'find-function-type-alist)))
                (alist-get type find-function-regexp-alist)))
           (form-matcher-factory
            (and (functionp (cdr-safe regexp-symbol))
                 (cdr regexp-symbol)))
           (regexp-symbol (if form-matcher-factory
                              (car regexp-symbol)
                            regexp-symbol)))
      (with-current-buffer (find-file-noselect filename)
	(let ((regexp (if (functionp regexp-symbol) regexp-symbol
                        (format (symbol-value regexp-symbol)
                                ;; Entry for ` (backquote) macro in loaddefs.el,
                                ;; (defalias (quote \`)..., has a \ but
                                ;; (symbol-name symbol) doesn't.  Add an
                                ;; optional \ to catch this.
                                (concat "\\\\?"
                                        (regexp-quote (symbol-name symbol))))))
	      (case-fold-search))
          (save-restriction
            (widen)
            (with-syntax-table emacs-lisp-mode-syntax-table
              (goto-char (point-min))
              (if (if (functionp regexp)
                      (funcall regexp symbol)
                    (or (re-search-forward regexp nil t)
                        ;; `regexp' matches definitions using known forms like
                        ;; `defun', or `defvar'.  But some functions/variables
                        ;; are defined using special macros (or functions), so
                        ;; if `regexp' can't find the definition, we look for
                        ;; something of the form "(SOMETHING <symbol> ...)".
                        ;; This fails to distinguish function definitions from
                        ;; variable declarations (or even uses thereof), but is
                        ;; a good pragmatic fallback.
                        (re-search-forward
                         (concat "^([^ ]+" find-function-space-re "['(]?"
                                 (regexp-quote (symbol-name symbol))
                                 "\\_>")
                         nil t)))
                  (progn
                    (beginning-of-line)
                    (cons (current-buffer) (point)))
                ;; If the regexp search didn't find the location of
                ;; the symbol (for example, because it is generated by
                ;; a macro), try a slightly more expensive search that
                ;; expands macros until it finds the symbol.  Since
                ;; macro-expansion involves arbitrary code execution,
                ;; only attempt it in trusted buffers.
                (cons (current-buffer)
                      (when (trusted-content-p)
                        (find-function--search-by-expanding-macros
                         (current-buffer) symbol type
                         form-matcher-factory)))))))))))

;;;###autoload
(defun find-function-update-type-alist (symbol type variable)
  "Update SYMBOL property `find-function-type-alist' with (TYPE . VARIABLE).
Property `find-function-type-alist' is a symbol-specific version
of variable `find-function-regexp-alist' and has the same format."
  (setf (alist-get type (get symbol 'find-function-type-alist)) variable))

(defun find-function--try-macroexpand (form)
  "Try to macroexpand FORM in full or partially.
This is a best-effort operation in which if macroexpansion fails,
this function returns FORM as is."
  (ignore-errors
    (or
     (macroexpand-all form)
     (macroexpand-1 form)
     form)))

(defun find-function--any-subform-p (form pred)
  "Walk FORM and apply PRED to its subexpressions.
Return t if any PRED returns t."
  (cond
   ((not (consp form)) nil)
   ((funcall pred form) t)
   (t
    (let ((left-child (car form))
          (right-child (cdr form)))
      (or
       (find-function--any-subform-p left-child pred)
       (find-function--any-subform-p right-child pred))))))

(defun find-function--search-by-expanding-macros
    (buf symbol type matcher-factory)
  "Expand macros in BUF to search for the definition of SYMBOL of TYPE."
  (with-current-buffer buf
    (when-let* ((expected-symbol-p
                 (cond ((null type)
                        (lambda (form)
                          ;; Check if a given form is a `defalias' to
                          ;; SYM, the function name we are searching
                          ;; for.  All functions in Emacs Lisp
                          ;; ultimately expand to a `defalias' form
                          ;; after several steps of macroexpansion.
                          (and (eq (car-safe form) 'defalias)
                               (equal (car-safe (cdr form))
                                      `(quote ,symbol)))))
                       ((eq type 'defvar)
                        (lambda (form)
                          ;; Variables generated by macros ultimately
                          ;; expand to `defvar'.
                          (and (eq (car-safe form) 'defvar)
                               (eq (car-safe (cdr form)) symbol))))
                       (matcher-factory
                        (funcall matcher-factory symbol)))))
      (catch 'found
        (save-excursion
          (goto-char (point-min))
          (condition-case nil
              (while t
                (when (find-function--any-subform-p
                       (find-function--try-macroexpand
                        (read (current-buffer)))
                       expected-symbol-p)
                  ;; We want to return the location at the beginning
                  ;; of the macro, so move back one sexp.
                  (throw 'found (progn (backward-sexp) (point)))))
            (end-of-file nil)))))))

(defun find-function-library (function &optional lisp-only verbose)
  "Return the pair (ORIG-FUNCTION . LIBRARY) for FUNCTION.

ORIG-FUNCTION is the original name, after resolving aliases.
LIBRARY is an absolute file name, a relative
file name inside the C sources directory, or a name of an
autoloaded feature.

If ORIG-FUNCTION is a built-in function and LISP-ONLY is non-nil,
signal an error.

If VERBOSE is non-nil, and FUNCTION is an alias, display a
message about the whole chain of aliases."
  (let ((def (when (symbolp function)
               (or (fboundp function)
                   (signal 'void-function (list function)))
               (find-function-advised-original function)))
        aliases)
    ;; FIXME for completeness, it might be nice to print something like:
    ;; foo (which is advised), which is an alias for bar (which is advised).
    (while (and def (symbolp def))
      (or (eq def function)
          (not verbose)
          (setq aliases (if aliases
                            (concat aliases
                                    (format-message
                                     ", which is an alias for `%s'"
                                     (symbol-name def)))
                          (format-message "`%s' is an alias for `%s'"
                                          function (symbol-name def)))))
      (setq function (find-function-advised-original function)
            def (find-function-advised-original function)))
    (if aliases
        (message "%s" aliases))
    (cons function
          (cond
           ((autoloadp def) (nth 1 def))
           ((subr-primitive-p def)
            (if lisp-only
                (error "%s is a built-in function" function))
            (help-C-file-name def 'subr))
           ((symbol-file function 'defun))))))

;;;###autoload
(defun find-function-noselect (function &optional lisp-only)
  "Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the source file containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.  If the function definition can't be found in
the buffer, returns (BUFFER).

If FUNCTION is a built-in function, this function normally
attempts to find it in the Emacs C sources; however, if LISP-ONLY
is non-nil, signal an error instead."
  (if (not function)
    (error "You didn't specify a function"))
  (let ((func-lib (find-function-library function lisp-only t)))
    (find-function-search-for-symbol (car func-lib) nil (cdr func-lib))))

(defvar find-function--read-history-function nil)
(defvar find-function--read-history-variable nil)
(defvar find-function--read-history-face nil)

(defun find-function-read (&optional type)
  "Read and return an interned symbol, defaulting to the one near point.

If TYPE is nil, insist on a symbol with a function definition.
Otherwise TYPE should be `defvar' or `defface'.
If TYPE is nil, defaults using `function-called-at-point',
otherwise uses `variable-at-point'."
  (let* ((symb1 (cond ((null type) (function-called-at-point))
                      ((eq type 'defvar) (variable-at-point))
                      ((eq type 'defface) (face-at-point t))
                      (t (variable-at-point t))))
         (symb  (unless (eq symb1 0) symb1))
         (predicate (cdr (assq type '((nil . fboundp)
                                      (defvar . boundp)
                                      (defface . facep)))))
         (prompt-type (cdr (assq type '((nil . "function")
                                        (defvar . "variable")
                                        (defface . "face")))))
         (enable-recursive-minibuffers t))
    (list (intern (completing-read
                   (format-prompt "Find %s" symb prompt-type)
                   obarray predicate
                   'lambda nil
                   (intern (format "find-function--read-history-%s" prompt-type))
                   (and symb (symbol-name symb)))))))

(defun find-function-do-it (symbol type switch-fn)
  "Find Emacs Lisp SYMBOL in a buffer and display it.
TYPE is nil to search for a function definition,
or else `defvar' or `defface'.

The variable `find-function-recenter-line' controls how
to recenter the display.  SWITCH-FN is the function to call
to display and select the buffer.
See also `find-function-after-hook'.

Set mark before moving, if the buffer already existed."
  (let* ((orig-point (point))
	(orig-buffers (buffer-list))
	(buffer-point (save-excursion
			(find-definition-noselect symbol type)))
	(new-buf (car buffer-point))
	(new-point (cdr buffer-point)))
    (when buffer-point
      (when (memq new-buf orig-buffers)
	(push-mark orig-point))
      (funcall switch-fn new-buf)
      (when new-point (goto-char new-point))
      (recenter find-function-recenter-line)
      (run-hooks 'find-function-after-hook))))

;;;###autoload
(defun find-function (function)
  "Find the definition of the Emacs Lisp FUNCTION near point.

Finds the source file containing the definition of the function
near point (selected by `function-called-at-point') in a buffer and
places point before the definition.
Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'.

Use \\[xref-find-definitions] to find definitions of functions and variables
that are not part of Emacs."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer))

;;;###autoload
(defun find-function-other-window (function)
  "Find, in another window, the definition of FUNCTION near point.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-window))

;;;###autoload
(defun find-function-other-frame (function)
  "Find, in another frame, the definition of FUNCTION near point.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-variable-noselect (variable &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of VARIABLE.

Finds the library containing the definition of VARIABLE in a buffer and
the point of the definition.  The buffer is not selected.
If the variable's definition can't be found in the buffer, return (BUFFER)."
  (if (not variable)
      (error "You didn't specify a variable")
    (let ((library (or file
                       (symbol-file variable 'defvar)
                       (help-C-file-name variable 'var))))
      (find-function-search-for-symbol variable 'defvar library))))

;;;###autoload
(defun find-variable (variable)
  "Find the definition of the VARIABLE at or before point.

Finds the library containing the definition of the variable
near point (selected by `variable-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer))

;;;###autoload
(defun find-variable-other-window (variable)
  "Find, in another window, the definition of VARIABLE near point.

See `find-variable' for more details."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer-other-window))

;;;###autoload
(defun find-variable-other-frame (variable)
  "Find, in another frame, the definition of VARIABLE near point.

See `find-variable' for more details."
  (interactive (find-function-read 'defvar))
  (find-function-do-it variable 'defvar 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-definition-noselect (symbol type &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL.
If the definition can't be found in the buffer, return (BUFFER).
TYPE says what type of definition: nil for a function, `defvar' for a
variable, `defface' for a face.  This function does not switch to the
buffer nor display it."
  (cond
   ((not symbol)
    (error "You didn't specify a symbol"))
   ((null type)
    (find-function-noselect symbol))
   ((eq type 'defvar)
    (find-variable-noselect symbol file))
   (t
    (let ((library (or file (symbol-file symbol type))))
      (find-function-search-for-symbol symbol type library)))))

;; For symmetry, this should be called find-face; but some programs
;; assume that, if that name is defined, it means something else.
;;;###autoload
(defun find-face-definition (face)
  "Find the definition of FACE.  FACE defaults to the name near point.

Finds the Emacs Lisp library containing the definition of the face
near point (selected by `variable-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'defface))
  (find-function-do-it face 'defface 'switch-to-buffer))

(defun find-function-on-key-do-it (key find-fn)
  "Find the function that KEY invokes.  KEY is a string.
Set mark before moving, if the buffer already existed.

FIND-FN is the function to call to navigate to the function."
  (let (defn)
    (save-excursion
      (let* ((event (and (eventp key) (aref key 0))) ; Null event OK below.
	     (start (event-start event))
	     (modifiers (event-modifiers event))
	     (window (and (or (memq 'click modifiers) (memq 'down modifiers)
			      (memq 'drag modifiers))
			  (posn-window start))))
	;; For a mouse button event, go to the button it applies to
	;; to get the right key bindings.  And go to the right place
	;; in case the keymap depends on where you clicked.
	(when (windowp window)
	  (set-buffer (window-buffer window))
	  (goto-char (posn-point start)))
	(setq defn (key-binding key))))
    (let ((key-desc (key-description key)))
      (if (or (null defn) (integerp defn))
	  (message "%s is unbound" key-desc)
	(if (consp defn)
	    (message "%s runs %s" key-desc (prin1-to-string defn))
	  (funcall find-fn defn))))))

;;;###autoload
(defun find-function-on-key (key)
  "Find the function that KEY invokes.  KEY is a string.
Set mark before moving, if the buffer already existed."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function))

;;;###autoload
(defun find-function-on-key-other-window (key)
  "Find, in the other window, the function that KEY invokes.
See `find-function-on-key'."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function-other-window))

;;;###autoload
(defun find-function-on-key-other-frame (key)
  "Find, in the other frame, the function that KEY invokes.
See `find-function-on-key'."
  (interactive "kFind function on key: ")
  (find-function-on-key-do-it key #'find-function-other-frame))

;;;###autoload
(defun find-function-at-point ()
  "Find directly the function at point in the other window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function-other-window symb))))

;;;###autoload
(defun find-variable-at-point ()
  "Find directly the variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (when (and symb (not (equal symb 0)))
      (find-variable-other-window symb))))

(defvar-keymap find-function-mode-map
  :doc "Keymap for `find-function-mode'."
  "C-x F"   #'find-function
  "C-x 4 F" #'find-function-other-window
  "C-x 5 F" #'find-function-other-frame

  "C-x K"   #'find-function-on-key
  "C-x 4 K" #'find-function-on-key-other-window
  "C-x 5 K" #'find-function-on-key-other-frame

  "C-x V"   #'find-variable
  "C-x 4 V" #'find-variable-other-window
  "C-x 5 V" #'find-variable-other-frame

  "C-x L"   #'find-library
  "C-x 4 L" #'find-library-other-window
  "C-x 5 L" #'find-library-other-frame)

;;;###autoload
(define-minor-mode find-function-mode
  "Enable some key bindings for the `find-function' family of functions."
  :group 'find-function :version "31.1" :global t :lighter nil
  (when find-function-mode-lower-precedence
    (rplacd (assq 'find-function-mode minor-mode-map-alist)
            (if find-function-mode
                (make-sparse-keymap)
              find-function-mode-map))
    (let ((parent (keymap-parent (current-global-map))))
      (if find-function-mode
          (unless (memq find-function-mode-map parent)
            (setf (keymap-parent (current-global-map))
                  (make-composed-keymap (list find-function-mode-map
                                              parent))))
        (when (memq find-function-mode-map parent)
          (delq find-function-mode-map parent))))))

;;;###autoload
(defun find-function-setup-keys ()
  "Turn on `find-function-mode', which see."
  (find-function-mode 1))
(make-obsolete 'find-function-setup-keys 'find-function-mode "31.1")

;; Custom variables with :set requires everything be defined
(defcustom find-function-mode-lower-precedence nil
  "If non-nil, `find-function-mode' defines keys in the global map.
This is for compatibility with the historical behavior of
the old `find-function-setup-keys'."
  :type 'boolean
  :version "31.1"
  :set (lambda (symbol value)
         ;; Toggle the mode off before changing this setting in order to
         ;; avoid getting into an inconsistent state.
         (let ((already-on find-function-mode))
           (when already-on (find-function-mode -1))
           (set-default symbol value)
           (when already-on (find-function-mode 1)))))

(provide 'find-func)

;;; find-func.el ends here
