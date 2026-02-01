;;; help-fns.el --- Complex help functions -*- lexical-binding: t -*-

;; Copyright (C) 1985-1986, 1993-1994, 1998-2026 Free Software
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

;; This file contains those help commands which are complicated, and
;; which may not be used in every session.  For example
;; `describe-function' will probably be heavily used when doing elisp
;; programming, but not if just editing C files.  Simpler help commands
;; are in help.el

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'help-mode)
(require 'radix-tree)
(eval-when-compile (require 'subr-x))   ;For when-let.

(defvar help-fns-describe-function-functions nil
  "List of functions to run in help buffer in `describe-function'.
Those functions will be run after the header line, the argument
list, and the function's documentation are inserted.
The functions will be called with one argument: the function's symbol.
They can assume that a newline was output just before they were called,
and they should terminate any of their own output with a newline.
By convention they should indent their output by 2 spaces.")

(defvar help-fns-describe-variable-functions nil
  "List of functions to run in help buffer in `describe-variable'.
Those functions will be run after the header line and value was inserted,
and before the documentation will be inserted.
The functions will receive the variable name as argument.
They can assume that a newline was output just before they were called,
and they should terminate any of their own output with a newline.
By convention they should indent their output by 2 spaces.
Current buffer is the buffer in which we queried the variable,
and the output should go to `standard-output'.")

(defvar help-fns-describe-face-functions nil
  "List of functions to run in help buffer in `describe-face'.
The functions will be used (and take the same parameters) as
described in `help-fns-describe-variable-functions', except that
the functions are called with two parameters: The face and the
frame.")

(defvar help-fns--activated-functions nil
  "Internal variable let-bound to help functions that have triggered.
Help functions can check the contents of this list to see whether
a specific previous help function has inserted something in the
current help buffer.")

;; Functions

(defvar help-definition-prefixes nil
  ;; FIXME: We keep `definition-prefixes' as a hash-table so as to
  ;; avoid pre-loading radix-tree and because it takes slightly less
  ;; memory.  But when we use this table it's more efficient to
  ;; represent it as a radix tree, since the main operation is to do
  ;; `radix-tree-prefixes'.  Maybe we should just bite the bullet and
  ;; use a radix tree for `definition-prefixes' (it's not *that*
  ;; costly, really).
  "Radix-tree representation replacing `definition-prefixes'.")

(defun help-definition-prefixes ()
  "Return the up-to-date radix-tree form of `definition-prefixes'."
  (when (and (null help-definition-prefixes)
             (> (hash-table-count definition-prefixes) 0))
    (maphash (lambda (prefix files)
               (let ((old (radix-tree-lookup help-definition-prefixes prefix)))
                 (setq help-definition-prefixes
                       (radix-tree-insert help-definition-prefixes
                                          prefix (append old files)))))
             definition-prefixes))
  help-definition-prefixes)

(defun help--loaded-p (file)
  "Try and figure out if FILE has already been loaded."
  ;; FIXME: this regexp business is not good enough: for file
  ;; `toto', it will say `toto' is loaded when in reality it was
  ;; just cedet/semantic/toto that has been loaded.
  (or (let ((feature (intern-soft file)))
        (and feature (featurep feature)))
      (let* ((re (load-history-regexp file))
             (done nil))
        (dolist (x load-history)
          (and (stringp (car x)) (string-match-p re (car x)) (setq done t)))
        done)))

(defun help--load-prefixes (prefixes)
  (pcase-dolist (`(,prefix . ,files) prefixes)
    (setq help-definition-prefixes
          (radix-tree-insert help-definition-prefixes prefix nil))
    (remhash prefix definition-prefixes)
    (dolist (file files)
      ;; FIXME: Should we scan help-definition-prefixes to remove
      ;; other prefixes of the same file?
      (unless (help--loaded-p file)
        (with-demoted-errors "while loading: %S"
          (load file 'noerror 'nomessage))))))


(define-obsolete-variable-alias 'help-enable-completion-auto-load
  'help-enable-completion-autoload "27.1")

(defcustom help-enable-completion-autoload t
  "Whether completion for Help commands can perform autoloading.
If non-nil, whenever invoking completion for `describe-function'
or `describe-variable' load files that might contain definitions
with the current prefix.  The files are chosen according to
`definition-prefixes'."
  :type 'boolean
  :group 'help
  :version "26.3")

(defcustom help-enable-variable-value-editing nil
  "If non-nil, allow editing variable values in *Help* buffers.

To edit the value of a variable, use \\[describe-variable] to
display a \"*Help*\" buffer, move point after the text
\"Its value is\" and type \\`e' to invoke `help-fns-edit-variable'.

Values that aren't readable by the Emacs Lisp reader can't be
edited even if this option is enabled."
  :type 'boolean
  :group 'help
  :version "29.1")

(defcustom help-enable-symbol-autoload nil
  "Perform autoload if docs are missing from autoload objects."
  :type 'boolean
  :group 'help
  :version "28.1")

(defcustom help-display-function-type t
  "Whether to display type specifiers of functions in \"*Help*\" buffers.

The type specifier of a function is returned by `comp-function-type-spec',
which see.  When this variable is non-nil, \\[describe-function] will \
display the function's
type specifier when available."
  :type 'boolean
  :group 'help
  :version "30.1")

(defun help--symbol-class (s)
  "Return symbol class characters for symbol S."
  (when (stringp s)
    (setq s (intern-soft s)))
  (concat
   (when (fboundp s)
     (concat
      (cond
       ((commandp s) "c")
       ((eq (car-safe (symbol-function s)) 'macro) "m")
       (t "f"))
      (and (let ((flist (indirect-function s)))
             (advice--p (if (eq 'macro (car-safe flist)) (cdr flist) flist)))
           "!")
      (and (get s 'byte-obsolete-info) "-")))
   (when (boundp s)
     (concat
      (if (custom-variable-p s) "u" "v")
      (and (local-variable-if-set-p s) "'")
      (and (ignore-errors (not (equal (symbol-value s) (default-value s)))) "*")
      (and (get s 'byte-obsolete-variable) "-")))
   (and (facep s) "a")
   (and (fboundp 'cl-find-class) (cl-find-class s) "t")))

(defun help--symbol-completion-table-affixation (completions)
  (mapcar (lambda (c)
            (let* ((s (intern c))
                   (doc (condition-case nil (documentation s) (error nil)))
                   (doc (and doc (substring doc 0 (string-search "\n" doc)))))
              (list c (propertize
                       (format "%-4s" (help--symbol-class s))
                       'face 'completions-annotations)
                    (if doc (propertize (format " -- %s" doc)
                                        'face 'completions-annotations)
                      ""))))
          completions))

(defun help--symbol-completion-table (string pred action)
  (if (eq action 'metadata)
      `(metadata
        ,@(when completions-detailed
            '((affixation-function . help--symbol-completion-table-affixation)))
        (category . symbol-help))
    (when (and help-enable-completion-autoload
               (memq action '(nil t lambda)))
      (let ((prefixes (radix-tree-prefixes (help-definition-prefixes) string)))
        ;; Don't load FOO.el during `test-completion' of `FOO-'.
        (unless (and (eq action 'lambda) (assoc string prefixes))
          (help--load-prefixes prefixes))))
    (let ((prefix-completions
           (and help-enable-completion-autoload
                (mapcar #'intern (all-completions string definition-prefixes)))))
      (complete-with-action action obarray string
                            (if pred (lambda (sym)
                                       (or (funcall pred sym)
                                           (memq sym prefix-completions))))))))

(defvar describe-function-orig-buffer nil
  "Buffer that was current when `describe-function' was invoked.
Functions on `help-fns-describe-function-functions' can use this
to get buffer-local values.")

(defun help-fns--describe-function-or-command-prompt (&optional want-command)
  "Prompt for a function from `describe-function' or `describe-command'.
If optional argument WANT-COMMAND is non-nil, prompt for an
interactive command."
  (let* ((fn (if want-command
                 (caar command-history)
               (function-called-at-point)))
         (prompt (format-prompt (if want-command
                                    "Describe command"
                                  "Describe function")
                                fn))
         (enable-recursive-minibuffers t)
         (val (completing-read
               prompt
               #'help--symbol-completion-table
               (lambda (f) (if want-command
                          (commandp f)
                        (or (fboundp f) (get f 'function-documentation))))
               ;; We used `confirm' for a while because we may want to see the
               ;; meta-info about a function even if the function itself is not
               ;; defined, but this use case is too marginal and rarely tested,
               ;; not worth the trouble (bug#64902).
               t nil nil
               (and fn (symbol-name fn)))))
    (unless (equal val "")
      (setq fn (intern val)))
    ;; These error messages are intended to be less technical for the
    ;; `describe-command' case, as they are directed at users that are
    ;; not necessarily Elisp programmers.
    (unless (and fn (symbolp fn))
      (user-error (if want-command
                      "You didn't specify a command's symbol"
                    "You didn't specify a function symbol")))
    (unless (or (fboundp fn) (get fn 'function-documentation))
      (user-error (if want-command
                      "Symbol is not a command: %s"
                    "Symbol's function definition is void: %s")
                  fn))
    (list fn)))

(declare-function project-combine-directories "project" (&rest lists))

(cl-defmethod xref-backend-references ((_backend (eql 'elisp)) identifier
                                       &context (major-mode help-mode))
  (mapcan
   (lambda (dir)
     (message "Searching %s..." dir)
     (redisplay)
     (prog1
         (xref-references-in-directory identifier dir)
       (message "Searching %s... done" dir)))
   (project-combine-directories (elisp-load-path-roots))))

(defun help-fns--setup-xref-backend ()
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t)
  (setq-local semantic-symref-filepattern-alist '((help-mode "*.el"))))

;;;###autoload
(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol).
When called from Lisp, FUNCTION may also be a function object.

See the `help-enable-symbol-autoload' variable for special
handling of autoloaded functions."
  (interactive (help-fns--describe-function-or-command-prompt))

  ;; We save describe-function-orig-buffer on the help xref stack, so
  ;; it is restored by the back/forward buttons.  'help-buffer'
  ;; expects (current-buffer) to be a help buffer when processing
  ;; those buttons, so we can't change the current buffer before
  ;; calling that.
  (let ((describe-function-orig-buffer
         (or describe-function-orig-buffer
             (current-buffer)))
        (help-buffer-under-preparation t))

    (help-setup-xref (list #'describe-function--helper
                           function describe-function-orig-buffer)
                     (called-interactively-p 'interactive))

    (save-excursion
      (with-help-window (help-buffer)
        (if (get function 'reader-construct)
            (princ function)
          (prin1 function))
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is ")
        (describe-function-1 function)
        (with-current-buffer standard-output
          (help-fns--setup-xref-backend)

          ;; Return the text we displayed.
          (buffer-string))))))

;;;###autoload
(defun help-find-source ()
  "Switch to a buffer visiting the source of what is being described in *Help*."
  (interactive)
  (if-let* ((help-buffer (get-buffer "*Help*")))
      (with-current-buffer help-buffer
          (help-view-source))
    (error "No *Help* buffer found")))

;;;###autoload
(defun describe-command (command)
  "Display the full documentation of COMMAND (a symbol).
When called from Lisp, COMMAND may also be a function object."
  (interactive (help-fns--describe-function-or-command-prompt 'is-command))
  (describe-function command))

;; Could be this, if we make symbol-file do the work below.
;; (defun help-C-file-name (subr-or-var kind)
;;   "Return the name of the C file where SUBR-OR-VAR is defined.
;; KIND should be `var' for a variable or `subr' for a subroutine."
;;   (symbol-file (if (symbolp subr-or-var) subr-or-var
;; 		 (subr-name subr-or-var))
;; 	       (if (eq kind 'var) 'defvar 'defun)))
;;;###autoload
(defun help-C-file-name (subr-or-var kind)
  "Return the name of the C file where SUBR-OR-VAR is defined.
KIND should be `var' for a variable or `subr' for a subroutine.
If we can't find the file name, nil is returned."
  (let ((docbuf (get-buffer-create " *DOC*"))
	(name (if (eq 'var kind)
		  (concat "V" (symbol-name subr-or-var))
		(concat "F" (if (symbolp subr-or-var)
                                (symbol-name subr-or-var)
                              (subr-name (advice--cd*r subr-or-var)))))))
    (with-current-buffer docbuf
      (goto-char (point-min))
      (if (eobp)
	  (insert-file-contents-literally
	   (expand-file-name internal-doc-file-name doc-directory)))
      (let ((file (catch 'loop
		    (while t
		      (let ((pnt (search-forward (concat "\^_" name "\n")
                                                 nil t)))
                        (if (not pnt)
                            (throw 'loop nil)
			  (re-search-backward "\^_S\\(.*\\)")
			  (let ((file (match-string 1)))
			    (if (member file build-files)
			        (throw 'loop file)
			      (goto-char pnt)))))))))
        (if (not file)
            nil
	  (if (string-match "^ns.*\\(\\.o\\|obj\\)\\'" file)
	      (setq file (replace-match ".m" t t file 1))
	    (if (string-match "\\.\\(o\\|obj\\)\\'" file)
	        (setq file (replace-match ".c" t t file))))
	  (if (string-match "\\.\\(c\\|m\\)\\'" file)
	      (concat "src/" file)
	    file))))))

(defcustom help-downcase-arguments nil
  "If non-nil, argument names in *Help* buffers are downcased."
  :type 'boolean
  :group 'help
  :version "23.2")

(defun help-highlight-arg (arg)
  "Highlight ARG as an argument name for a *Help* buffer.
Return ARG in face `help-argument-name'; ARG is also downcased
if the variable `help-downcase-arguments' is non-nil."
  (propertize (if help-downcase-arguments (downcase arg) arg)
	      'face 'help-argument-name))

(defun help-do-arg-highlight (doc args &optional usage-p)
  (with-syntax-table (make-syntax-table emacs-lisp-mode-syntax-table)
    (modify-syntax-entry ?\- "w")
    (dolist (arg args)
      (setq doc (replace-regexp-in-string
                 ;; This is heuristic, but covers all common cases
                 ;; except ARG1-ARG2
                 (concat (when usage-p
                           ;; Skip function name in usage string
                           ;; (Bug#65580).
                           "([^ ]+ .*")
                         "\\<"                   ; beginning of word
                         "\\(?:[a-z-]*-\\)?"     ; for xxx-ARG
                         "\\("
                         (regexp-quote arg)
                         "\\)"
                         "\\(?:es\\|s\\|th\\)?"  ; for ARGth, ARGs
                         "\\(?:-[a-z0-9-]+\\)?"  ; for ARG-xxx, ARG-n
                         "\\(?:-[{([<`\"‘].*?\\)?"; for ARG-{x}, (x), <x>, [x], `x', ‘x’
                         "\\>")                  ; end of word
                 (help-highlight-arg arg)
                 doc t t 1)))
    doc))

(defun help-highlight-arguments (usage doc &rest args)
  (when (and usage (string-match "^(" usage))
    (with-temp-buffer
      (insert usage)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (next (not (or args (looking-at "\\["))))
            (opt nil))
        ;; Make a list of all arguments
        (skip-chars-forward "^ ")
        (while next
          (or opt (not (looking-at " &")) (setq opt t))
          (if (not (re-search-forward " \\([\\[(]*\\)\\([^] &).]+\\)" nil t))
              (setq next nil)
            (setq args (cons (match-string 2) args))
            (when (and opt (string= (match-string 1) "("))
              ;; A pesky CL-style optional argument with default value,
              ;; so let's skip over it
              (search-backward "(")
              (goto-char (scan-sexps (point) 1)))))
        ;; Highlight arguments in the USAGE string
        (setq usage (help-do-arg-highlight (buffer-string) args t))
        ;; Highlight arguments in the DOC string
        (setq doc (and doc (help-do-arg-highlight doc args))))))
  ;; Return value is like the one from help-split-fundoc, but highlighted
  (cons usage doc))

;; The following function was compiled from the former functions
;; `describe-simplify-lib-file-name' and `find-source-lisp-file' with
;; some excerpts from `describe-function-1' and `describe-variable'.
;; The only additional twists provided are (1) locate the defining file
;; for autoloaded functions, and (2) give preference to files in the
;; "install directory" (directories found via `load-path') rather than
;; to files in the "compile directory" (directories found by searching
;; the loaddefs.el file).  We autoload it because it's also used by
;; `describe-face' (instead of `describe-simplify-lib-file-name').

;;;###autoload
(defun find-lisp-object-file-name (object type &optional also-c-source)
  "Guess the file that defined the Lisp object OBJECT, of type TYPE.
OBJECT should be a symbol associated with a function, variable, or face;
  alternatively, it can be a function definition.
If TYPE is `defvar', search for a variable definition.
If TYPE is `defface', search for a face definition.
If TYPE is not a symbol, search for a function definition.

The return value is the absolute name of a readable file where OBJECT is
defined.  If several such files exist, preference is given to a file
found via `load-path'.  The return value can also be `C-source', which
means that OBJECT is a function or variable defined in C, but
it's currently unknown where.  If no suitable file is found,
return nil.

If ALSO-C-SOURCE is non-nil, instead of returning `C-source',
this function will attempt to locate the definition of OBJECT in
the C sources, too."
  (let* ((autoloaded (autoloadp type))
	 (file-name (or (and autoloaded (nth 1 type))
			(symbol-file
                         ;; FIXME: Why do we have this weird "If TYPE is the
                         ;; value returned by `symbol-function' for a function
                         ;; symbol" exception?
			 object (or (if (symbolp type) type) 'defun)))))
    (cond
     (autoloaded
      ;; An autoloaded function: Locate the file since `symbol-function'
      ;; has only returned a bare string here.
      (setq file-name
	    (locate-file file-name load-path '(".el" ".elc") 'readable)))
     ((and (stringp file-name)
	   (string-match "[.]*loaddefs.elc?\\'" file-name))
      ;; An autoloaded variable or face.  Visit loaddefs.el in a buffer
      ;; and try to extract the defining file.  The following form is
      ;; from `describe-function-1' and `describe-variable'.
      (let ((location
	     (condition-case nil
		 (find-function-search-for-symbol object nil file-name)
	       (error nil))))
	(when (cdr location)
	  (with-current-buffer (car location)
	    (goto-char (cdr location))
	    (when (re-search-backward
		   "^;;; Generated autoloads from \\(.*\\)" nil t)
	      (setq file-name
		    (locate-file
		     (file-name-sans-extension
		      (match-string-no-properties 1))
		     load-path '(".el" ".elc") 'readable))))))))

    (cond
     ((and (not file-name)
           (subrp type)
           (not (native-comp-function-p type)))
      ;; A built-in function.  The form is from `describe-function-1'.
      (if (or (get-buffer " *DOC*")
              (and also-c-source
                   (get-buffer-create " *DOC*")))
	  (help-C-file-name type 'subr)
	'C-source))
     ((and (not file-name) (symbolp object)
           (eq type 'defvar)
	   (integerp (get object 'variable-documentation)))
      ;; A variable defined in C.  The form is from `describe-variable'.
      (if (or (get-buffer " *DOC*")
              (and also-c-source
                   (get-buffer-create " *DOC*")))
	  (help-C-file-name object 'var)
	'C-source))
     ((not (stringp file-name))
      ;; If we don't have a file-name string by now, we lost.
      nil)
     ;; Now, `file-name' should have become an absolute file name.
     ;; For files loaded from ~/.foo.elc, try ~/.foo.
     ;; This applies to config files like ~/.emacs,
     ;; which people sometimes compile.
     ((let (fn)
	(and (string-match "\\`\\..*\\.elc\\'"
			   (file-name-nondirectory file-name))
	     (string-equal (file-name-directory file-name)
			   (file-name-as-directory (expand-file-name "~")))
	     (file-readable-p (setq fn (file-name-sans-extension file-name)))
	     fn)))
     ;; When the Elisp source file can be found in the install
     ;; directory, return the name of that file.
     ((let ((lib-name
	     (if (string-match "[.]elc\\'" file-name)
		 (substring-no-properties file-name 0 -1)
	       file-name)))
	(or (and (file-readable-p lib-name) lib-name)
	    ;; The library might be compressed.
	    (and (file-readable-p (concat lib-name ".gz")) lib-name))))
     ((let* ((lib-name (file-name-nondirectory file-name))
	     ;; The next form is from `describe-simplify-lib-file-name'.
	     (file-name
	      ;; Try converting the absolute file name to a library
	      ;; name, convert that back to a file name and see if we
	      ;; get the original one.  If so, they are equivalent.
	      (if (equal file-name (locate-file lib-name load-path '("")))
		  (if (string-match "[.]elc\\'" lib-name)
		      (substring-no-properties lib-name 0 -1)
		    lib-name)
		file-name))
	     (src-file (locate-library file-name t nil 'readable)))
	(and src-file (file-readable-p src-file) src-file))))))

(defun help-fns--key-bindings (function)
  (when (commandp function)
    (let ((pt2 (with-current-buffer standard-output (point)))
          (remapped (command-remapping function)))
      (unless (memq remapped '(ignore undefined))
        (let* ((all-keys
                (with-current-buffer
                    (or describe-function-orig-buffer (current-buffer))
                  (where-is-internal
                   (or remapped function) overriding-local-map nil nil)))
               (seps (seq-group-by
                      (lambda (key)
                        (and (vectorp key)
                             (eq (elt key 0) 'menu-bar)))
                      all-keys))
               (keys (cdr (assq nil seps)))
               (menus (cdr (assq t seps)))
               non-modified-keys)
          (if (and (eq function 'self-insert-command)
                   (vectorp (car-safe keys))
                   (consp (aref (car keys) 0)))
              (princ "It is bound to many ordinary text characters.\n")
            ;; Which non-control non-meta keys run this command?
            (dolist (key keys)
              (if (member (event-modifiers (aref key 0)) '(nil (shift)))
                  (push key non-modified-keys)))
            (when remapped
              (princ "Its keys are remapped to ")
              (princ (if (symbolp remapped)
                         (format-message "`%s'" remapped)
		       "an anonymous command"))
              (princ ".\n"))

            (when keys
              (princ (if remapped
                         "Without this remapping, it would be bound to "
                       "It is bound to "))
              ;; If lots of ordinary text characters run this command,
              ;; don't mention them one by one.
              (if (< (length non-modified-keys) 10)
                  (with-current-buffer standard-output
                    (help-fns--insert-bindings keys))
                (dolist (key non-modified-keys)
                  (setq keys (delq key keys)))
                (if keys
                    (with-current-buffer standard-output
                      (help-fns--insert-bindings keys)
                      (insert ", and many ordinary text characters"))
                  (princ "many ordinary text characters."))))
            (when (or remapped keys non-modified-keys)
              (princ ".")
              (terpri)))

          (with-current-buffer standard-output
            (fill-region-as-paragraph pt2 (point))
            (unless (bolp)
              (insert "\n"))
            (when menus
              (let ((start (point)))
                (help-fns--insert-menu-bindings
                 menus
                 (concat "It " (if remapped "could " "can ") (and keys "also ")
                         "be invoked from the menu: "))
                (when remapped
                  (princ ", but that was remapped to ")
                  (princ (if (symbolp remapped)
                             (format-message "`%s'" remapped)
		           "an anonymous command"))
                  (princ "as well.\n"))
                ;; The (= (point) start) condition tests whether
                ;; 'help-fns--insert-menu-bindings' inserted anything;
                ;; if it didn't, we already have a period from the
                ;; previous 'princ' call.
                (or remapped (= (point) start) (princ "."))
                (fill-region-as-paragraph start (point))))
            (ensure-empty-lines)))))))

(defun help-fns--insert-bindings (keys)
  (seq-do-indexed (lambda (key i)
                    (insert
                     (cond ((zerop i) "")
                           ((= i (1- (length keys))) " and ")
                           (t ", ")))
                    (insert (help--key-description-fontified key)))
                  keys))

(defun help-fns--insert-menu-bindings (menus heading)
  (let ((strings nil))
    ;; First collect all the printed representations of menus.
    (dolist (menu menus)
      (let ((map (lookup-key global-map (seq-take menu 1)))
            (string nil)
            (sep (if (char-displayable-p ?→) " → " " => ")))
        (seq-do-indexed
         (lambda (entry level)
           (when (symbolp map)
             (setq map (symbol-function map)))
           (when-let* ((elem (assq entry (cdr map)))
                       (_ (proper-list-p elem)))
             (when (> level 0)
               (push sep string))
             (if (eq (nth 1 elem) 'menu-item)
                 (progn
                   (push (propertize (nth 2 elem) 'face 'help-key-binding)
                         string)
                   (setq map (cadddr elem)))
               (push (propertize (nth 1 elem) 'face 'help-key-binding)
                     string)
               (setq map (cddr elem)))))
         (cdr (seq-into menu 'list)))
        (when string
          (push string strings))))
    ;; Then output them.
    (when strings
      (when heading
        (insert heading))
      (seq-do-indexed
       (lambda (string i)
         (insert
          (cond ((zerop i) "")
                ((= i (1- (length menus))) " and ")
                (t ", "))
          (string-join (nreverse string))))
       strings))))

(defun help-fns--compiler-macro (function)
  (pcase-dolist (`(,type . ,handler)
                 (list (cons "compiler macro"
                             (function-get function 'compiler-macro))
                       (cons (substitute-command-keys "`byte-compile' property")
                             (function-get function 'byte-compile))
                       (cons "byte-code optimizer"
                             (function-get function 'byte-optimizer))))
    (when handler
      (if (bolp)
          (insert "  This function has a ")
        (insert " and a "))
      (insert type)
      (if (symbolp handler)
          (progn
            (insert (format-message " `%s'" handler))
            (save-excursion
              (re-search-backward (substitute-command-keys "`\\([^`']+\\)'"))
              (help-xref-button 1 'help-function handler)))
        ;; FIXME: Obsolete since 24.4.
        (let ((lib (get function 'compiler-macro-file)))
          (when (stringp lib)
            (insert (format-message " in `%s'" lib))
            (save-excursion
              (re-search-backward (substitute-command-keys "`\\([^`']+\\)'"))
              (help-xref-button 1 'help-function-cmacro function lib)))))))
  (unless (bolp)
    (insert ".  See "
            (buttonize "the manual"
                       (lambda (_) (info "(elisp)Advice and Byte Code")))
            " for details.\n")
    (save-restriction
      (let ((fill-prefix "    "))
        (narrow-to-region (line-beginning-position -1) (point))
        (fill-region (point-min) (point-max)))
      (goto-char (point-max)))))

(defun help-fns--signature (function doc real-def real-function buffer)
  "Insert usage at point and return docstring.  With highlighting."
  (if (keymapp function)
      doc                       ; If definition is a keymap, skip arglist note.
    (let* ((advertised (get-advertised-calling-convention real-def))
           (arglist (if (listp advertised)
                        advertised (help-function-arglist real-def)))
           (usage (help-split-fundoc doc function)))
      (if usage (setq doc (cdr usage)))
      (let* ((use (cond
                   ((and usage (not (listp advertised))) (car usage))
                   ((listp arglist)
                    (help--make-usage-docstring function arglist))
                   ((stringp arglist) arglist)
                   ;; Maybe the arglist is in the docstring of a symbol
                   ;; this one is aliased to.
                   ((let ((fun real-function))
                      (while (and (symbolp fun)
                                  (setq fun (symbol-function fun))
                                  (not (setq usage (help-split-fundoc
                                                    (documentation fun)
                                                    function)))))
                      usage)
                    (car usage))
                   ((or (stringp real-def)
                        (vectorp real-def))
                    (format "\nMacro: %s"
                            (help--docstring-quote
                             (format-kbd-macro real-def))))
                   (t "[Missing arglist.]")))
             ;; Insert "`X", not "(\` X)", when documenting `X.
             (use1 (replace-regexp-in-string
                    "\\`(\\\\=\\\\\\\\=` \\([^\n ]*\\))\\'"
                    "\\\\=`\\1" use t))
             (high (if buffer
                       (let (subst-use1 subst-doc)
                         (with-current-buffer buffer
                           (setq subst-use1 (substitute-command-keys use1))
                           (setq subst-doc (substitute-command-keys doc)))
                         (help-highlight-arguments subst-use1 subst-doc))
                     (cons use1 doc))))
        (let ((fill-begin (point))
              (high-usage (car high))
              (high-doc (cdr high)))
          (unless (and (symbolp function)
                       (get function 'reader-construct))
            (insert high-usage "\n"))
          (fill-region fill-begin (point))
          (when-let* (help-display-function-type
                      (res (comp-function-type-spec function))
                      (type-spec (car res))
                      (kind (cdr res)))
            (insert (if (eq kind 'inferred)
                        "\nInferred type:\n  "
                      "\nDeclared type:\n  "))
            (with-demoted-errors "%S"
              (let ((beg (point)))
                (pp type-spec (current-buffer))
                ;; Put it on a single line if it fits.
                (and (eql beg (+ 2 (line-beginning-position 0)))
                     (save-excursion
                       (forward-char -1)
                       (<= (current-column) (- fill-column 12)))
                     (replace-region-contents (- beg 3) beg " " 0)))))
          high-doc)))))

(defun help-fns--parent-mode (function)
  ;; If this is a derived mode, link to the parent.
  (when (symbolp function)
    (let ((parent-mode (get function 'derived-mode-parent))
          (extra-parents (get function 'derived-mode-extra-parents)))
    (when parent-mode
      (insert (substitute-quotes "  Parent mode: `"))
      (let ((beg (point)))
        (insert (format "%S" parent-mode))
        (make-text-button beg (point)
                          'type 'help-function
                          'help-args (list parent-mode)))
      (insert (substitute-quotes "'.\n")))
    (when extra-parents
      (insert (format "  Extra parent mode%s:" (if (cdr extra-parents) "s" "")))
      (dolist (parent extra-parents)
        (insert (substitute-quotes " `"))
        (let ((beg (point)))
          (insert (format "%S" parent))
          (make-text-button beg (point)
                            'type 'help-function
                            'help-args (list parent)))
        (insert (substitute-quotes "'")))
      (insert ".\n")))))

(defun help-fns--obsolete (function)
  ;; Ignore lambda constructs, keyboard macros, etc.
  (let* ((obsolete (and (symbolp function)
			(get function 'byte-obsolete-info)))
         (use (car obsolete))
         (start (point)))
    (when obsolete
      (insert "This "
	      (if (eq (car-safe (symbol-function function)) 'macro)
		  "macro"
		"function")
	      " is obsolete")
      (when (nth 2 obsolete)
        (insert (format " since %s" (nth 2 obsolete))))
      (insert (cond ((stringp use) (concat "; " (substitute-quotes use)))
                    (use (format-message "; use `%s' instead." use))
                    (t "."))
              "\n")
      (fill-region-as-paragraph start (point))
      (ensure-empty-lines))))

(add-hook 'help-fns-describe-function-functions
          #'help-fns--globalized-minor-mode)
(defun help-fns--globalized-minor-mode (function)
  (when (and (symbolp function)
             (get function 'globalized-minor-mode))
    (help-fns--customize-variable function " the global mode variable.")
    (terpri)))

;; We could use `symbol-file' but this is a wee bit more efficient.
(defun help-fns--autoloaded-p (function)
  "Return non-nil if FUNCTION has previously been autoloaded."
  (seq-some #'autoloadp (get function 'function-history)))

(defun help-fns--interactive-only (function)
  "Insert some help blurb if FUNCTION should only be used interactively."
  ;; Ignore lambda constructs, keyboard macros, etc.
  (and (symbolp function)
       (not (eq (car-safe (symbol-function function)) 'macro))
       (let* ((interactive-only
               (or (function-get function 'interactive-only)
                   (if (boundp 'byte-compile-interactive-only-functions)
                       (memq function
                             byte-compile-interactive-only-functions)))))
         (when interactive-only
           (insert "  This function is for interactive use only"
                   ;; Cf byte-compile-form.
                   (cond ((stringp interactive-only)
                          (format ";\n  in Lisp code %s" interactive-only))
                         ((and (symbolp interactive-only)
                               (not (eq interactive-only t)))
                          (format-message ";\n  in Lisp code use `%s' instead."
                                          interactive-only))
                         (t "."))
                   "\n")))))

(add-hook 'help-fns-describe-function-functions #'help-fns--side-effects)
(defun help-fns--side-effects (function)
  (when (and (symbolp function)
             (or (function-get function 'pure)
                 (function-get function 'side-effect-free)))
    (insert "  This function does not change global state, "
            "including the match data.\n")))

(add-hook 'help-fns-describe-function-functions #'help-fns--disabled)
(defun help-fns--disabled (function)
  (when (and (symbolp function)
             (function-get function 'disabled))
    (insert "  This function is disabled.\n")))

(defun help-fns--first-release-regexp (symbol)
  (let* ((name (symbol-name symbol))
         (quoted (regexp-quote name)))
    ;; We used to use just (concat "\\_<" (regexp-quote name) "\\_>"),
    ;; which had the advantage of adapting to the various notational
    ;; conventions we've used over the years in etc/NEWS*, but it was also
    ;; leading to many false positives.  So we use a more restrictive regexp
    ;; now (which can still lead to false positives, e.g. because we don't
    ;; distinguish between occurrences of the same symbol for
    ;; different purposes, such as function name, var name, face name,
    ;; property name, ...).
    (concat
     ;; The main "canonical" occurrence of symbols is within '...'.
     "'" quoted "'"
     ;; Commands can also occur as `M-x blabla'.
     "\\|M-x[ \t\n]+" quoted "\\_>"
     ;; Other times we do '<key>' (<cmdname>).
     "\\|(" quoted ")"
     ;; Finally other times we just include sample code, which we will
     ;; only recognize if it's indented by at least 2 spaces and start with
     ;; an open paren.
     "\\|^\\(?:  \\|\t\\)[ \t]*(\\(.*[( ']\\)?" quoted "\\_>")
    ))


(defun help-fns--first-release-override (symbol type)
  "The first release defining SYMBOL of TYPE, or nil.
TYPE indicates the namespace and is `fun' or `var'."
  (let* ((sym-rel-file (expand-file-name "symbol-releases.eld" data-directory))
         (tuples
          (with-temp-buffer
            (ignore-errors
              (insert-file-contents sym-rel-file)
              (goto-char (point-min))
              (read (current-buffer))))))
    (unless (cl-every (lambda (x) (and (= (length x) 3) (stringp (car x))))
                      tuples)
      (error "Bad %s format" sym-rel-file))
    (car (rassoc (list type symbol) tuples))))

(defun help-fns--first-release (symbol)
  "Return the likely first release that defined SYMBOL, or nil."
  ;; Code below relies on the etc/NEWS* files.
  ;; FIXME: Maybe we should also use the */ChangeLog* files when available.
  ;; FIXME: Maybe we should also look for announcements of the addition
  ;; of the *packages* in which the function is defined.
  (let* ((re (help-fns--first-release-regexp symbol))
         (news (directory-files data-directory t "\\`NEWS\\(\\'\\|\\.\\)"))
         (case-fold-search nil)
         (place nil)
         (first nil))
    (with-temp-buffer
      (dolist (f news)
        (erase-buffer)
        (insert-file-contents f)
        (goto-char (point-min))
        ;; Failed git merges can leave empty files that look like NEWS
        ;; in etc.  Don't error here.
        (when (search-forward "\n*" nil t)
          (while (re-search-forward re nil t)
            (let ((pos (match-beginning 0)))
              (save-excursion
                ;; Almost all entries are of the form "* ... in Emacs NN.MM."
                ;; but there are also a few in the form "* Emacs NN.MM is a bug
                ;; fix release ...".
                (if (not (re-search-backward
                          "^\\* \\(?:.* \\)?Emacs \\([0-9.]+[0-9]\\)"
                          nil t))
                    (message "Ref to %S found in non-versioned section in %S"
                             symbol (file-name-nondirectory f))
                  (let ((version (match-string 1)))
                    (when (or (null first) (version< version first))
                      (setq place (list f pos))
                      (setq first version))))))))))
    (when first
      (make-text-button first nil 'type 'help-news 'help-args place))))

(add-hook 'help-fns-describe-function-functions
          #'help-fns--mention-first-function-release)
(add-hook 'help-fns-describe-variable-functions
          #'help-fns--mention-first-variable-release)

(defun help-fns--mention-first-function-release (object)
  (help-fns--mention-first-release object 'fun))

(defun help-fns--mention-first-variable-release (object)
  ;; Don't output anything if we've already output the :version from
  ;; the `defcustom'.
  (unless (memq 'help-fns--customize-variable-version
                help-fns--activated-functions)
    (help-fns--mention-first-release object 'var)))

(defun help-fns--mention-first-release (object type)
  (when (symbolp object)
    (when-let* ((first (or (help-fns--first-release-override object type)
                           (help-fns--first-release object))))
      (with-current-buffer standard-output
        (insert (format "  Probably introduced at or before Emacs version %s.\n"
                        first))))))

(declare-function shortdoc-display-group "shortdoc")
(declare-function shortdoc-function-groups "shortdoc")

(add-hook 'help-fns-describe-function-functions
          #'help-fns--mention-shortdoc-groups)
(defun help-fns--mention-shortdoc-groups (object)
  (require 'shortdoc)
  (when-let* ((groups (and (symbolp object)
                           (shortdoc-function-groups object))))
    (let ((start (point))
          (times 0))
      (with-current-buffer standard-output
        (insert "  Other relevant functions are documented in the ")
        (mapc
         (lambda (group)
           (when (> times 0)
             (insert (if (= times (1- (length groups)))
                         " and "
                       ", ")))
           (setq times (1+ times))
           (insert-text-button
            (symbol-name group)
            'action (lambda (_)
                      (shortdoc-display-group group object
                                              help-window-keep-selected))
            'follow-link t
            'help-echo "mouse-1, RET: show documentation group"))
         groups)
        (insert (if (= (length groups) 1)
                    " group.\n"
                  " groups.\n")))
      (save-restriction
        (narrow-to-region start (point))
        (fill-region-as-paragraph (point-min) (point-max))
        (goto-char (point-max))))))

(require 'radix-tree)

(defconst help-fns--radix-trees
  (make-hash-table :weakness 'key :test 'equal)
  "Cache of radix-tree representation of `load-path'.")

(defun help-fns--filename (file)
  (let ((f (abbreviate-file-name (expand-file-name file))))
    (if (file-name-case-insensitive-p f) (downcase f) f)))

(defun help-fns--radix-tree (dirs)
  (with-memoization (gethash dirs help-fns--radix-trees)
    (let ((rt radix-tree-empty))
      (dolist (d dirs)
        (let ((d (help-fns--filename (file-name-as-directory d))))
          (setq rt (radix-tree-insert rt d t))))
      rt)))

(defun help-fns-short-filename (filename)
  "Turn a full Elisp file name to one relative to `load-path'."
  (if (not (file-name-absolute-p filename))
      ;; This happens when FILENAME is a `src/*.c' returned by
      ;; `help-C-file-name', in which case it's not searched through
      ;; `load-path' anyway (bug#74504).
      ;; Even if it came from elsewhere it's not clear to
      ;; which directory it is relative.
      filename
    (let* ((short (help-fns--filename filename))
           (prefixes (radix-tree-prefixes (help-fns--radix-tree load-path)
                                          (file-name-directory short))))
      (if (not prefixes)
          ;; The file is not inside the `load-path'.
          ;; FIXME: Here's the old code (too slow, bug#73766),
          ;; which used to try and shorten it with "../" as well.
          ;; (dolist (dir load-path)
          ;;   (let ((rel (file-relative-name filename dir)))
          ;;     (if (< (length rel) (length short))
          ;;         (setq short rel)))
          ;;   (let ((rel (file-relative-name abbrev dir)))
          ;;     (if (< (length rel) (length short))
          ;;         (setq short rel))))
          short
        (file-relative-name short (caar prefixes))))))

(defun help-fns--analyze-function (function)
  ;; FIXME: Document/explain the differences between FUNCTION,
  ;; REAL-FUNCTION, DEF, and REAL-DEF.
  "Return information about FUNCTION.
Returns a list of the form (REAL-FUNCTION DEF ALIASED REAL-DEF)."
  (let* ((advised (and (symbolp function)
		       (advice--p (advice--symbol-function function))))
	 ;; If the function is advised, use the symbol that has the
	 ;; real definition, if that symbol is already set up.
	 (real-function
	  (or (and advised
                   (advice--cd*r (advice--symbol-function function)))
	      function))
	 ;; Get the real definition, if any.
	 (def (if (symbolp real-function)
                  (cond ((symbol-function real-function))
                        ((get real-function 'function-documentation)
                         nil)
                        (t (signal 'void-function (list real-function))))
		real-function))
	 (aliased (and def
                       (or (symbolp def)
                           ;; Advised & aliased function.
                           (and advised (symbolp real-function)
                                (not (eq 'autoload (car-safe def))))
                           (and (subrp def) (symbolp function)
                                (not (string= (subr-name def)
                                              (symbol-name function)))))))
	 (real-def (cond
                    ((and aliased (not (subrp def)))
                     (or (car (function-alias-p real-function))
                         real-function))
		    ((subrp def) (intern (subr-name def)))
                    (t def))))

    ;; If we don't have a doc string, then try to load the file.
    (when (and help-enable-symbol-autoload
               (autoloadp real-def)
               ;; Empty documentation slot.
               (not (nth 2 real-def)))
      (condition-case err
          (autoload-do-load real-def)
        (error (message "Error while autoloading: %S" err))))

    (list real-function def aliased real-def)))

(defun help-fns-function-description-header (function)
  "Print a line describing FUNCTION to `standard-output'."
  (pcase-let* ((`(,_real-function ,def ,aliased ,real-def)
                (help-fns--analyze-function function))
               (file-name (find-lisp-object-file-name
                           function (if aliased 'defun def)))
               (beg (if (and (or (functionp def)
                                 (keymapp def)
                                 (eq (car-safe def) 'macro))
                             (stringp file-name)
                             (help-fns--autoloaded-p function))
                        (concat
                         "an autoloaded " (if (commandp def)
                                              "interactive "))
                      (if (commandp def) "an interactive " "a ")))
               ;; Print what kind of function-like object FUNCTION is.
               (description
		(cond ((or (stringp def) (vectorp def))
		  "a keyboard macro")
		 ((and (symbolp function)
                       (get function 'reader-construct))
                  "a reader construct")
		 ;; Aliases are Lisp functions, so we need to check
		 ;; aliases before functions.
		 (aliased
		  (format-message "an alias for `%s'" real-def))
		 ((autoloadp def)
		  (format "an autoloaded %s"
                          (cond
			   ((commandp def) "interactive Lisp function")
			   ((eq (nth 4 def) 'keymap) "keymap")
			   ((nth 4 def) "Lisp macro")
                           (t "Lisp function"))))
		 ((or (eq (car-safe def) 'macro)
		      ;; For advised macros, def is a lambda
		      ;; expression or a compiled-function-p, so we
		      ;; need to check macros before functions.
		      (macrop function))
		  (concat beg "Lisp macro"))
		 ((keymapp def)
		  (let ((is-full nil)
			(elts (cdr-safe def)))
		    (while elts
		      (if (char-table-p (car-safe elts))
			  (setq is-full t
				elts nil))
		      (setq elts (cdr-safe elts)))
		    (concat beg (if is-full "keymap" "sparse keymap"))))
		 (t
                  (let* ((type (or (oclosure-type def) (cl-type-of def)))
                         (typ-str
                          (format "%s"
		                  (if (and (consp def) (symbolp (car def)))
                                      (car def)
                                    (make-text-button
                                       (symbol-name type) nil
                                       'type 'help-type
                                       'help-args (list type))))))
                    ;; FIXME: If someday Emacs has a function type symbol
                    ;; like `unicode-function' or `hour-function', this
                    ;; will produce an ungrammatical string (bug#79469).
                    (concat (if (string-match-p "\\`[aeiou]" (symbol-name type))
                                "an "
                              beg)
                            typ-str))))))
    (with-current-buffer standard-output
      (insert description))

    (if (and aliased (not (fboundp real-def)))
	(princ ",\nwhich is not defined.")
      (with-current-buffer standard-output
	(save-excursion
	  (save-match-data
	    (when (re-search-backward (substitute-command-keys
                                       "alias for `\\([^`']+\\)'")
                                      nil t)
	      (help-xref-button 1 'help-function real-def)))))

      (if (not file-name)
	  (with-current-buffer standard-output
            (setq help-mode--current-data (list :symbol function)))
	;; We used to add .el to the file name,
	;; but that's completely wrong when the user used load-file.
	(princ (format-message " in `%s'"
                               (if (eq file-name 'C-source)
                                   "C source code"
                                 (help-fns-short-filename file-name))))
	;; Make a hyperlink to the library.
	(with-current-buffer standard-output
          (setq help-mode--current-data (list :symbol function
                                              :file file-name))
	  (save-excursion
            (re-search-backward (substitute-command-keys "`\\([^`']+\\)'"))
	    (help-xref-button 1 'help-function-def function file-name))))
      (princ "."))))

(defun help-fns--ensure-empty-line ()
  (unless (eolp) (insert "\n"))
  (unless (eq ?\n (char-before (1- (point)))) (insert "\n")))

;;;###autoload
(defun describe-function-1 (function)
  (let ((pt1 (with-current-buffer standard-output (point))))
    (help-fns-function-description-header function)
    (with-current-buffer standard-output
      (let ((inhibit-read-only t))
        (fill-region-as-paragraph
         (save-excursion
           (goto-char pt1)
           (forward-line 0)
           (point))
         (point)
         nil t)
        (ensure-empty-lines))))

  (help-fns--obsolete function)

  (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                (help-fns--analyze-function function))
               (doc-raw (condition-case nil
                            ;; FIXME: Maybe `documentation' should return nil
                            ;; for invalid functions i.s.o. signaling an error.
                            (documentation function t)
                          ;; E.g. an alias for a not yet defined function.
                          ((invalid-function void-function) nil))))

    ;; If the function is autoloaded, and its docstring has
    ;; key substitution constructs, load the library.
    (and (autoloadp real-def) doc-raw
         help-enable-autoload
         (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]\\|`.*'" doc-raw)
         (autoload-do-load real-def))

    (help-fns--key-bindings function)
    (with-current-buffer standard-output
      (let ((doc (condition-case nil
                     ;; FIXME: Maybe `help-fns--signature' should return `doc'
                     ;; for invalid functions i.s.o. signaling an error.
                     (help-fns--signature
                      function doc-raw
                      (if (subrp def) (indirect-function real-def) real-def)
                      real-function describe-function-orig-buffer)
                   ;; E.g. an alias for a not yet defined function.
                   ((invalid-function void-function) doc-raw))))
        (help-fns--ensure-empty-line)
        (insert (or doc "Not documented."))
        (help-fns--run-describe-functions
         help-fns-describe-function-functions function))
      ;; Avoid asking the user annoying questions if she decides
      ;; to save the help buffer, when her locale's codeset
      ;; isn't UTF-8.
      (unless (memq text-quoting-style '(straight grave))
        (set-buffer-file-coding-system 'utf-8)))))

;; Add defaults to `help-fns-describe-function-functions'.
(add-hook 'help-fns-describe-function-functions #'help-fns--interactive-only)
(add-hook 'help-fns-describe-function-functions #'help-fns--parent-mode)
(add-hook 'help-fns-describe-function-functions #'help-fns--compiler-macro 100)

(defun help-fns--generalized-variable (function)
  (when (and (symbolp function)
             (or (get function 'gv-expander)
                 ;; This is a hack, see cl-macs.el:
                 (get function 'document-generalized-variable))
             ;; Don't mention obsolete generalized variables.
             (not (get function 'byte-obsolete-generalized-variable)))
    (insert (format-message "  `%s' is also a " function)
            (buttonize "generalized variable"
                       (lambda (_) (info "(elisp)Generalized Variables")))
            ".\n")))
(add-hook 'help-fns-describe-function-functions
          #'help-fns--generalized-variable)


;; Variables

;;;###autoload
(defun variable-at-point (&optional any-symbol)
  "Return the bound variable symbol found at or before point.
Return 0 if there is no such symbol.
If ANY-SYMBOL is non-nil, don't insist the symbol be bound."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
	    (save-excursion
	      (skip-chars-forward "'")
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (boundp obj) obj)))
          (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (or any-symbol (boundp sym)))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (or any-symbol (boundp sym)) sym)))))
        0)))

(defun describe-variable-custom-version-info (variable &optional type)
  (let ((custom-version (get variable 'custom-version))
	(cpv (get variable 'custom-package-version))
        (type (or type "variable"))
	(output nil))
    (if custom-version
	(setq output
	      (format "  This %s was introduced, or its default value was changed, in\n  version %s of Emacs.\n"
                      type custom-version))
      (when cpv
	(let* ((package (car-safe cpv))
	       (version (if (listp (cdr-safe cpv))
			    (car (cdr-safe cpv))
			  (cdr-safe cpv)))
	       (pkg-versions (assq package customize-package-emacs-version-alist))
	       (emacsv (cdr (assoc version pkg-versions))))
	  (if (and package version)
	      (setq output
		    (format (concat "  This %s was introduced, or its default value was changed, in\n  version %s of the %s package"
				    (if emacsv
					(format " that is part of Emacs %s" emacsv))
				    ".\n")
			    type version package))))))
    output))

;;;###autoload
(defun describe-variable (variable &optional buffer frame)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER or FRAME
\(default to the current buffer and current frame),
it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
	 val)
     (setq val (completing-read
                (format-prompt "Describe variable" (and (symbolp v) v))
                #'help--symbol-completion-table
                (lambda (vv)
                  (or (get vv 'variable-documentation)
                      (and (not (keywordp vv))
                           ;; Since the variable may only exist in the
                           ;; original buffer, we have to look for it
                           ;; there.
                           (buffer-local-boundp vv orig-buffer))))
                t nil nil
                (if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (let (file-name
        (help-buffer-under-preparation t))
    (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
    (unless (frame-live-p frame) (setq frame (selected-frame)))
    (if (not (symbolp variable))
	(user-error "You didn't specify a variable")
      (save-excursion
	(let ((valvoid (not (with-current-buffer buffer (boundp variable))))
	      val val-start-pos locus)
	  ;; Extract the value before setting up the output buffer,
	  ;; in case `buffer' *is* the output buffer.
	  (unless valvoid
	    (with-selected-frame frame
	      (with-current-buffer buffer
		(setq val (symbol-value variable)
		      locus (variable-binding-locus variable)))))
	  (help-setup-xref (list #'describe-variable variable buffer)
			   (called-interactively-p 'interactive))
	  (with-help-window (help-buffer)
	    (with-current-buffer buffer
	      (prin1 variable)
	      (setq file-name (find-lisp-object-file-name variable 'defvar))

	      (princ (if file-name
		         (progn
		           (princ (format-message
                                   " is a variable defined in `%s'.\n\n"
                                   (if (eq file-name 'C-source)
                                       "C source code"
                                     (help-fns-short-filename file-name))))
		           (with-current-buffer standard-output
                             (setq help-mode--current-data
                                   (list :symbol variable
                                         :type (if (eq file-name 'C-source)
                                                   'variable
                                                 'defvar)
                                         :file file-name))
                             (save-excursion
			       (re-search-backward (substitute-command-keys
                                                    "`\\([^`']+\\)'"))
			       (help-xref-button 1 'help-variable-def
					         variable file-name)))
		           (if valvoid
			       "It is void as a variable."
                             "Its "))
	               (with-current-buffer standard-output
                         (setq help-mode--current-data (list :symbol variable
                                                             :type 'variable)))
                       (if valvoid
		           " is void as a variable."
                         (substitute-command-keys "'s ")))))
	    (unless valvoid
	      (with-current-buffer standard-output
		(setq val-start-pos (point))
		(princ "value is")
		(let ((line-beg (line-beginning-position))
		      (print-rep
		       (let ((rep
			      (let ((print-quoted t)
                                    (print-circle t))
                                (cl-prin1-to-string val))))
                         (if (and (symbolp val) (not (booleanp val)))
			     (format-message "`%s'" rep)
			   rep)))
                      (start (point)))
		 (let (beg)
		   (if (< (+ (length print-rep) (point) (- line-beg)) 68)
		       (progn
		         (setq beg (1+ (point)))
		         (insert " " print-rep))
		     (terpri)
                     (setq beg (point))
                     (let ((buf (current-buffer)))
                       (with-temp-buffer
                         (lisp-data-mode)
                         (insert print-rep)
                         (pp-buffer)
                         (font-lock-ensure)
                         (let ((pp-buffer (current-buffer)))
                           (with-current-buffer buf
                             (insert-buffer-substring pp-buffer))))))
                   ;; Remove trailing newline.
                   (and (= (char-before) ?\n) (delete-char -1))
                   ;; Put a `syntax-table' property on the data, as
                   ;; a kind of poor man's multi-major-mode support here.
                   (put-text-property beg (point)
		                      'syntax-table
		                      lisp-data-mode-syntax-table))
                  (help-fns--editable-variable start (point)
                                               variable val buffer)
		  (let* ((sv (get variable 'standard-value))
			 (origval (and (consp sv)
				       (condition-case nil
					   (custom--standard-value variable)
					 (error :help-eval-error))))
                         from)
		    (when (and (consp sv)
                               (not (equal origval val))
                               (not (equal origval :help-eval-error)))
		      (princ "\nOriginal value was \n")
		      (setq from (point))
		      (if (and (symbolp origval) (not (booleanp origval)))
			  (let* ((rep (cl-prin1-to-string origval))
				 (print-rep (format-message "`%s'" rep)))
			    (insert print-rep))
			(cl-prin1 origval))
                      (save-restriction
                        (narrow-to-region from (point))
                        (save-excursion (pp-buffer)))
                      (help-fns--editable-variable from (point)
                                                   variable origval buffer)
		      (if (< (point) (+ from 20))
			  (delete-region (1- from) from)))))))
	    (terpri)
	    (when locus
	      (cond
               ((bufferp locus)
                (princ (format "Local in buffer %s; "
                               (buffer-name buffer))))
               ((terminal-live-p locus)
                (princ "It is a terminal-local variable; "))
               (t
                (princ (format "It is local to %S" locus))))
	      (if (not (default-boundp variable))
		  (princ "globally void")
		(let ((global-val (default-value variable)))
		  (with-current-buffer standard-output
		    (princ "global value is ")
		    (if (eq val global-val)
			(princ "the same.")
		      (terpri)
		      ;; Fixme: pp can take an age if you happen to
		      ;; ask for a very large expression.  We should
		      ;; probably print it raw once and check it's a
		      ;; sensible size before prettyprinting.  -- fx
		      (let ((from (point)))
                        (cl-prin1 global-val)
                        (save-restriction
                          (narrow-to-region from (point))
                          (save-excursion (pp-buffer)))
			;; See previous comment for this function.
			;; (help-xref-on-pp from (point))
			(if (< (point) (+ from 20))
			    (delete-region (1- from) from))
                        (help-fns--editable-variable
                         from (point) variable global-val buffer))))))
              (terpri))

	    ;; If the value is large, move it to the end.
	    (with-current-buffer standard-output
	      (when (> (count-lines (point-min) (point-max)) 10)
		(goto-char val-start-pos)
		(when (looking-at "value is") (replace-match ""))
		(save-excursion
		  (insert "\n\nValue:")
                  (setq-local help-button-cache (point-marker)))
		(insert "value is shown ")
		(insert-button "below"
			       'action help-button-cache
			       'follow-link t
			       'help-echo "mouse-2, RET: show value")
		(insert ".\n")))
            (terpri)

            (let* ((alias (condition-case nil
                              (indirect-variable variable)
                            (error variable)))
                   (doc (or (documentation-property
                             variable 'variable-documentation)
                            (documentation-property
                             alias 'variable-documentation))))

	      (with-current-buffer standard-output
                (help-fns--var-obsolete variable)
		(insert (or doc "Not documented as a variable.")))

              ;; Output the indented administrative bits.
              (with-current-buffer buffer
                (help-fns--run-describe-functions
                 help-fns-describe-variable-functions variable))

              (with-current-buffer standard-output
                ;; If we have the long value of the variable at the
                ;; end, remove superfluous empty lines before it.
                (unless (eobp)
                  (while (looking-at-p "\n")
                    (delete-char 1)))))

	    (with-current-buffer standard-output
              (help-fns--setup-xref-backend)
	      ;; Return the text we displayed.
	      (buffer-string))))))))

(defun help-fns--editable-variable (start end variable value buffer)
  (when (and (readablep value)
             help-enable-variable-value-editing)
    (add-text-properties
     start end
     (list 'help-echo "`e' to edit the value"
           'help-fns--edit-variable (list variable value buffer
                                          (current-buffer))
           'keymap (define-keymap
                     :parent button-map
                     "e" #'help-fns-edit-variable)))))

(put 'help-fns-edit-variable 'disabled t)
(defun help-fns-edit-variable ()
  "Edit the variable value at point in \"*Help*\" buffer.
This command only works if `help-enable-variable-value-editing' is non-nil.

To edit the value of a variable, use \\[describe-variable] followed by the name
of a variable, to display a \"*Help*\" buffer, move point to
the variable's value, usually after the text \"Its value is\", and
type \\`e' to invoke this command.

Values that aren't readable by the Emacs Lisp reader can't be edited
by this command."
  (declare (completion ignore))
  (interactive)
  (let ((var (get-text-property (point) 'help-fns--edit-variable)))
    (unless var
      (error "No variable under point"))
    (string-edit
     (format ";; Edit the `%s' variable." (nth 0 var))
     (prin1-to-string (nth 1 var))
     (lambda (edited)
       (set (nth 0 var) edited)
       (exit-recursive-edit))
     :abort-callback
     (lambda ()
       (exit-recursive-edit)
       (error "Aborted edit, variable unchanged"))
     :major-mode-sym #'emacs-lisp-mode
     :read #'read)
    (recursive-edit)
    (revert-buffer)))

(autoload 'shortdoc-help-fns-examples-function "shortdoc")

(defun help-fns--run-describe-functions (functions &rest args)
  (with-current-buffer standard-output
    (unless (bolp)
      (insert "\n"))
    (help-fns--ensure-empty-line))
  (let ((help-fns--activated-functions nil))
    (dolist (func functions)
      (let ((size (buffer-size standard-output)))
        (apply func args)
        ;; This function inserted something, so register it.
        (when (> (buffer-size standard-output) size)
          (push func help-fns--activated-functions)))))
  (with-current-buffer standard-output
    (help-fns--ensure-empty-line)))

(add-hook 'help-fns-describe-variable-functions #'help-fns--customize-variable)
(defun help-fns--customize-variable (variable &optional text)
  ;; Make a link to customize if this variable can be customized.
  (when (custom-variable-p variable)
    (let ((customize-label "customize")
          (custom-set (get variable 'custom-set))
          (opoint (with-current-buffer standard-output
                    (point))))
      (princ (concat "  You can " customize-label (or text " this variable.")))
      (when (and custom-set
                 ;; Don't override manually written documentation.
                 (not (string-match (rx word-start "setopt" word-end)
                                    (documentation-property
                                     variable 'variable-documentation))))
        (princ (substitute-quotes
                (concat "\n  Setting this variable directly with `setq' may not take effect;"
                        "\n  use either customize or `setopt'"
                        ;; Skip text if `custom-set' property is an
                        ;; anonymous function.
                        (when (symbolp custom-set)
                          (concat ", or call `" (symbol-name custom-set) "'"))
                        "."))))
      (with-current-buffer standard-output
	(save-excursion
          (while (re-search-backward (concat "\\(" customize-label "\\)") opoint t)
	    (help-xref-button 1 'help-customize-variable variable))))
      (terpri))))

(add-hook 'help-fns-describe-variable-functions
          #'help-fns--customize-variable-version)
(defun help-fns--customize-variable-version (variable)
  (when (custom-variable-p variable)
    ;; Note variable's version or package version.
    (when-let* ((output (describe-variable-custom-version-info variable)))
      (princ output))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-safe-local)
(defun help-fns--var-safe-local (variable)
  (let ((safe-var (get variable 'safe-local-variable)))
    (when safe-var
      (princ "  This variable is safe as a file local variable ")
      (princ "if its value\n  satisfies the predicate ")
      (princ (if (compiled-function-p safe-var)
		 "which is a compiled expression.\n"
	       (format-message "`%s'.\n" safe-var))))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-risky)
(defun help-fns--var-risky (variable)
  ;; Can be both risky and safe, eg auto-fill-function.
  (when (risky-local-variable-p variable)
    (princ "  This variable may be risky if used as a \
file-local variable.\n")
    (when (assq variable safe-local-variable-values)
      (princ (substitute-quotes
              "  However, you have added it to \
`safe-local-variable-values'.\n")))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-ignored-local)
(defun help-fns--var-ignored-local (variable)
  (when (memq variable ignored-local-variables)
    (princ "  This variable is ignored as a file-local \
variable.\n")))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-file-local)
(defun help-fns--var-file-local (variable)
  (when (boundp variable)
    (let ((val (symbol-value variable)))
      (when (member (cons variable val)
                    file-local-variables-alist)
        (if (member (cons variable val)
                    dir-local-variables-alist)
	    (let ((file (and buffer-file-name
                             (not (file-remote-p buffer-file-name))
                             (dir-locals-find-file buffer-file-name)))
                  (is-directory nil))
	      (princ (substitute-command-keys
		      "  This variable's value is directory-local"))
              (when (consp file)       ; result from cache
                ;; If the cache element has an mtime, we
                ;; assume it came from a file.
                (if (nth 2 file)
                    ;; (car file) is a directory.
                    (setq file (dir-locals--all-files (car file)))
                  ;; Otherwise, assume it was set directly.
                  (setq file (car file)
                        is-directory t)))
              (if (null file)
                  (princ ".\n")
                (princ ", set ")
                (princ (substitute-command-keys
                        (cond
                         (is-directory "for the directory\n  `")
                         ;; Many files matched.
                         ((and (consp file) (cdr file))
                          (setq file (file-name-directory (car file)))
                          (format "by one of the\n  %s files in the directory\n  `"
                                  dir-locals-file))
                         (t (setq file (car file))
                            "by the file\n  `"))))
	        (with-current-buffer standard-output
	          (insert-text-button
	           file 'type 'help-dir-local-var-def
                   'help-args (list variable file)))
                (princ (substitute-quotes "'.\n"))))
          (princ (substitute-quotes
	          "  This variable's value is file-local.\n")))))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-watchpoints)
(defun help-fns--var-watchpoints (variable)
  (let ((watchpoints (get-variable-watchers variable)))
    (when watchpoints
      (princ "  Calls these functions when changed: ")
      ;; FIXME: Turn function names into hyperlinks.
      (princ watchpoints)
      (terpri))))

(defun help-fns--var-obsolete (variable)
  (let* ((obsolete (get variable 'byte-obsolete-variable))
	 (use (car obsolete))
         (start (point)))
    (when obsolete
      (insert "This variable is obsolete")
      (if (nth 2 obsolete)
          (insert (format " since %s" (nth 2 obsolete))))
      (insert (cond ((stringp use) (substitute-command-keys (concat "; " use)))
		    (use (format-message "; use `%s' instead."
                                         (car obsolete)))
		    (t "."))
              "\n")
      (fill-region-as-paragraph start (point))
      (ensure-empty-lines))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-alias)
(defun help-fns--var-alias (variable)
  ;; Mention if it's an alias.
  (let ((alias (condition-case nil
                   (indirect-variable variable)
                 (error variable))))
    (unless (eq alias variable)
      (princ (format-message
              "  This variable is an alias for `%s'.\n"
              alias)))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-aliases)
(defun help-fns--var-aliases (variable)
  ;; Mention if it has any aliases.
  (let (aliases alias)
    (mapatoms
     (lambda (sym)
       (when (and (boundp sym)
		  (setq alias (indirect-variable sym))
                  (eq alias variable)
		  (not (eq alias sym)))
	 (push sym aliases)))
     obarray)
    (when aliases
      (princ
       (if (= (length aliases) 1)
           (format-message
            "  This variable has an alias: `%s'.\n" (car aliases))
         (format-message
          "  This variable has the following aliases: %s.\n"
          (mapconcat
           (lambda (sym)
             (format "`%s'" sym))
           aliases ",\n    ")))))))

(add-hook 'help-fns-describe-variable-functions #'help-fns--var-bufferlocal)
(defun help-fns--var-bufferlocal (variable)
  (let ((permanent-local (get variable 'permanent-local))
        (locus (variable-binding-locus variable)))
    ;; Mention if it's a local variable.
    (cond
     ((and (local-variable-if-set-p variable)
	   (or (not (local-variable-p variable))
	       (with-temp-buffer
	         (local-variable-if-set-p variable))))
      (princ "  Automatically becomes ")
      (if permanent-local
	  (princ "permanently "))
      (princ "buffer-local when set.\n"))
     ((not permanent-local))
     ((bufferp locus)
      (princ
       (substitute-quotes
        "  This variable's buffer-local value is permanent.\n")))
     (t
      (princ (substitute-quotes
	      "  This variable's value is permanent \
if it is given a local binding.\n"))))))


;; Faces.

;;;###autoload
(defun describe-face (face &optional frame)
  "Display the properties of face FACE on FRAME.
Interactively, FACE defaults to the faces of the character after point
and FRAME defaults to the selected frame.

If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (interactive (list (read-face-name "Describe face"
                                     (or (face-at-point t) 'default)
                                     t)))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'describe-face face)
		     (called-interactively-p 'interactive))
    (unless face
      (setq face 'default))
    (setq face (ensure-list face))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (dolist (f face (buffer-string))
	  (if (stringp f) (setq f (intern f)))
	  ;; We may get called for anonymous faces (i.e., faces
	  ;; expressed using prop-value plists).  Those can't be
	  ;; usefully customized, so ignore them.
	  (when (symbolp f)
	    (insert "Face: " (symbol-name f))
	    (if (not (facep f))
	        (insert "   undefined face.\n")
	      (let ((customize-label "customize this face")
		    file-name)
	        (insert (concat " (" (propertize "sample" 'font-lock-face f) ")"))
	        (princ (concat " (" customize-label ")\n"))
	        ;; FIXME not sure how much of this belongs here, and
	        ;; how much in `face-documentation'.  The latter is
	        ;; not used much, but needs to return nil for
	        ;; undocumented faces.
	        (let ((alias (get f 'face-alias))
		      (face f)
		      obsolete)
		  (when alias
		    (setq face alias)
		    (insert
		     (format-message
                      "\n  %s is an alias for the face `%s'.\n%s"
                      f alias
                      (if (setq obsolete (get f 'obsolete-face))
                          (format-message
                           "  This face is obsolete%s; use `%s' instead.\n"
                           (if (stringp obsolete)
                               (format " since %s" obsolete)
                             "")
                           alias)
                        ""))))
		  (insert "\nDocumentation:\n"
                          (or (face-documentation face)
                              "Not documented as a face.")
			  "\n\n"))
	        (with-current-buffer standard-output
		  (save-excursion
                    (re-search-backward (concat "\\(" customize-label "\\)"))
		    (help-xref-button 1 'help-customize-face f)))
	        (setq file-name (find-lisp-object-file-name f 'defface))
	        (if (not file-name)
                    (setq help-mode--current-data (list :symbol f))
                  (setq help-mode--current-data (list :symbol f
                                                      :file file-name))
                (princ (substitute-quotes "Defined in `"))
		  (princ (help-fns-short-filename file-name))
                (princ (substitute-quotes "'"))
		  ;; Make a hyperlink to the library.
		  (save-excursion
		    (re-search-backward
                     (substitute-command-keys "`\\([^`']+\\)'"))
		    (help-xref-button 1 'help-face-def f file-name))
		  (princ ".")
		  (terpri)
		  (terpri))))
	    (terpri)
            (help-fns--run-describe-functions
             help-fns-describe-face-functions f frame)))))))

(add-hook 'help-fns-describe-face-functions
          #'help-fns--face-custom-version-info)
(defun help-fns--face-custom-version-info (face _frame)
  (when-let* ((version-info (describe-variable-custom-version-info face 'face)))
    (insert version-info)
    (terpri)))

(add-hook 'help-fns-describe-face-functions #'help-fns--face-attributes)
(defun help-fns--face-attributes (face frame)
  (let* ((attrs '((:family . "Family")
		  (:foundry . "Foundry")
		  (:width . "Width")
		  (:height . "Height")
		  (:weight . "Weight")
		  (:slant . "Slant")
		  (:foreground . "Foreground")
		  (:distant-foreground . "DistantForeground")
		  (:background . "Background")
		  (:underline . "Underline")
		  (:overline . "Overline")
		  (:strike-through . "Strike-through")
		  (:box . "Box")
		  (:inverse-video . "Inverse")
		  (:stipple . "Stipple")
		  (:font . "Font")
		  (:fontset . "Fontset")
                  (:extend . "Extend")
		  (:inherit . "Inherit")))
         (max-width (apply #'max (mapcar (lambda (x) (length (cdr x)))
					 attrs))))
    (dolist (a attrs)
      (let ((attr (face-attribute face (car a) frame)))
	(insert (make-string (- max-width (length (cdr a))) ?\s)
		(cdr a) ": " (format "%s" attr))
	(if (and (eq (car a) :inherit)
		 (not (eq attr 'unspecified)))
	    ;; Make a hyperlink to the parent face.
	    (save-excursion
              (re-search-backward ": \\([^:]+\\)")
	      (help-xref-button 1 'help-face attr)))
	(insert "\n")))
    (terpri)))

(defvar help-xref-stack-item)

;;;###autoload
(defun describe-symbol (symbol &optional buffer frame)
  "Display the full documentation of SYMBOL.
Will show the info of SYMBOL as a function, variable, and/or face.
Optional arguments BUFFER and FRAME specify for which buffer and
frame to show the information about SYMBOL; they default to the
current buffer and the selected frame, respectively."
  (interactive
   (let* ((v-or-f (symbol-at-point))
          (found (if v-or-f (cl-some (lambda (x) (funcall (nth 1 x) v-or-f))
                                     describe-symbol-backends)))
          (v-or-f (if found v-or-f (function-called-at-point)))
          (found (or found v-or-f))
          (enable-recursive-minibuffers t)
          (val (completing-read (format-prompt "Describe symbol"
                                               (and found v-or-f))
				#'help--symbol-completion-table
				(lambda (vv)
                                  (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                           describe-symbol-backends))
				t nil nil
				(if found (symbol-name v-or-f)))))
     (list (if (equal val "")
	       (or v-or-f "") (intern val)))))
  (let ((help-buffer-under-preparation t))
    (if (not (symbolp symbol))
        (user-error "You didn't specify a function or variable"))
    (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
    (unless (frame-live-p frame) (setq frame (selected-frame)))
    (with-current-buffer (help-buffer)
      ;; Push the previous item on the stack before clobbering the output buffer.
      (help-setup-xref nil nil)
      (let* ((docs
              (nreverse
               (delq nil
                     (mapcar (pcase-lambda (`(,name ,testfn ,descfn))
                               (when (funcall testfn symbol)
                                 ;; Don't record the current entry in the stack.
                                 (setq help-xref-stack-item nil)
                                 (let ((help-xref-stack nil)
                                       (help-xref-forward-stack nil))
                                   (funcall descfn symbol buffer frame))
                                 (cons name (buffer-string))))
                             describe-symbol-backends))))
             (single (null (cdr docs))))
        (when (null docs)               ; Don't silently do nothing.
          (user-error "Unknown symbol: %S" symbol))
        (while (cdr docs)
          (goto-char (point-min))
          (let ((inhibit-read-only t)
                (name (caar docs))        ;Name of doc currently at BOB.
                (doc (cdr (cadr docs))))  ;Doc to add at BOB.
            (when doc
              (insert doc)
              (delete-region (point)
                             (progn (skip-chars-backward " \t\n") (point)))
              (insert "\n\n" (make-separator-line) "\n")
              (when name
                (insert (symbol-name symbol)
                        " is also a " name "." "\n\n"))))
          (setq docs (cdr docs)))
        (unless single
          ;; Don't record the `describe-variable' item in the stack.
          (setq help-xref-stack-item nil)
          (let ((help-mode--current-data help-mode--current-data))
            (help-setup-xref (list #'describe-symbol symbol) nil)))
        (goto-char (point-max))
        (help-xref--navigation-buttons)
        (goto-char (point-min))))))

;;;###autoload
(defun describe-syntax (&optional buffer)
  "Describe the syntax specifications in the syntax table of BUFFER.
The descriptions are inserted in a help buffer, which is then displayed.
BUFFER defaults to the current buffer."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'describe-syntax buffer)
		     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (let ((table (with-current-buffer buffer (syntax-table))))
        (with-current-buffer standard-output
	  (describe-vector table 'internal-describe-syntax-value)
	  (while (setq table (char-table-parent table))
	    (insert "\nThe parent syntax table is:")
	    (describe-vector table 'internal-describe-syntax-value)))))))

(defun help-describe-category-set (value)
  (insert (cond
	   ((null value) "default")
	   ((char-table-p value) "deeper char-table ...")
	   (t (condition-case nil
		  (category-set-mnemonics value)
                (error "Invalid"))))))

;;;###autoload
(defun describe-categories (&optional buffer)
  "Describe the category specifications in the current category table.
The descriptions are inserted in a buffer, which is then displayed.
If BUFFER is non-nil, then describe BUFFER's category table instead.
BUFFER should be a buffer or a buffer name."
  (interactive)
  (let ((help-buffer-under-preparation t))
    (setq buffer (or buffer (current-buffer)))
    (help-setup-xref (list #'describe-categories buffer)
		     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (let* ((table (with-current-buffer buffer (category-table)))
	     (docs (char-table-extra-slot table 0)))
        (if (or (not (vectorp docs)) (/= (length docs) 95))
	    (error "Invalid first extra slot in this category table\n"))
        (with-current-buffer standard-output
          (setq-default help-button-cache (make-marker))
	  (insert "Legend of category mnemonics ")
          (insert-button "(longer descriptions at the bottom)"
                         'action help-button-cache
                         'follow-link t
                         'help-echo "mouse-2, RET: show full legend")
          (insert "\n")
	  (let ((pos (point)) (items 0) lines n)
	    (dotimes (i 95)
	      (if (aref docs i) (setq items (1+ items))))
	    (setq lines (1+ (/ (1- items) 4)))
	    (setq n 0)
	    (dotimes (i 95)
	      (let ((elt (aref docs i)))
	        (when elt
		  (string-match ".*" elt)
		  (setq elt (match-string 0 elt))
		  (if (>= (length elt) 17)
		      (setq elt (concat (substring elt 0 14) "...")))
		  (if (< (point) (point-max))
		      (move-to-column (* 20 (/ n lines)) t))
		  (insert (+ i ?\s) ?: elt)
		  (if (< (point) (point-max))
		      (forward-line 1)
		    (insert "\n"))
		  (setq n (1+ n))
		  (if (= (% n lines) 0)
		      (goto-char pos))))))
	  (goto-char (point-max))
	  (insert "\n"
		  "character(s)\tcategory mnemonics\n"
		  "------------\t------------------")
	  (describe-vector table 'help-describe-category-set)
          (set-marker help-button-cache (point))
	  (insert "Legend of category mnemonics:\n")
	  (dotimes (i 95)
	    (let ((elt (aref docs i)))
	      (when elt
	        (if (string-match "\n" elt)
		    (setq elt (substring elt (match-end 0))))
	        (insert (+ i ?\s) ": " elt "\n"))))
	  (while (setq table (char-table-parent table))
	    (insert "\nThe parent category table is:")
	    (describe-vector table 'help-describe-category-set)))))))

(defun help-fns-find-keymap-name (keymap)
  "Find the name of the variable with value KEYMAP.
Return nil if KEYMAP is not a valid keymap, or if there is no
variable with value KEYMAP."
  (when (keymapp keymap)
    (let ((name (catch 'found-keymap
                  (mapatoms (lambda (symb)
                              (when (and (boundp symb)
                                         (eq (symbol-value symb) keymap)
                                         (not (eq symb 'keymap)))
                                (throw 'found-keymap symb))))
                  nil)))
      ;; Follow aliasing.
      (or (ignore-errors (indirect-variable name)) name))))

(defun help-fns--most-relevant-active-keymap ()
  "Return the name of the most relevant active keymap.
The heuristic to determine which keymap is most likely to be
relevant to a user follows this order:

1. `keymap' text property at point
2. `local-map' text property at point
3. the `current-local-map'

This is used to set the default value for the interactive prompt
in `describe-keymap'.  See also `Searching the Active Keymaps'."
  (help-fns-find-keymap-name (or (get-char-property (point) 'keymap)
                         (if (get-text-property (point) 'local-map)
                             (get-char-property (point) 'local-map)
                           (current-local-map)))))

(defvar keymap-name-history nil
  "History for input to `describe-keymap'.")

;;;###autoload
(defun describe-keymap (keymap)
  "Describe key bindings in KEYMAP.
When called interactively, prompt for a variable that has a
keymap value."
  (interactive
   (let* ((sym (symbol-at-point))
          (km (or (and (keymapp (ignore-errors (symbol-value sym)))
                       sym)
                  (help-fns--most-relevant-active-keymap)))
          (val (completing-read
                (format-prompt "Keymap" km)
                obarray
                (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
                t nil 'keymap-name-history
                (symbol-name km))))
     (unless (equal val "")
       (setq km (intern val)))
     (unless (and km (keymapp (symbol-value km)))
       (user-error "Not a keymap: %s" km))
     (list km)))
  (let (used-gentemp
        (help-buffer-under-preparation t))
    (unless (and (symbolp keymap)
                 (boundp keymap)
                 (keymapp (symbol-value keymap)))
      (when (not (keymapp keymap))
        (if (symbolp keymap)
            (error "Not a keymap variable: %S" keymap)
          (error "Not a keymap")))
      (let ((sym (cl-gentemp "KEYMAP OBJECT (no variable) ")))
        (setq used-gentemp t)
        (set sym keymap)
        (setq keymap sym)))
    ;; Follow aliasing.
    (setq keymap (or (ignore-errors (indirect-variable keymap)) keymap))
    (help-setup-xref (list #'describe-keymap keymap)
                     (called-interactively-p 'interactive))
    (let* ((name (symbol-name keymap))
           (doc (documentation-property keymap 'variable-documentation))
           (file-name (find-lisp-object-file-name keymap 'defvar)))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (unless used-gentemp
            (princ (format-message "%S is a keymap variable" keymap))
            (if (not file-name)
                (progn
                  (setq help-mode--current-data (list :symbol keymap))
                  (princ ".\n\n"))
              (princ (format-message
                      " defined in `%s'.\n\n"
                      (if (eq file-name 'C-source)
                          "C source code"
                        (help-fns-short-filename file-name))))
              (save-excursion
                (re-search-backward (substitute-command-keys "`\\([^`']+\\)'"))
                (setq help-mode--current-data (list :symbol keymap
                                                    :file file-name))
                (help-xref-button 1 'help-variable-def
                                  keymap file-name))))
          (when (and (not (equal "" doc)) doc)
            (princ "Documentation:\n")
            (princ (format-message "%s\n\n" doc)))
          ;; Use `insert' instead of `princ', so control chars (e.g. \377)
          ;; insert correctly.
          (insert (substitute-command-keys (concat "\\{" name "}"))))))
    ;; Cleanup.
    (when used-gentemp
      (makunbound keymap))))

(defcustom describe-mode-outline t
  "Non-nil enables outlines in the output buffer of `describe-mode'."
  :type 'boolean
  :group 'help
  :version "30.1")

;;;###autoload
(defun describe-mode (&optional buffer)
  "Display documentation of current major mode and minor modes.
A brief summary of the minor modes comes first, followed by the
major mode description.  This is followed by detailed
descriptions of the minor modes, each on a separate page.

For this to work correctly for a minor mode, the mode's indicator
variable \(listed in `minor-mode-alist') must also be a function
whose documentation describes the minor mode.

If called from Lisp with a non-nil BUFFER argument, display
documentation for the major and minor modes of that buffer.

When `describe-mode-outline' is non-nil, Outline minor mode
is enabled in the Help buffer."
  (interactive "@")
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((help-buffer-under-preparation t)
        (local-minors (buffer-local-value 'local-minor-modes buffer)))
    (help-setup-xref (list #'describe-mode buffer)
		     (called-interactively-p 'interactive))
    ;; For the sake of help-do-xref and help-xref-go-back,
    ;; don't switch buffers before calling `help-buffer'.
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        ;; Add the local minor modes at the start.
        (when local-minors
          (unless describe-mode-outline
            (insert (format "Minor mode%s enabled in this buffer:"
                            (if (length> local-minors 1)
                                "s" ""))))
          (describe-mode--minor-modes local-minors))

        ;; Document the major mode.
        (let ((major (buffer-local-value 'major-mode buffer)))
          (when describe-mode-outline
            (goto-char (point-min))
            (put-text-property
             (point) (progn (insert (format "Major mode %S" major)) (point))
             'outline-level 1)
            (insert "\n\n"))
          (insert "The major mode is "
                  (buttonize
                   (propertize (format-mode-line
                                (buffer-local-value 'mode-name buffer)
                                nil nil buffer)
                               'face 'bold)
                   (lambda (_)
                     (describe-function major))))
          (insert " mode")
          (when-let* ((file-name (find-lisp-object-file-name major nil)))
	    (insert (format " defined in %s"
                            (buttonize
                             (help-fns-short-filename file-name)
                             (lambda (_)
                               (help-function-def--button-function
                                major file-name))))))
          (insert ":\n\n"
                  (help-split-fundoc (documentation major) nil 'doc)
                  (with-current-buffer buffer
                    (help-fns--list-local-commands)))
          (ensure-empty-lines 1)

          ;; Insert the global minor modes after the major mode.
          (when global-minor-modes
            (unless describe-mode-outline
              (insert (format "Global minor mode%s enabled:"
                              (if (length> global-minor-modes 1)
                                  "s" ""))))
            (describe-mode--minor-modes global-minor-modes t)
            (unless describe-mode-outline
              (when (re-search-forward "^\f")
                (beginning-of-line)
                (ensure-empty-lines 1))))

          (when describe-mode-outline
            (setq-local outline-search-function #'outline-search-level)
            (setq-local outline-level (lambda () 1))
            (setq-local outline-minor-mode-cycle t
                        outline-minor-mode-highlight t
                        outline-minor-mode-use-buttons 'insert)
            (outline-minor-mode 1))

          ;; For the sake of IELM and maybe others
          nil)))))

(defun describe-mode--minor-modes (modes &optional global)
  (dolist (mode (seq-sort #'string< modes))
    (let ((pretty-minor-mode
           (capitalize
            (replace-regexp-in-string
             "\\(\\(-minor\\)?-mode\\)?\\'" ""
             (symbol-name mode)))))
      (if (not describe-mode-outline)
          (insert
           " "
           (buttonize
            pretty-minor-mode
            (lambda (mode)
              (goto-char (point-min))
              (text-property-search-forward
               'help-minor-mode mode t)
              (beginning-of-line))
            mode))
        (goto-char (point-max))
        (put-text-property
         (point) (progn (insert (if global "Global" "Local")
                                (format " minor mode %S" mode))
                        (point))
         'outline-level 1)
        (insert "\n\n"))
      (save-excursion
	(unless describe-mode-outline
          (goto-char (point-max))
	  (insert "\n\n\f\n"))
	;; Document the minor modes fully.
        (insert (buttonize
                 (propertize pretty-minor-mode 'help-minor-mode mode)
                 #'describe-function
                 mode))
        (let ((indicator
               (format-mode-line (assq mode minor-mode-alist))))
	  (insert (format " minor mode (%s):\n"
			  (if (zerop (length indicator))
			      "no indicator"
			    (format "indicator%s"
				    indicator)))))
	(insert (or (help-split-fundoc (documentation mode) nil 'doc)
	            "No docstring"))
        (when describe-mode-outline
          (insert "\n\n")))))
  (unless describe-mode-outline
    (forward-line -1)
    (fill-paragraph nil)
    (forward-paragraph 1)
    (ensure-empty-lines 1)))

(defun help-fns--list-local-commands ()
  (let ((functions nil))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  ;; Ignore aliases.
                  (not (symbolp (symbol-function sym)))
                  ;; Ignore obsolete commands.
                  (not (get sym 'byte-obsolete-info))
                  ;; Ignore everything bound.
                  (not (where-is-internal sym nil t))
                  (derived-mode-p (command-modes sym)))
         (push sym functions))))
    (with-temp-buffer
      (when functions
        (setq functions (sort functions #'string<))
        (insert "\n\nOther commands for this mode, not bound to any keys:\n\n")
        (dolist (function functions)
          (insert (format "`%s'\n" function))))
      (buffer-string))))


;; Widgets.

(defvar describe-widget-functions
  '(button-describe widget-describe)
  "A list of functions for `describe-widget' to call.
Each function should take one argument, a buffer position, and return
non-nil if it described a widget at that position.")

;;;###autoload
(defun describe-widget (&optional pos)
  "Display a buffer with information about a widget.
You can use this command to describe buttons (e.g., the links in a *Help*
buffer), editable fields of the customization buffers, etc.

Interactively, click on a widget to describe it, or hit RET to describe the
widget at point.

When called from Lisp, POS may be a buffer position or a mouse position list.

Calls each function of the list `describe-widget-functions' in turn, until
one of them returns non-nil."
  (interactive
   (list
    (let ((key
           (read-key
            "Click on a widget, or hit RET to describe the widget at point")))
      (cond ((eq key ?\C-m) (point))
            ((and (mouse-event-p key)
                  (eq (event-basic-type key) 'mouse-1)
                  (equal (event-modifiers key) '(click)))
             (event-end key))
            ((eq key ?\C-g) (signal 'quit nil))
            (t (user-error "You didn't specify a widget"))))))
  (let (buf
        (help-buffer-under-preparation t))
    ;; Allow describing a widget in a different window.
    (when (posnp pos)
      (setq buf (window-buffer (posn-window pos))
            pos (posn-point pos)))
    (with-current-buffer (or buf (current-buffer))
      (unless (cl-some (lambda (fun) (when (fboundp fun) (funcall fun pos)))
                       describe-widget-functions)
        (message "No widget found at that position")))))


;;; Replacements for old lib-src/ programs.  Don't seem especially useful.

;; Replaces lib-src/digest-doc.c.
;;;###autoload
(defun doc-file-to-man (file)
  "Produce an nroff buffer containing the doc-strings from the DOC file."
  (interactive (list (read-file-name "Name of DOC file: " doc-directory
                                     internal-doc-file-name t)))
  (or (file-readable-p file)
      (error "Cannot read file `%s'" file))
  (pop-to-buffer (generate-new-buffer "*man-doc*"))
  (setq buffer-undo-list t)
  (insert ".TH \"Command Summary for GNU Emacs\"\n"
          ".AU Richard M. Stallman\n")
  (insert-file-contents file)
  (let (notfirst)
    (while (search-forward "\^_" nil 'move)
      (if (= (following-char) ?S)
          (delete-region (1- (point)) (line-end-position))
        (delete-char -1)
        (if notfirst
            (insert "\n.DE\n")
          (setq notfirst t))
        (insert "\n.SH ")
        (insert (if (= (following-char) ?F) "Function " "Variable "))
        (delete-char 1)
        (forward-line 1)
        (insert ".DS L\n"))))
  (insert "\n.DE\n")
  (setq buffer-undo-list nil)
  (nroff-mode))

;; Replaces lib-src/sorted-doc.c.
;;;###autoload
(defun doc-file-to-info (file)
  "Produce a texinfo buffer with sorted doc-strings from the DOC file."
  (interactive (list (read-file-name "Name of DOC file: " doc-directory
                                     internal-doc-file-name t)))
  (or (file-readable-p file)
      (error "Cannot read file `%s'" file))
  (let ((i 0) type name doc alist)
    (with-temp-buffer
      (insert-file-contents file)
      ;; The characters "@{}" need special treatment.
      (while (re-search-forward "[@{}]" nil t)
        (backward-char)
        (insert "@")
        (forward-char 1))
      (goto-char (point-min))
      (while (search-forward "\^_" nil t)
        (when (/= (following-char) ?S)
          (setq type (char-after)
                name (buffer-substring (1+ (point)) (line-end-position))
                doc (buffer-substring (line-beginning-position 2)
                                      (if (search-forward  "\^_" nil 'move)
                                          (1- (point))
                                        (point)))
                alist (cons (list name type doc) alist))
          (backward-char 1))))
    (pop-to-buffer (generate-new-buffer "*info-doc*"))
    (setq buffer-undo-list t)
    ;; Write the output header.
    (insert "\\input texinfo  @c -*-texinfo-*-\n"
            "@setfilename emacsdoc.info\n"
            "@settitle Command Summary for GNU Emacs\n"
            "@finalout\n"
            "\n@node Top\n"
            "@unnumbered Command Summary for GNU Emacs\n\n"
            "@table @asis\n\n"
            "@iftex\n"
            "@global@let@ITEM@item\n"
            "@def@item{@filbreak@vskip5pt@ITEM}\n"
            "@font@tensy cmsy10 scaled @magstephalf\n"
            "@font@teni cmmi10 scaled @magstephalf\n"
            "@def\\{{@tensy@char110}}\n" ; this backslash goes with cmr10
            "@def|{{@tensy@char106}}\n"
            "@def@{{{@tensy@char102}}\n"
            "@def@}{{@tensy@char103}}\n"
            "@def<{{@teni@char62}}\n"
            "@def>{{@teni@char60}}\n"
            "@chardef@@64\n"
            "@catcode43=12\n"
            "@tableindent-0.2in\n"
            "@end iftex\n")
    ;; Sort the array by name; within each name, by type (functions first).
    (setq alist (sort alist (lambda (e1 e2)
                              (if (string-equal (car e1) (car e2))
                                  (<= (cadr e1) (cadr e2))
                                (string-lessp (car e1) (car e2))))))
    ;; Print each function.
    (dolist (e alist)
      (insert "\n@item "
              (if (char-equal (cadr e) ?\F) "Function" "Variable")
              " @code{" (car e) "}\n@display\n"
              (nth 2 e)
              "\n@end display\n")
      ;; Try to avoid a save size overflow in the TeX output routine.
      (if (zerop (setq i (% (1+ i) 100)))
          (insert "\n@end table\n@table @asis\n")))
    (insert "@end table\n"
            "@bye\n")
    (setq buffer-undo-list nil)
    (texinfo-mode)))

(defconst help-fns--function-numbers
  (make-hash-table :test 'equal :weakness 'value))
(defconst help-fns--function-names (make-hash-table :weakness 'key))

(defun help-fns--display-function (function)
  (cond
   ((subr-primitive-p function)
    (describe-function function))
   ((and (compiled-function-p function)
         (not (and (fboundp 'kmacro-p) (kmacro-p function))))
    (disassemble function))
   (t
    ;; FIXME: Use cl-print!
    (pp-display-expression function "*Help Source*" (consp function)))))

;;;###autoload
(defun help-fns-function-name (function)
  "Return a short buttonized string representing FUNCTION.
The string is propertized with a button; clicking on that
provides further details about FUNCTION.
FUNCTION can be a function, a built-in, a keyboard macro,
or a compile function.
This function is intended to be used to display various
callable symbols in buffers in a way that allows the user
to find out more details about the symbols."
  ;; FIXME: For kmacros, should we print the key-sequence?
  (cond
   ((symbolp function)
    (let ((name (if (eq (intern-soft (symbol-name function)) function)
                    (symbol-name function)
                  (concat "#:" (symbol-name function)))))
      (if (not (fboundp function))
          name
        (make-text-button name nil
                          'type 'help-function
                          'help-args (list function)))))
   ((gethash function help-fns--function-names))
   ((subrp function)
    (let ((name (subr-name function)))
      ;; FIXME: For native-elisp-functions, should we use `help-function'
      ;; or `disassemble'?
      (format "#<%s %s>"
              (cl-type-of function)
              (make-text-button name nil
                                'type 'help-function
                                ;; Let's hope the subr hasn't been redefined!
                                'help-args (list (intern name))))))
   (t
    (let ((type (or (oclosure-type function)
                    (if (consp function)
                        (car function) (cl-type-of function))))
          (hash (sxhash-eq function))
          ;; Use 3 digits minimum.
          (mask #xfff)
          name)
      (while
          (let* ((hex (format (concat "%0"
                                      (number-to-string (1+ (/ (logb mask) 4)))
                                      "X")
                              (logand mask hash)))
                 ;; FIXME: For kmacros, we don't want to `disassemble'!
                 (button (buttonize
                          hex #'help-fns--display-function function
                          ;; FIXME: Shouldn't `buttonize' add
                          ;; the "mouse-2, RET:" prefix?
                          "mouse-2, RET: Display the function's body")))
            (setq name (format "#<%s %s>" type button))
            (and (< mask (abs hash))    ; We can add more digits.
                 (gethash name help-fns--function-numbers)))
        ;; Add a digit.
        (setq mask (+ (ash mask 4) #x0f)))
      (puthash name function help-fns--function-numbers)
      (puthash function name help-fns--function-names)
      name))))

(provide 'help-fns)

;;; help-fns.el ends here
