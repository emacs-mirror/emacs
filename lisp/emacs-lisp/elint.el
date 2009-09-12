;;; elint.el --- Lint Emacs Lisp

;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009  Free Software Foundation, Inc.

;; Author: Peter Liljenberg <petli@lysator.liu.se>
;; Created: May 1997
;; Keywords: lisp

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

;; This is a linter for Emacs Lisp.  Currently, it mainly catches
;; misspellings and undefined variables, although it can also catch
;; function calls with the wrong number of arguments.

;; To use, call elint-current-buffer or elint-defun to lint a buffer
;; or defun.  The first call runs `elint-initialize' to set up some
;; argument data, which may take a while.

;; The linter will try to "include" any require'd libraries to find
;; the variables defined in those.  There is a fair amount of voodoo
;; involved in this, but it seems to work in normal situations.

;;; To do:

;; * List of variables and functions defined in dumped lisp files.
;; * Adding type checking. (Stop that sniggering!)
;; * Handle eval-when-compile (eg for requires, being sensitive to the
;;   difference between funcs and macros).
;; * Requires within function bodies.

;;; Code:

(defvar elint-log-buffer "*Elint*"
  "*The buffer to insert lint messages in.")

;;;
;;; Data
;;;


;; FIXME does this serve any useful purpose now elint-builtin-variables exists?
(defconst elint-standard-variables '(local-write-file-hooks vc-mode)
  "Standard buffer local variables, excluding `elint-builtin-variables'.")

(defvar elint-builtin-variables nil
  "List of built-in variables.  Set by `elint-initialize'.")

(defvar elint-autoloaded-variables nil
  "List of `loaddefs.el' variables.  Set by `elint-initialize'.")

;; FIXME dumped variables and functions.

(defconst elint-unknown-builtin-args nil
  "Those built-ins for which we can't find arguments, if any.")

(defconst elint-extra-errors '(file-locked file-supersession ftp-error)
  "Errors without error-message or error-confitions properties.")

;;;
;;; ADT: top-form
;;;

(defsubst elint-make-top-form (form pos)
  "Create a top form.
FORM is the form, and POS is the point where it starts in the buffer."
  (cons form pos))

(defsubst elint-top-form-form (top-form)
  "Extract the form from a TOP-FORM."
  (car top-form))

(defsubst elint-top-form-pos (top-form)
  "Extract the position from a TOP-FORM."
  (cdr top-form))

;;;
;;; ADT: env
;;;

(defsubst elint-make-env ()
  "Create an empty environment."
  (list (list nil) nil nil))

(defsubst elint-env-add-env (env newenv)
  "Augment ENV with NEWENV.
None of them is modified, and the new env is returned."
  (list (append (car env) (car newenv))
	(append (cadr env) (cadr newenv))
	(append (car (cdr (cdr env))) (car (cdr (cdr newenv))))))

(defsubst elint-env-add-var (env var)
  "Augment ENV with the variable VAR.
The new environment is returned, the old is unmodified."
  (cons (cons (list var) (car env)) (cdr env)))

(defsubst elint-env-add-global-var (env var)
  "Augment ENV with the variable VAR.
ENV is modified so VAR is seen everywhere.
ENV is returned."
  (nconc (car env) (list (list var)))
  env)

(defsubst elint-env-find-var (env var)
  "Non-nil if ENV contains the variable VAR.
Actually, a list with VAR as a single element is returned."
  (assq var (car env)))

(defsubst elint-env-add-func (env func args)
  "Augment ENV with the function FUNC, which has the arguments ARGS.
The new environment is returned, the old is unmodified."
  (list (car env)
	(cons (list func args) (cadr env))
	(car (cdr (cdr env)))))

(defsubst elint-env-find-func (env func)
  "Non-nil if ENV contains the function FUNC.
Actually, a list of (FUNC ARGS) is returned."
  (assq func (cadr env)))

(defsubst elint-env-add-macro (env macro def)
  "Augment ENV with the macro named MACRO.
DEF is the macro definition (a lambda expression or similar).
The new environment is returned, the old is unmodified."
  (list (car env)
	(cadr env)
	(cons (cons macro def) (car (cdr (cdr env))))))

(defsubst elint-env-macro-env (env)
  "Return the macro environment of ENV.
This environment can be passed to `macroexpand'."
  (car (cdr (cdr env))))

(defsubst elint-env-macrop (env macro)
  "Non-nil if ENV contains MACRO."
  (assq macro (elint-env-macro-env env)))

;;;
;;; User interface
;;;

;;;###autoload
(defun elint-file (file)
  "Lint the file FILE."
  (interactive "fElint file: ")
  (setq file (expand-file-name file))
  (or elint-builtin-variables
      (elint-initialize))
  (let ((dir (file-name-directory file)))
    (let ((default-directory dir))
      (elint-display-log))
    (elint-set-mode-line t)
    (with-current-buffer elint-log-buffer
      (unless (string-equal default-directory dir)
	(elint-log-message (format "\nLeaving directory `%s'"
				   default-directory) t)
	(elint-log-message (format "Entering directory `%s'" dir) t)
	(setq default-directory dir))))
  (let ((str (format "Linting file %s" file)))
    (message "%s..." str)
    (or noninteractive
	(elint-log-message (format "\n%s at %s" str (current-time-string)) t))
    ;; elint-current-buffer clears log.
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file)
	    (max-lisp-eval-depth (max 1000 max-lisp-eval-depth)))
	(with-syntax-table emacs-lisp-mode-syntax-table
	  (mapc 'elint-top-form (elint-update-env)))))
    (elint-set-mode-line)
    (message "%s...done" str)))

;; cf byte-recompile-directory.
;;;###autoload
(defun elint-directory (directory)
  "Lint all the .el files in DIRECTORY."
  (interactive "DElint directory: ")
  (let ((elint-running t))
    (dolist (file (directory-files directory t))
      ;; Bytecomp has emacs-lisp-file-regexp.
      (when (and (string-match "\\.el\\'" file)
		 (file-readable-p file)
		 (not (auto-save-file-name-p file)))
	(elint-file file))))
  (elint-set-mode-line))

;;;###autoload
(defun elint-current-buffer ()
  "Lint the current buffer.
If necessary, this first calls `elint-initalize'."
  (interactive)
  (or elint-builtin-variables
      (elint-initialize))
  (elint-clear-log (format "Linting %s" (or (buffer-file-name)
					    (buffer-name))))
  (elint-display-log)
  (elint-set-mode-line t)
  (mapc 'elint-top-form (elint-update-env))
  ;; Tell the user we're finished.  This is terribly klugy: we set
    ;; elint-top-form-logged so elint-log-message doesn't print the
    ;; ** top form ** header...
  (elint-set-mode-line)
  (elint-log-message "\nLinting finished.\n" t))


;;;###autoload
(defun elint-defun ()
  "Lint the function at point.
If necessary, this first calls `elint-initalize'."
  (interactive)
  (or elint-builtin-variables
      (elint-initialize))
  (save-excursion
    (or (beginning-of-defun) (error "Lint what?"))
    (let ((pos (point))
	  (def (read (current-buffer))))
      (elint-display-log)
      (elint-update-env)
      (elint-top-form (elint-make-top-form def pos)))))

;;;
;;; Top form functions
;;;

(defvar elint-buffer-env nil
  "The environment of a elisp buffer.
Will be local in linted buffers.")

(defvar elint-buffer-forms nil
  "The top forms in a buffer.
Will be local in linted buffers.")

(defvar elint-last-env-time nil
  "The last time the buffers env was updated.
Is measured in buffer-modified-ticks and is local in linted buffers.")

(defun elint-update-env ()
  "Update the elint environment in the current buffer.
Don't do anything if the buffer hasn't been changed since this
function was called the last time.
Returns the forms."
  (if (and (local-variable-p 'elint-buffer-env (current-buffer))
	   (local-variable-p 'elint-buffer-forms (current-buffer))
	   (local-variable-p 'elint-last-env-time (current-buffer))
	   (= (buffer-modified-tick) elint-last-env-time))
      ;; Env is up to date
      elint-buffer-forms
    ;; Remake env
    (set (make-local-variable 'elint-buffer-forms) (elint-get-top-forms))
    (set (make-local-variable 'elint-buffer-env)
	 (elint-init-env elint-buffer-forms))
    (set (make-local-variable 'elint-last-env-time) (buffer-modified-tick))
    elint-buffer-forms))

(defun elint-get-top-forms ()
  "Collect all the top forms in the current buffer."
  (save-excursion
    (let ((tops nil))
      (goto-char (point-min))
      (while (elint-find-next-top-form)
	(let ((pos (point)))
	  (condition-case nil
	      (setq tops (cons
			  (elint-make-top-form (read (current-buffer)) pos)
			  tops))
	    (end-of-file
	     (goto-char pos)
	     (error "Missing ')' in top form: %s"
		    (buffer-substring pos (line-end-position)))))))
      (nreverse tops))))

(defun elint-find-next-top-form ()
  "Find the next top form from point.
Return nil if there are no more forms, t otherwise."
  (parse-partial-sexp (point) (point-max) nil t)
  (not (eobp)))

(defun elint-init-env (forms)
  "Initialize the environment from FORMS."
  (let ((env (elint-make-env))
	form)
    (while forms
      (setq form (elint-top-form-form (car forms))
	    forms (cdr forms))
      (cond
       ;; Add defined variable
       ((memq (car form) '(defvar defconst defcustom))
	(setq env (elint-env-add-var env (cadr form))))
       ;; Add function
       ((memq (car form) '(defun defsubst))
	(setq env (elint-env-add-func env (cadr form) (nth 2 form))))
       ((eq (car form) 'define-derived-mode)
	(setq env (elint-env-add-func env (cadr form) ())
	      env (elint-env-add-var env (cadr form))))
       ;; FIXME it would be nice to check the autoloads are correct.
       ((eq (car form) 'autoload)
	(setq env (elint-env-add-func env (cadr (cadr form)) 'unknown)))
       ((eq (car form) 'declare-function)
	(setq env (elint-env-add-func env (cadr form)
				      (if (or (< (length form) 4)
					      (eq (nth 3 form) t))
					'unknown
					(nth 3 form)))))
       ((and (eq (car form) 'defalias) (listp (nth 2 form)))
	;; If the alias points to something already in the environment,
	;; add the alias to the environment with the same arguments.
	(let ((def (elint-env-find-func env (cadr (nth 2 form)))))
	  ;; FIXME warn if the alias target is unknown.
	  (setq env (elint-env-add-func env (cadr (cadr form))
					(if def (cadr def) 'unknown)))))
       ;; Add macro, both as a macro and as a function
       ((eq (car form) 'defmacro)
	(setq env (elint-env-add-macro env (cadr form)
				       (cons 'lambda (cddr form)))
	      env (elint-env-add-func env (cadr form) (nth 2 form))))
       ;; Import variable definitions
       ((eq (car form) 'require)
	(let ((name (eval (cadr form)))
	      (file (eval (nth 2 form))))
	  (setq env (elint-add-required-env env name file))))))
    env))

(defun elint-add-required-env (env name file)
  "Augment ENV with the variables defined by feature NAME in FILE."
  (condition-case nil
      (let* ((libname (if (stringp file)
			  file
			(symbol-name name)))

	     ;; First try to find .el files, then the raw name
	     (lib1 (locate-library (concat libname ".el") t))
	     (lib (or lib1 (locate-library libname t))))
	;; Clear the messages :-/
	(message nil)
	(if lib
	    (save-excursion
 	      ;;; (set-buffer (find-file-noselect lib))
 	      ;;; (elint-update-env)
 	      ;;; (setq env (elint-env-add-env env elint-buffer-env)))
	      (with-temp-buffer
		(insert-file-contents lib)
		(with-syntax-table emacs-lisp-mode-syntax-table
		  (elint-update-env))
		(setq env (elint-env-add-env env elint-buffer-env))))
	      ;;(message "Elint processed (require '%s)" name))
	  (error "Unable to find require'd library %s" name)))
    (error
     (message "Can't get variables from require'd library %s" name)))
  env)

(defvar elint-top-form nil
  "The currently linted top form, or nil.")

(defvar elint-top-form-logged nil
  "T if the currently linted top form has been mentioned in the log buffer.")

(defun elint-top-form (form)
  "Lint a top FORM."
  (let ((elint-top-form form)
	(elint-top-form-logged nil)
	(elint-current-pos (elint-top-form-pos form)))
    (elint-form (elint-top-form-form form) elint-buffer-env)))

;;;
;;; General form linting functions
;;;

(defconst elint-special-forms
  '((let . elint-check-let-form)
    (let* . elint-check-let-form)
    (setq . elint-check-setq-form)
    (quote . elint-check-quote-form)
    (cond . elint-check-cond-form)
    (lambda . elint-check-defun-form)
    (function . elint-check-function-form)
    (setq-default . elint-check-setq-form)
    (defun . elint-check-defun-form)
    (defsubst . elint-check-defun-form)
    (defmacro . elint-check-defun-form)
    (defvar . elint-check-defvar-form)
    (defconst . elint-check-defvar-form)
    (defcustom . elint-check-defcustom-form)
    (macro . elint-check-macro-form)
    (condition-case . elint-check-condition-case-form))
  "Functions to call when some special form should be linted.")

(defun elint-form (form env)
  "Lint FORM in the environment ENV.
The environment created by the form is returned."
  (cond
   ((consp form)
    (let ((func (cdr (assq (car form) elint-special-forms))))
      (if func
	  ;; Special form
	  (funcall func form env)

	(let* ((func (car form))
	       (args (elint-get-args func env))
	       (argsok t))
	  (cond
	   ((eq args 'undefined)
	    (setq argsok nil)
	    (elint-error "Call to undefined function: %s" func))

	   ((eq args 'unknown) nil)

	   (t (setq argsok (elint-match-args form args))))

	  ;; Is this a macro?
	  (if (elint-env-macrop env func)
	      ;; Macro defined in buffer, expand it
	      (if argsok
		  ;; FIXME error if macro uses macro, eg bytecomp.el.
		  (condition-case nil
		      (elint-form
		       (macroexpand form (elint-env-macro-env env)) env)
		    (error
		     (elint-error "Elint failed to expand macro: %s" func)
		     env))
		env)

	    (let ((fcode (if (symbolp func)
			     (if (fboundp func)
				 (indirect-function func))
			   func)))
	      (if (and (listp fcode) (eq (car fcode) 'macro))
		  ;; Macro defined outside buffer
		  (if argsok
		      (elint-form (macroexpand form) env)
		    env)
		;; Function, lint its parameters
		(elint-forms (cdr form) env))))))))
   ((symbolp form)
    ;; :foo variables are quoted
    (if (and (/= (aref (symbol-name form) 0) ?:)
	     (elint-unbound-variable form env))
	(elint-warning "Reference to unbound symbol: %s" form))
    env)

   (t env)))

(defun elint-forms (forms env)
  "Lint the FORMS, accumulating an environment, starting with ENV."
  ;; grumblegrumbletailrecursiongrumblegrumble
  (if (listp forms)
      (dolist (f forms env)
	(setq env (elint-form f env)))
    ;; Loop macro?
    (elint-error "Elint failed to parse form: %s" forms)
    env))

(defun elint-unbound-variable (var env)
  "T if VAR is unbound in ENV."
  (not (or (memq var '(nil t))
	   (elint-env-find-var env var)
	   (memq var elint-builtin-variables)
	   (memq var elint-autoloaded-variables)
	   (memq var elint-standard-variables))))

;;;
;;; Function argument checking
;;;

(defun elint-match-args (arglist argpattern)
  "Match ARGLIST against ARGPATTERN."
  (let ((state 'all)
	(al (cdr arglist))
	(ap argpattern)
	(ok t))
    (while
	(cond
	 ((and (null al) (null ap)) nil)
	 ((eq (car ap) '&optional)
	  (setq state 'optional)
	  (setq ap (cdr ap))
	  t)
	 ((eq (car ap) '&rest)
	  nil)
	 ((or (and (eq state 'all) (or (null al) (null ap)))
	      (and (eq state 'optional) (and al (null ap))))
	  (elint-error "Wrong number of args: %s, %s" arglist argpattern)
	  (setq ok nil)
	  nil)
	 ((and (eq state 'optional) (null al))
	  nil)
	 (t (setq al (cdr al)
		  ap (cdr ap))
	    t)))
    ok))

(defun elint-get-args (func env)
  "Find the args of FUNC in ENV.
Returns `unknown' if we couldn't find arguments."
  (let ((f (elint-env-find-func env func)))
    (if f
	(cadr f)
      (if (symbolp func)
	  (if (fboundp func)
	      (let ((fcode (indirect-function func)))
		(if (subrp fcode)
		    ;; FIXME builtins with no args have args = nil.
		    (or (get func 'elint-args) 'unknown)
		  (elint-find-args-in-code fcode)))
	    'undefined)
	(elint-find-args-in-code func)))))

(defun elint-find-args-in-code (code)
  "Extract the arguments from CODE.
CODE can be a lambda expression, a macro, or byte-compiled code."
  (cond
   ((byte-code-function-p code)
    (aref code 0))
   ((and (listp code) (eq (car code) 'lambda))
    (car (cdr code)))
   ((and (listp code) (eq (car code) 'macro))
    (elint-find-args-in-code (cdr code)))
   (t 'unknown)))

;;;
;;; Functions to check some special forms
;;;

(defun elint-check-cond-form (form env)
  "Lint a cond FORM in ENV."
  (dolist (f (cdr form) env)
    (if (consp f)
	(elint-forms f env)
      (elint-error "cond clause should be a list: %s" f))))

(defun elint-check-defun-form (form env)
  "Lint a defun/defmacro/lambda FORM in ENV."
  (setq form (if (eq (car form) 'lambda) (cdr form) (cddr form)))
  (mapc (lambda (p)
	  (or (memq p '(&optional &rest))
	      (setq env (elint-env-add-var env p))))
	(car form))
  (elint-forms (cdr form) env))

(defun elint-check-let-form (form env)
  "Lint the let/let* FORM in ENV."
  (let ((varlist (cadr form)))
    (if (not varlist)
	(progn
	  (elint-error "Missing varlist in let: %s" form)
	  env)
      ;; Check for (let (a (car b)) ...) type of error
      (if (and (= (length varlist) 2)
	       (symbolp (car varlist))
	       (listp (car (cdr varlist)))
	       (fboundp (car (car (cdr varlist)))))
	  (elint-warning "Suspect varlist: %s" form))
      ;; Add variables to environment, and check the init values
      (let ((newenv env))
	(mapc (lambda (s)
		(cond
		 ((symbolp s)
		  (setq newenv (elint-env-add-var newenv s)))
		 ((and (consp s) (<= (length s) 2))
		  (elint-form (cadr s)
			      (if (eq (car form) 'let)
				  env
				newenv))
		  (setq newenv
			(elint-env-add-var newenv (car s))))
		 (t (elint-error
		     "Malformed `let' declaration: %s" s))))
	      varlist)

	;; Lint the body forms
	(elint-forms (cddr form) newenv)))))

(defun elint-check-setq-form (form env)
  "Lint the setq FORM in ENV."
  (or (= (mod (length form) 2) 1)
      (elint-error "Missing value in setq: %s" form))
  (let ((newenv env)
	sym val)
    (setq form (cdr form))
    (while form
      (setq sym (car form)
	    val (car (cdr form))
	    form (cdr (cdr form)))
      (if (symbolp sym)
	  (if (elint-unbound-variable sym newenv)
	      (elint-warning "Setting previously unbound symbol: %s" sym))
	(elint-error "Setting non-symbol in setq: %s" sym))
      (elint-form val newenv)
      (if (symbolp sym)
	  (setq newenv (elint-env-add-var newenv sym))))
    newenv))

(defun elint-check-defvar-form (form env)
  "Lint the defvar/defconst FORM in ENV."
  (if (or (= (length form) 2)
	  (= (length form) 3)
	  (and (= (length form) 4) (stringp (nth 3 form))))
          (elint-env-add-global-var (elint-form (nth 2 form) env)
			     (car (cdr form)))
    (elint-error "Malformed variable declaration: %s" form)
    env))

(defun elint-check-defcustom-form (form env)
  "Lint the defcustom FORM in ENV."
  (if (and (> (length form) 3)
	   ;; even no. of keyword/value args ?
	   (zerop (logand (length form) 1)))
      (elint-env-add-global-var (elint-form (nth 2 form) env)
				(car (cdr form)))
    (elint-error "Malformed variable declaration: %s" form)
    env))

(defun elint-check-function-form (form env)
  "Lint the function FORM in ENV."
  (let ((func (car (cdr-safe form))))
    (cond
     ((symbolp func)
      (or (elint-env-find-func env func)
	  (fboundp func)
	  (elint-warning "Reference to undefined function: %s" form))
      env)
     ((and (consp func) (memq (car func) '(lambda macro)))
      (elint-form func env))
     ((stringp func) env)
     (t (elint-error "Not a function object: %s" form)
	env))))

(defun elint-check-quote-form (form env)
  "Lint the quote FORM in ENV."
  env)

(defun elint-check-macro-form (form env)
  "Check the macro FORM in ENV."
  (elint-check-function-form (list (car form) (cdr form)) env))

(defun elint-check-condition-case-form (form env)
  "Check the `condition-case' FORM in ENV."
  (let ((resenv env))
    (if (< (length form) 3)
	(elint-error "Malformed condition-case: %s" form)
      (or (symbolp (cadr form))
	  (elint-warning "First parameter should be a symbol: %s" form))
      (setq resenv (elint-form (nth 2 form) env))
      (let ((newenv (elint-env-add-var env (cadr form)))
	    errlist)
	(dolist (err (nthcdr 3 form))
	  (setq errlist (car err))
	  (mapc (lambda (s)
		  (or (get s 'error-conditions)
		      (get s 'error-message)
		      (memq s elint-extra-errors)
		      (elint-warning
		       "Not an error symbol in error handler: %s" s)))
		(cond
		 ((symbolp errlist) (list errlist))
		 ((listp errlist) errlist)
		 (t (elint-error "Bad error list in error handler: %s"
				 errlist)
		    nil)))
	  (elint-forms (cdr err) newenv))))
    resenv))

;;;
;;; Message functions
;;;

(defvar elint-current-pos)	 ; dynamically bound in elint-top-form

(defun elint-log (type string args)
  (elint-log-message (format "%s:%d:%s: %s"
			     (let ((f (buffer-file-name)))
			       (if f
				   (file-name-nondirectory f)
				 (buffer-name)))
			     (save-excursion
			       (goto-char elint-current-pos)
			       (1+ (count-lines (point-min)
						(line-beginning-position))))
			     type
			     (apply 'format string args))))

(defun elint-error (string &rest args)
  "Report a linting error.
STRING and ARGS are thrown on `format' to get the message."
  (elint-log "Error" string args))

(defun elint-warning (string &rest args)
  "Report a linting warning.
See `elint-error'."
  (elint-log "Warning" string args))

(defun elint-output (string)
  "Print or insert STRING, depending on value of `noninteractive'."
  (if noninteractive
      (message "%s" string)
    (insert string "\n")))

(defun elint-log-message (errstr &optional top)
  "Insert ERRSTR last in the lint log buffer.
Optional argument TOP non-nil means pretend `elint-top-form-logged' is non-nil."
  (with-current-buffer (elint-get-log-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (or (bolp) (newline))
      ;; Do we have to say where we are?
      (unless (or elint-top-form-logged top)
	(let* ((form (elint-top-form-form elint-top-form))
	       (top (car form)))
	  (elint-output (cond
			 ((memq top '(defun defsubst))
			  (format "\nIn function %s:" (cadr form)))
			 ((eq top 'defmacro)
			  (format "\nIn macro %s:" (cadr form)))
			 ((memq top '(defvar defconst))
			  (format "\nIn variable %s:" (cadr form)))
			 (t "\nIn top level expression:"))))
	(setq elint-top-form-logged t))
      (elint-output errstr))))

(defun elint-clear-log (&optional header)
  "Clear the lint log buffer.
Insert HEADER followed by a blank line if non-nil."
  (let ((dir default-directory))
    (with-current-buffer (elint-get-log-buffer)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(if header (insert header "\n"))))))

(defun elint-display-log ()
  "Display the lint log buffer."
  (let ((pop-up-windows t))
    (display-buffer (elint-get-log-buffer))
    (sit-for 0)))

(defvar elint-running)

(defun elint-set-mode-line (&optional on)
  "Set the mode-line-process of the Elint log buffer."
  (with-current-buffer (elint-get-log-buffer)
    (and (eq major-mode 'compilation-mode)
	 (setq mode-line-process
	       (list (if (or on (bound-and-true-p elint-running))
			 (propertize ":run" 'face 'compilation-warning)
		       (propertize ":finished" 'face 'compilation-info)))))))

(defun elint-get-log-buffer ()
  "Return a log buffer for elint."
  (or (get-buffer elint-log-buffer)
      (with-current-buffer (get-buffer-create elint-log-buffer)
	(or (eq major-mode 'compilation-mode)
	    (compilation-mode))
	(setq buffer-undo-list t)
	(current-buffer))))

;;;
;;; Initializing code
;;;

;;;###autoload
(defun elint-initialize (&optional reinit)
  "Initialize elint.
If elint is already initialized, this does nothing, unless
optional prefix argument REINIT is non-nil."
  (interactive "P")
  (if (and elint-builtin-variables (not reinit))
      (message "Elint is already initialized")
    (message "Initializing elint...")
    (setq elint-builtin-variables (elint-find-builtin-variables)
	  elint-autoloaded-variables (elint-find-autoloaded-variables))
    (mapc (lambda (x) (or (not (symbolp (car x)))
			  (eq (cdr x) 'unknown)
			  (put (car x) 'elint-args (cdr x))))
	  (elint-find-builtin-args))
    (if elint-unknown-builtin-args
	(mapc (lambda (x) (put (car x) 'elint-args (cdr x)))
	      elint-unknown-builtin-args))
    (message "Initializing elint...done")))


(defun elint-find-builtin-variables ()
  "Return a list of all built-in variables."
  ;; Cribbed from help-fns.el.
  (let ((docbuf " *DOC*")
	vars var)
    (save-excursion
      (if (get-buffer docbuf)
	  (progn
	    (set-buffer docbuf)
	    (goto-char (point-min)))
	(set-buffer (get-buffer-create docbuf))
	(insert-file-contents-literally
	 (expand-file-name internal-doc-file-name doc-directory)))
      (while (search-forward "V" nil t)
	(and (setq var (intern-soft
			(buffer-substring (point) (line-end-position))))
	     (boundp var)
	     (setq vars (cons var vars))))
      vars)))

(defun elint-find-autoloaded-variables ()
  "Return a list of all autoloaded variables."
  (let (var vars)
    (with-temp-buffer
      (insert-file-contents (locate-library "loaddefs.el"))
      (while (re-search-forward "^(defvar \\([[:alnum:]_-]+\\)" nil t)
	(and (setq var (intern-soft (match-string 1)))
	     (boundp var)
	     (setq vars (cons var vars)))))
    vars))

(defun elint-find-builtins ()
  "Return a list of all built-in functions."
  (let (subrs)
    (mapatoms (lambda (s) (and (fboundp s) (subrp (symbol-function s))
			       (setq subrs (cons s subrs)))))
    subrs))

(defun elint-find-builtin-args (&optional list)
  "Return a list of the built-in functions and their arguments.
If LIST is nil, call `elint-find-builtins' to get a list of all built-in
functions, otherwise use LIST.

Each function is represented by a cons cell:
\(function-symbol . args)
If no documentation could be found args will be `unknown'."
  (mapcar (lambda (f)
	    (let ((doc (documentation f t)))
	      (or (and doc
		       (string-match "\n\n(fn\\(.*)\\)\\'" doc)
		       (ignore-errors
			;; "BODY...)" -> "&rest BODY)".
			(read (replace-regexp-in-string
			       "\\([^ ]+\\)\\.\\.\\.)\\'" "&rest \\1)"
			       (format "(%s %s" f (match-string 1 doc)) t))))
		  (cons f 'unknown))))
	  (or list (elint-find-builtins))))

(provide 'elint)

;; arch-tag: b2f061e2-af84-4ddc-8e39-f5e969ac228f
;;; elint.el ends here
