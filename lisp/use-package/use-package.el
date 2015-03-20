;;; use-package.el --- A use-package declaration for simplifying your .emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 17 Jun 2012
;; Version: 2.0
;; Package-Requires: ((bind-key "1.0") (diminish "0.44"))
;; Keywords: dotemacs startup speed config package
;; X-URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The `use-package' declaration macro allows you to isolate package
;; configuration in your ".emacs" in a way that is performance-oriented and,
;; well, just tidy.  I created it because I have over 80 packages that I use
;; in Emacs, and things were getting difficult to manage.  Yet with this
;; utility my total load time is just under 1 second, with no loss of
;; functionality!
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'bind-key)
(require 'bytecomp)
(require 'diminish nil t)
(require 'bytecomp)
(eval-when-compile (require 'cl))

(declare-function package-installed-p 'package)

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defcustom use-package-verbose nil
  "Whether to report about loading and configuration details.

If you customize this, then you should require the `use-package'
feature in files that use `use-package', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-minimum-reported-time 0.1
  "Minimal load time that will be reported.

Note that `use-package-verbose' has to be set to t, for anything
to be reported at all.

If you customize this, then you should require the `use-package'
feature in files that use `use-package', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type 'number
  :group 'use-package)

(defcustom use-package-inject-hooks nil
  "If non-nil, add hooks to the `:init' and `:config' sections.
In particular, for a given package `foo', the following hooks
become available:

  `use-package--foo--pre-init-hook'
  `use-package--foo--post-init-hook'
  `use-package--foo--pre-config-hook'
  `use-package--foo--post-config-hook'

This way, you can add to these hooks before evalaution of a
`use-package` declaration, and exercise some control over what
happens.

Note that if either `pre-init' hooks returns a nil value, that
block's user-supplied configuration is not evaluated, so be
certain to return `t' if you only wish to add behavior to what
the user specified."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-keywords
  '(:disabled
    :pin
    :ensure
    :if
    :when
    :unless
    :requires
    :load-path
    :no-require
    :preface
    :bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :commands
    :defines
    :functions
    :defer
    :demand
    :init
    :config
    :diminish)
  "Establish which keywords are valid, and the order they are processed in."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
This disables:
  - Printing to the *Messages* buffer of slowly-evaluating forms
  - Capture of load errors (normally redisplayed as warnings)
  - Conditional loading of packages (load failures become errors)
The only advantage is that, if you know your configuration works,
then your byte-compiled init file is as minimal as possible."
  :type 'boolean
  :group 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions
;;

(defun use-package-expand (name label form)
  "FORM is a list of forms, so `((foo))' if only `foo' is being called."
  (declare (indent 1))
  (when form
    (if use-package-expand-minimally
        form
      (let ((err (make-symbol "err")))
        (list
         `(condition-case-unless-debug ,err
              ,(macroexp-progn form)
            (error
             (ignore
              (display-warning 'use-package
                               (format "use-package: Error in %s: %s" ,name
                                       (error-message-string ,err))
                               :error)))))))))

(put 'use-package-expand 'lisp-indent-function 'defun)

(defun use-package-hook-injector (name-string keyword args)
  "Wrap pre/post hook injections around a given keyword form.
ARGS is a list of forms, so `((foo))' if only `foo' is being called."
  (if (not use-package-inject-hooks)
      (use-package-expand name-string (format "%s" keyword)
        (plist-get args keyword))
    (let ((keyword-name (substring (format "%s" keyword) 1))
          (block (plist-get args keyword)))
      (when block
        `((when ,(macroexp-progn
                  (use-package-expand name-string (format "pre-%s hook" keyword)
                    `(run-hook-with-args-until-failure
                      ',(intern (concat "use-package--" name-string
                                        "--pre-" keyword-name "-hook")))))
            ,(macroexp-progn
              (use-package-expand name-string (format "%s" keyword)
                (plist-get args keyword)))
            ,(macroexp-progn
              (use-package-expand name-string (format "post-%s hook" keyword)
                `(run-hooks
                  ',(intern (concat "use-package--" name-string
                                    "--post-" keyword-name "-hook")))))))))))

(defun use-package--with-elapsed-timer (text body)
  "BODY is a list of forms, so `((foo))' if only `foo' is being called."
  (declare (indent 1))
  (if use-package-expand-minimally
      body
    (let ((nowvar (make-symbol "now")))
      (if (bound-and-true-p use-package-verbose)
          `((let ((,nowvar (current-time)))
              (message "%s..." ,text)
              (prog1
                  ,(macroexp-progn body)
                (let ((elapsed
                       (float-time (time-subtract (current-time) ,nowvar))))
                  (if (> elapsed ,use-package-minimum-reported-time)
                      (message "%s...done (%.3fs)" ,text elapsed)
                    (message "%s...done" ,text))))))
        body))))

(put 'use-package--with-elapsed-timer 'lisp-indent-function 1)

(defsubst use-package-error (msg)
  "Report MSG as an error, so the user knows it came from this package."
  (error "use-package: %s" msg))

(defun use-package-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun use-package-split-list (pred xs)
  (let ((ys (list nil)) (zs (list nil)) flip)
    (dolist (x xs)
      (if flip
          (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

(defun use-package-keyword-index (keyword)
  (loop named outer
        with index = 0
        for k in use-package-keywords do
        (if (eq k keyword)
            (return-from outer index))
        (incf index)))

(defun use-package-sort-keywords (plist)
  (let (plist-grouped)
    (while plist
      (push (cons (car plist) (cadr plist))
            plist-grouped)
      (setq plist (cddr plist)))
    (append
     (sort plist-grouped
           #'(lambda (l r) (< (use-package-keyword-index (car l))
                         (use-package-keyword-index (car r))))))))

(defsubst use-package-cat-maybes (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'nconc (delete nil (delete (list nil) elems))))

(defconst use-package-font-lock-keywords
  '(("(\\(use-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Normalization functions
;;

(defsubst use-package-normalize-value (label arg)
  "Normalize a value."
  (cond ((symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun use-package-normalize-paths (label arg &optional recursed)
  "Normalize a list of filesystem paths."
  (cond
   ((or (symbolp arg) (functionp arg))
    (let ((value (use-package-normalize-value label arg)))
      (use-package-normalize-paths label (eval value))))
   ((stringp arg)
    (let ((path (if (file-name-absolute-p arg)
                    arg
                  (expand-file-name arg user-emacs-directory))))
      (list path)))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x)
                (car (use-package-normalize-paths label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a directory path, or list of paths")))))

(defun use-package-as-one (label args f)
  "Call F on the first element of ARGS if it has one element, or all of ARGS."
  (declare (indent 1))
  (if (and (listp args) (listp (cdr args)))
      (if (= (length args) 1)
          (funcall f label (car args))
        (funcall f label args))
    (use-package-error
     (concat label " wants a list"))))

(put 'use-package-as-one 'lisp-indent-function 'defun)

(defsubst use-package-is-sympair (x &optional allow-vector)
  "Return t if X has the type (STRING . SYMBOL)."
  (and (consp x)
       (or (stringp (car x))
           (and allow-vector (vectorp (car x))))
       (symbolp (cdr x))))

(defun use-package-normalize-pairs
    (name-symbol label arg &optional recursed allow-vector)
  "Normalize a list of string/symbol pairs."
  (cond
   ((or (stringp arg) (and allow-vector (vectorp arg)))
    (list (cons arg name-symbol)))
   ((use-package-is-sympair arg allow-vector)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-pairs
                                name-symbol label x t allow-vector))) arg))
   (t
    (use-package-error
     (concat label " wants a string, (string . symbol) or list of these")))))

(defun use-package-normalize-binder (name-symbol keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (use-package-normalize-pairs name-symbol label arg nil t))))

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

(defun use-package-normalize-mode (name-symbol keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-pairs name-symbol)))

(defalias 'use-package-normalize/:mode 'use-package-normalize-mode)
(defalias 'use-package-normalize/:interpreter 'use-package-normalize-mode)

(defun use-package-normalize-symbols (label arg &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((symbolp arg)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-symbols label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a symbol, or list of symbols")))))

(defun use-package-normalize-symlist (name-symbol keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-symbols))

(defalias 'use-package-normalize/:commands 'use-package-normalize-symlist)
(defalias 'use-package-normalize/:defines 'use-package-normalize-symlist)
(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)
(defalias 'use-package-normalize/:requires 'use-package-normalize-symlist)

(defun use-package-only-one (label args f)
  "Call F on the first member of ARGS if it has exactly one element."
  (declare (indent 1))
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (funcall f label (car args)))
   (t
    (use-package-error
     (concat label " wants exactly one argument")))))

(put 'use-package-only-one 'lisp-indent-function 'defun)

(defun use-package-normalize-predicate (name-symbol keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      #'use-package-normalize-value)))

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)
(defalias 'use-package-normalize/:demand 'use-package-normalize-predicate)
(defalias 'use-package-normalize/:disabled 'use-package-normalize-predicate)
(defalias 'use-package-normalize/:no-require 'use-package-normalize-predicate)

(defun use-package-normalize/:ensure (name-symbol keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      (lambda (label arg)
        (if (symbolp arg)
            arg
          (use-package-error
           (concat ":ensure wants an optional package name "
                   "(an unquoted symbol name)")))))))

(defun use-package-normalize-test (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defalias 'use-package-normalize/:if 'use-package-normalize-test)
(defalias 'use-package-normalize/:when 'use-package-normalize-test)

(defun use-package-normalize/:unless (name-symbol keyword args)
  (not (use-package-only-one (symbol-name keyword) args
         #'use-package-normalize-value)))

(defun use-package-normalize-diminish (name-symbol label arg &optional recursed)
  "Normalize the arguments to diminish down to a list of one of two forms:
     SYMBOL
     (SYMBOL . STRING)"
  (cond
   ((symbolp arg)
    (list arg))
   ((stringp arg)
    (list (cons (intern (concat (symbol-name name-symbol) "-mode")) arg)))
   ((and (consp arg) (stringp (cdr arg)))
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-diminish
                                name-symbol label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a string, symbol, "
             "(symbol . string) or list of these")))))

(defun use-package-normalize/:diminish (name-symbol keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-diminish name-symbol)))

(defun use-package-normalize-form (label args)
  "Given a list of forms, return it wrapped in `progn'."
  (unless (listp (car args))
    (use-package-error (concat label " wants a sexp or list of sexps")))
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (eq (car form) 'use-package))
                  (macroexpand form)
                form)) args))

(defun use-package-normalize-forms (name-symbol keyword args)
  (use-package-normalize-form (symbol-name keyword) args))

(defalias 'use-package-normalize/:preface 'use-package-normalize-forms)
(defalias 'use-package-normalize/:init 'use-package-normalize-forms)
(defalias 'use-package-normalize/:config 'use-package-normalize-forms)

(defun use-package-normalize/:load-path (name-symbol keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun use-package-normalize/:pin (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       ((symbolp arg) (symbol-name arg))
       (t
        (use-package-error
         ":pin wants an archive name (a string)"))))))

(defun use-package-normalize-plist (name-symbol input)
  "Given a pseudo-plist, normalize it to a regular plist."
  (unless (null input)
    (let* ((keyword (car input))
           (xs (use-package-split-list #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer (intern (concat "use-package-normalize/"
                                       (symbol-name keyword))))
           (arg
            (cond
             ((functionp normalizer)
              (funcall normalizer name-symbol keyword args))
             ((= (length args) 1)
              (car args))
             (t
              args))))
      (if (memq keyword use-package-keywords)
          (cons keyword
                (cons arg (use-package-normalize-plist name-symbol tail)))
        (use-package-error (format "Unrecognized keyword: %s" keyword))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyword processing
;;

(defun use-package-process-keywords (name-symbol plist state)
  "Process the next keyword in the free-form property list PLIST.
The values in the PLIST have each been normalized by the function
use-package-normalize/KEYWORD (minus the colon).

STATE is a property list that the function may modify and/or
query.  This is useful if a package defines multiple keywords and
wishes them to have some kind of stateful interaction.

Unless the KEYWORD being processed intends to ignore remaining
keywords, it must call this function recursively, passing in the
plist with its keyword and argument removed, and passing in the
next value for the STATE."
  (let ((plist* (use-package-sort-keywords
                 (use-package-normalize-plist name-symbol plist))))
    (unless (null plist*)
      (let* ((keyword (car plist*))
             (arg (cadr plist*))
             (rest (cddr plist*)))
        (unless (keywordp keyword)
          (use-package-error (format "%s is not a keyword" keyword)))
        (let* ((handler (concat "use-package-handler/"
                                (symbol-name keyword)))
               (handler-sym (intern handler)))
          (if (functionp handler-sym)
              (funcall handler-sym name-symbol keyword arg rest state)
            (use-package-error
             (format "Keyword handler not defined: %s" handler))))))))

(defun use-package-handler/:if (name-symbol keyword pred rest state)
  `((when ,pred
      ,@(use-package-process-keywords name-symbol rest state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The main macro
;;

(defun use--package (name name-symbol name-string args)
  "See docstring for `use-package'."
  (let*
      ((commands (plist-get args :commands))
       (deferral (plist-get args :defer))

       ;; Note: evaluation of this forms possibly extends the value of
       ;; `commands'.
       (bindings
        (append
         (mapcar #'(lambda (binding)
                     `(bind-key ,(car binding)
                                #'(lambda () (interactive)
                                    (use-package-autoload-keymap
                                     ',(cdr binding) ',name-symbol nil))))
                 (plist-get args :bind-keymap))

         (mapcar #'(lambda (binding)
                     `(bind-key ,(car binding)
                                #'(lambda () (interactive)
                                    (use-package-autoload-keymap
                                     ',(cdr binding) ',name-symbol t))))
                 (plist-get args :bind-keymap*))

         (mapcar #'(lambda (mode)
                     (push (cdr mode) commands)
                     `(push ',mode auto-mode-alist))
                 (plist-get args :mode))

         (mapcar #'(lambda (interpreter)
                     (push (cdr interpreter) commands)
                     `(push ',interpreter interpreter-mode-alist))
                 (plist-get args :interpreter))

         (mapcar #'(lambda (binding)
                     (push (cdr binding) commands)
                     `(bind-key ,(car binding) #',(cdr binding)))
                 (plist-get args :bind))

         (mapcar #'(lambda (binding)
                     (push (cdr binding) commands)
                     `(bind-key* ,(car binding) #',(cdr binding)))
                 (plist-get args :bind*))))

       ;; Should we defer loading of the package lazily?
       (defer-loading (and (not (plist-get args :demand))
                           (or commands deferral
                               (plist-get args :no-require)
                               (plist-get args :bind-keymap)
                               (plist-get args :bind-keymap*))))

       (pre-compile-load
        ;; When byte-compiling, load the package here so that all of its
        ;; symbols are in scope.
        (when (bound-and-true-p byte-compile-current-file)
          `((eval-when-compile
              ,@(mapcar #'(lambda (var) `(defvar ,var))
                        (plist-get args :defines))
              (with-demoted-errors
                  ,(format "Error in %s: %%S" name-string)
                ,(if use-package-verbose
                     `(message "Compiling package %s" ,name-string))
                ,(unless (plist-get args :no-require)
                   `(require ',name-symbol nil t)))))))

       ;; These are all the configurations to be made after the package has
       ;; loaded.
       (config-body
        (use-package--with-elapsed-timer
            (format "Configuring package %s" name-string)
          (use-package-cat-maybes
           (use-package-hook-injector name-string :config args)

           (mapcar #'(lambda (var)
                       (if (listp var)
                           `(diminish ',(car var) ,(cdr var))
                         `(diminish ',var)))
                   (plist-get args :diminish)))))

       (config-defun
        (make-symbol (concat "use-package--" name-string "--config"))))

    (setq commands (delete-dups commands))

    ;; Return the main body of the macro
    (use-package-cat-maybes
     ;; Setup the load-path
     (mapcar #'(lambda (path)
                 `(eval-and-compile (push ,path load-path)))
             (plist-get args :load-path))

     pre-compile-load

     (mapcar #'(lambda (form)
                 `(eval-and-compile ,form))
             (plist-get args :preface))

     ;; Setup any required autoloads
     (if defer-loading
         (apply
          #'nconc
          (mapcar #'(lambda (command)
                      (append
                       `((unless (fboundp ',command)
                           (autoload #',command ,name-string nil t)))
                       (when (bound-and-true-p byte-compile-current-file)
                         `((eval-when-compile
                             (declare-function ,command ,name-string))))))
                  commands)))

     (when (bound-and-true-p byte-compile-current-file)
       (mapcar #'(lambda (fn) `(eval-when-compile
                            (declare-function ,fn ,name-string)))
               (plist-get args :functions)))

     (if (numberp deferral)
         `((run-with-idle-timer ,deferral nil #'require ',name-symbol nil t)))

     ;; (if (and defer-loading config-body)
     ;;     `((defalias ',config-defun #'(lambda () ,config-body*))))

     ;; The user's initializations
     (use-package-hook-injector name-string :init args)

     (if defer-loading
         (use-package-cat-maybes
          bindings
          (if config-body
              `((eval-after-load ',name
                  ;; '(,config-defun)
                  ',(macroexp-progn config-body))))
          (list t))
       (use-package--with-elapsed-timer
           (format "Loading package %s" name-string)
         (if use-package-expand-minimally
             (use-package-cat-maybes
              (list `(require ',name-symbol))
              bindings
              config-body
              (list t))
           `((if (not (require ',name-symbol nil t))
                 (ignore
                  (display-warning
                   'use-package
                   (format "Could not load package %s" ,name-string)
                   :error))
               ,@(use-package-cat-maybes
                  bindings
                  config-body
                  (list t))))))))))

(defmacro use-package (name &rest args)
  "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init          Code to run before PACKAGE-NAME has been loaded.
:config        Code to run after PACKAGE-NAME has been loaded.  Note that if
               loading is deferred for any reason, this code does not execute
               until the lazy load has occurred.
:preface       Code to be run before everything except `:disabled'; this can
               be used to define functions for use in `:if', or that should be
               seen by the byte-compiler.

:mode          Form to be added to `auto-mode-alist'.
:interpreter   Form to be added to `interpreter-mode-alist'.

:commands      Define autoloads for commands that will be defined by the
               package.  This is useful if the package is being lazily loaded,
               and you wish to conditionally call functions in your `:init'
               block that are defined in the package.

:bind          Bind keys, and define autoloads for the bound commands.
:bind*         Bind keys, and define autoloads for the bound commands,
               *overriding all minor mode bindings*.
:bind-keymap   Bind a key prefix to an auto-loaded keymap defined in the
               package.  This is like `:bind', but for keymaps.
:bind-keymap*  Like `:bind-keymap', but overrides all minor mode bindings

:defer         Defer loading of a package -- this is implied when using
               `:commands', `:bind', `:bind*', `:mode' or `:interpreter'.
               This can be an integer, to force loading after N seconds of
               idle time, if the package has not already been loaded.
:demand        Prevent deferred loading in all cases.

:if EXPR       Initialize and load only if EXPR evaluates to a non-nil value.
:disabled      The package is ignored completely if this keyword is present.
:defines       Declare certain variables to silence the byte-compiler.
:functions     Declare certain functions to silence the byte-compiler.
:load-path     Add to the `load-path' before attempting to load the package.
:diminish      Support for diminish.el (if installed).
:ensure        Loads the package using package.el if necessary.
:pin           Pin the package to an archive."
  (declare (indent 1))
  (unless (member :disabled args)
    (let* ((name-string (if (stringp name) name (symbol-name name)))
           (name-symbol (if (stringp name) (intern name) name))
           (args* (use-package-normalize-plist name-symbol args))
           (archive-name (plist-get args* :pin))
           (ensure (plist-get args* :ensure))
           (package-name (or (and (eq ensure t) name) ensure)))
      ;; Pin any packages that have been marked with `:pin'.
      (when archive-name
        (use-package-pin-package name-symbol archive-name))

      ;; Ensure that the package has been installed, if marked with
      ;; `:ensure'.
      (when package-name
        (require 'package)
        (use-package-ensure-elpa package-name))

      ;; At this point, we can expand the macro using the helper function.
      ;; `use--package'.
      (let*
          ((body (use-package-cat-maybes
                  (use--package name name-symbol name-string args*)
                  (when archive-name
                    `((push '(,name-symbol . ,archive-name)
                            package-pinned-packages)))))
           (pred (plist-get args* :if))
           (expansion (if pred
                          `((when ,pred ,@body))
                        body))
           (requires (plist-get args* :requires))
           (body*
            (macroexp-progn
             (if (null requires)
                 expansion
               `((if ,(if (listp requires)
                          `(not (member nil (mapcar #'featurep ',requires)))
                        `(featurep ',requires))
                     ,@expansion))))))
        ;; (message "Expanded:\n%s" (pp-to-string body*))
        body*))))

(put 'use-package 'lisp-indent-function 'defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Special support for autoloading keymaps
;;

(defun use-package-autoload-keymap (keymap-symbol package override)
  "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL.  It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword.  It
works by binding the given key sequence to an invocation of this
function for a particular keymap.  The keymap is expected to be
defined by the package.  In this way, loading the package is
deferred until the prefix key sequence is pressed."
  (if (not (require package nil t))
      (error "Could not load package %s" package)
    (if (and (boundp keymap-symbol)
             (keymapp (symbol-value keymap-symbol)))
        (let ((key (key-description (this-command-keys-vector)))
              (keymap (symbol-value keymap-symbol)))
          (if override
              ;; eval form is necessary to avoid compiler error
              `(eval `(bind-key* ,key ,keymap))
            (bind-key key keymap))
          (setq unread-command-events
                (listify-key-sequence (this-command-keys-vector))))
      (error "use-package: package %s failed to define keymap %s"
             package keymap-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; :pin and :ensure support
;;

(eval-when-compile
  (defvar package-pinned-packages)
  (defvar package-archives))

(defun use-package-pin-package (package archive)
  "Pin PACKAGE to ARCHIVE."
  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))
  (let ((archive-symbol (if (symbolp archive) archive (intern archive)))
        (archive-name   (if (stringp archive) archive (symbol-name archive))))
    (if (use-package--archive-exists-p archive-symbol)
        (push (cons package archive-name) package-pinned-packages)
      (error "Archive '%s' requested for package '%s' is not available."
             archive-name package))
    (package-initialize t)))

(defun use-package--archive-exists-p (archive)
  "Check if a given ARCHIVE is enabled.

ARCHIVE can be a string or a symbol or 'manual to indicate a
manually updated package."
  (if (member archive '(manual "manual"))
      't
    (let ((valid nil))
      (dolist (pa package-archives)
        (when (member archive (list (car pa) (intern (car pa))))
          (setq valid 't)))
      valid)))

(defun use-package-ensure-elpa (package)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'use-package)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; use-package.el ends here
