;;; up-core.el --- A configuration macro for simplifying your .emacs

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 29 Nov 2017
;; Version: 2.4
;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
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
(require 'cl-lib)

(eval-when-compile
  (require 'cl)
  (require 'regexp-opt))

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defconst use-package-version "2.4"
  "This version of use-package.")

(defcustom use-package-keywords
  '(:disabled
    :if :when :unless
    :requires
    :load-path
    :no-require
    :defines
    :functions
    :preface
    :after
    :custom
    :custom-face
    :init
    :bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :magic
    :magic-fallback
    :hook
    ;; Any other keyword that also declares commands to be autoloaded (such as
    ;; :bind) must appear before this keyword.
    :commands
    :defer
    :demand
    :load
    ;; This must occur almost last; the only forms which should appear after
    ;; are those that must happen directly after the config forms.
    :config)
  "The set of valid keywords, in the order they are processed in.
The order of this list is *very important*, so it is only
advisable to insert new keywords, never to delete or reorder
them. Further, attention should be paid to the NEWS.md if the
default order ever changes, as they may have subtle effects on
the semantics of use-package declarations and may necessitate
changing where you had inserted a new keyword earlier.

Note that `:disabled' is special in this list, as it causes
nothing at all to happen, even if the rest of the use-package
declaration is incorrect."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-deferring-keywords
  '(:bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :magic
    :magic-fallback
    :commands
    :hook)
  "Unless `:demand' is used, keywords in this list imply deferred loading."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-verbose nil
  "Whether to report about loading and configuration details.
If you customize this, then you should require the `use-package'
feature in files that use `use-package', even if these files only
contain compiled expansions of the macros. If you don't do so,
then the expanded macros do their job silently."
  :type '(choice (const :tag "Quiet, without catching errors" errors)
                 (const :tag "Quiet" nil)
                 (const :tag "Verbose" t)
                 (const :tag "Debug" debug))
  :group 'use-package)

(defcustom use-package-check-before-init nil
  "If non-nil, check that package exists before executing its `:init' block.
This check is performed by calling `locate-library'."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-defer nil
  "If non-nil, assume `:defer t' unless `:demand' is used.
See also `use-package-defaults', which uses this value."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-demand nil
  "If non-nil, assume `:demand t' unless `:defer' is used.
See also `use-package-defaults', which uses this value."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-defaults
  '(;; this '(t) has special meaning; see `use-package-handler/:config'
    (:config '(t) t)
    (:init nil t)
    (:defer use-package-always-defer
            (lambda (args)
              (and use-package-always-defer
                   (not (plist-member args :defer))
                   (not (plist-member args :demand)))))
    (:demand use-package-always-demand
             (lambda (args)
               (and use-package-always-demand
                    (not (plist-member args :defer))
                    (not (plist-member args :demand))))))
  "Alist of default values for `use-package' keywords.
Each entry in the alist is a list of three elements. The first
element is the `use-package' keyword and the second is a form
that can be evaluated to get the default value. The third element
is a form that can be evaluated to determine whether or not to
assign a default value; if it evaluates to nil, then the default
value is not assigned even if the keyword is not present in the
`use-package' form. This third element may also be a function, in
which case it receives the list of keywords (in normalized form),
and should return nil or t according to whether defaulting should
be attempted."
  :type `(repeat
          (list (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  use-package-keywords))
                (choice :tag "Default value" sexp)
                (choice :tag "Enable if non-nil" sexp function)))
  :group 'use-package)

(defcustom use-package-minimum-reported-time 0.1
  "Minimal load time that will be reported.
Note that `use-package-verbose' has to be set to a non-nil value
for anything to be reported at all."
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

This way, you can add to these hooks before evaluation of a
`use-package` declaration, and exercise some control over what
happens.

NOTE: These hooks are run even if the user does not specify an
`:init' or `:config' block, and they will happen at the regular
time when initialization and configuration would have been
performed.

NOTE: If the `pre-init' hook return a nil value, that block's
user-supplied configuration is not evaluated, so be certain to
return `t' if you only wish to add behavior to what the user
had specified."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
This disables:

  - Printing to the *Messages* buffer of slowly-evaluating forms
  - Capturing of load errors (normally redisplayed as warnings)
  - Conditional loading of packages (load failures become errors)

The main advantage to this variable is that, if you know your
configuration works, it will make the byte-compiled file as
minimal as possible. It can also help with reading macro-expanded
definitions, to understand the main intent of what's happening."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-form-regexp-eval
  `(concat ,(eval-when-compile
              (concat "^\\s-*("
                      (regexp-opt '("use-package" "require") t)
                      "\\s-+\\("))
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
  "Sexp providing regexp for finding use-package forms in user files.
This is used by `use-package-jump-to-package-form' and
`use-package-enable-imenu-support'."
  :type 'sexp
  :group 'use-package)

(defcustom use-package-enable-imenu-support nil
  "If non-nil, adjust `lisp-imenu-generic-expression' to include
support for finding `use-package' and `require' forms.

Must be set before loading use-package."
  :type 'boolean
  :set
  #'(lambda (sym value)
      (eval-after-load 'lisp-mode
        (if value
            `(add-to-list 'lisp-imenu-generic-expression
                          (list "Packages" ,use-package-form-regexp-eval 2))
          `(setq lisp-imenu-generic-expression
                 (remove (list "Packages" ,use-package-form-regexp-eval 2)
                         lisp-imenu-generic-expression)))))
  :group 'use-package)

(defconst use-package-font-lock-keywords
  '(("(\\(use-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

(defvar use-package--hush-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utility functions
;;

(defsubst use-package-error (msg)
  "Report MSG as an error, so the user knows it came from this package."
  (error "use-package: %s" msg))

(defsubst use-package-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst use-package-non-nil-symbolp (sym)
  (and sym (symbolp sym)))

(defsubst use-package-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defsubst use-package-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst use-package-regex-p (re)
  "Return t if RE is some regexp-like thing."
  (or (and (listp re) (eq (car re) 'rx))
      (stringp re)))

(defun use-package-normalize-regex (re)
  "Given some regexp-like thing, resolve it down to a regular expression."
  (cond
   ((and (listp re) (eq (car re) 'rx)) (eval re))
   ((stringp re) re)
   (t (error "Not recognized as regular expression: %s" re))))

(defsubst use-package-is-pair (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying the given predicates.
CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
respectively."
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))

(defun use-package-as-mode (string-or-symbol)
  "If STRING-OR-SYMBOL ends in `-mode' (or its name does), return
it as a symbol.  Otherwise, return it as a symbol with `-mode'
appended."
  (let ((string (use-package-as-string string-or-symbol)))
    (intern (if (string-match "-mode\\'" string)
                string
              (concat string "-mode")))))

(defsubst use-package-load-name (name &optional noerror)
  "Return a form which will load or require NAME depending on
whether it's a string or symbol."
  (if (stringp name)
      `(load ,name ,noerror)
    `(require ',name nil ,noerror)))

(defun use-package-hook-injector (name-string keyword body)
  "Wrap pre/post hook injections around a given keyword form.
ARGS is a list of forms, so `((foo))' if only `foo' is being called."
  (if (not use-package-inject-hooks)
      body
    (let ((keyword-name (substring (format "%s" keyword) 1)))
      `((when (run-hook-with-args-until-failure
               ',(intern (concat "use-package--" name-string
                                 "--pre-" keyword-name "-hook")))
          ,@body
          (run-hooks
           ',(intern (concat "use-package--" name-string
                             "--post-" keyword-name "-hook"))))))))

(defun use-package-with-elapsed-timer (text body)
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

(put 'use-package-with-elapsed-timer 'lisp-indent-function 1)

(defun use-package-require (name &optional no-require body)
  (if use-package-expand-minimally
      (use-package-concat
       (unless no-require
         (list (use-package-load-name name)))
       body)
    (if no-require
        body
      (use-package-with-elapsed-timer
          (format "Loading package %s" name)
        `((if (not ,(use-package-load-name name t))
              (ignore
               (display-warning 'use-package
                                (format "Cannot load %s" ',name)
                                :error))
            ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Property lists
;;

(defun use-package-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun use-package-plist-delete-first (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (eq property (car plist))
          (setq p (nconc p (cddr plist))
                plist nil)
        (setq p (nconc p (list (car plist) (cadr plist)))
              plist (cddr plist))))
    p))

(defsubst use-package-plist-maybe-put (plist property value)
  "Add a VALUE for PROPERTY to PLIST, if it does not already exist."
  (if (plist-member plist property)
      plist
    (plist-put plist property value)))

(defsubst use-package-plist-cons (plist property value)
  "Cons VALUE onto the head of the list at PROPERTY in PLIST."
  (plist-put plist property (cons value (plist-get plist property))))

(defsubst use-package-plist-append (plist property value)
  "Append VALUE onto the front of the list at PROPERTY in PLIST."
  (plist-put plist property (append value (plist-get plist property))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Keywords
;;

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
    (let (result)
      (dolist (x
               (nreverse
                (sort plist-grouped
                      #'(lambda (l r) (< (use-package-keyword-index (car l))
                                    (use-package-keyword-index (car r)))))))
        (setq result (cons (car x) (cons (cdr x) result))))
      result)))

(defun use-package-unalias-keywords (name args)
  (setq args (cl-nsubstitute :if :when args))
  (let (temp)
    (while (setq temp (plist-get args :unless))
      (setq args (use-package-plist-delete-first args :unless)
            args (append args `(:if (not ,temp))))))
  args)

(defun use-package-merge-keys (key new old)
  (pcase key
    (`:if `(and ,new ,old))
    (`:after `(:all ,new ,old))
    (`:defer old)
    (_ (append new old))))

(defun use-package-normalize-keywords (name args)
  (let* ((name-symbol (if (stringp name) (intern name) name))
         (name-string (symbol-name name-symbol)))

    ;; Reduce the set of keywords down to its most fundamental expression.
    (setq args (use-package-unalias-keywords name-symbol args))

    ;; Normalize keyword values, coalescing multiple occurrences.
    (setq args (use-package-normalize-plist name-symbol args nil
                                            #'use-package-merge-keys))

    ;; Add default values for keywords not specified, when applicable.
    (dolist (spec use-package-defaults)
      (when (pcase (nth 2 spec)
              ((and func (pred functionp)) (funcall func args))
              (sexp (eval sexp)))
        (setq args (use-package-plist-maybe-put
                    args (nth 0 spec) (eval (nth 1 spec))))))

    ;; If byte-compiling, pre-load the package so all its symbols are in
    ;; scope. This is done by prepending statements to the :preface.
    (when (bound-and-true-p byte-compile-current-file)
      (setq args
            (use-package-plist-append
             args :preface
             (use-package-concat
              (mapcar #'(lambda (var) `(defvar ,var))
                      (plist-get args :defines))
              (mapcar #'(lambda (fn) `(declare-function ,fn ,name-string))
                      (plist-get args :functions))
              `((eval-when-compile
                  (with-demoted-errors
                      ,(format "Cannot load %s: %%S" name-string)
                    ,(when (eq use-package-verbose 'debug)
                       `(message ,(format "Compiling package %s" name-string)))
                    ,(unless (plist-get args :no-require)
                       `(load ,name-string nil t)))))))))

    ;; Certain keywords imply :defer, if :demand was not specified.
    (when (and (not (plist-member args :demand))
               (not (plist-member args :defer))
               (cl-some #'identity
                        (mapcar (apply-partially #'plist-member args)
                                use-package-deferring-keywords)))
      (setq args (append args '(:defer t))))

    (when (and (plist-member args :load)
               (plist-member args :no-require))
      (setq args (use-package-plist-delete args :no-require)))

    (when (and (not (plist-member args :load))
               (not (plist-member args :defer))
               (not (plist-member args :no-require)))
      (setq args (append args `(:load (,name)))))

    ;; Sort the list of keywords based on the order of `use-package-keywords'.
    (use-package-sort-keywords args)))

(defun use-package-normalize-plist (name input &optional plist merge-function)
  "Given a pseudo-plist, normalize it to a regular plist.
The normalized key/value pairs from input are added to PLIST,
extending any keys already present."
  (when input
    (let* ((keyword (car input))
           (xs (use-package-split-list #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer (intern (concat "use-package-normalize/"
                                       (symbol-name keyword))))
           (arg (cond ((functionp normalizer)
                       (funcall normalizer name keyword args))
                      ((= (length args) 1)
                       (car args))
                      (t
                       args))))
      (if (memq keyword use-package-keywords)
          (progn
            (setq plist (use-package-normalize-plist
                         name tail plist merge-function))
            (plist-put plist keyword
                       (if (plist-member plist keyword)
                           (funcall merge-function keyword
                                    arg (plist-get plist keyword))
                         arg)))
        (ignore
         (display-warning 'use-package
                          (format "Unrecognized keyword: %s" keyword)
                          :warning))))))

(defun use-package-process-keywords (name plist &optional state)
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
  (declare (indent 1))
  (unless (null plist)
    (let* ((keyword (car plist))
           (arg (cadr plist))
           (rest (cddr plist)))
      (unless (keywordp keyword)
        (use-package-error (format "%s is not a keyword" keyword)))
      (let* ((handler (concat "use-package-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym name keyword arg rest state)
          (use-package-error
           (format "Keyword handler not defined: %s" handler)))))))

(put 'use-package-process-keywords 'lisp-indent-function 'defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Argument Processing
;;

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

(defun use-package-as-one (label args f &optional allow-empty)
  "Call F on the first element of ARGS if it has one element, or all of ARGS.
If ALLOW-EMPTY is non-nil, it's OK for ARGS to be an empty list."
  (declare (indent 1))
  (if (if args
          (and (listp args) (listp (cdr args)))
        allow-empty)
      (if (= (length args) 1)
          (funcall f label (car args))
        (funcall f label args))
    (use-package-error
     (concat label " wants a non-empty list"))))

(put 'use-package-as-one 'lisp-indent-function 'defun)

(defun use-package-memoize (f arg)
  "Ensure the macro-expansion of F applied to ARG evaluates ARG
no more than once."
  (let ((loaded (gensym "use-package--loaded"))
        (result (gensym "use-package--result"))
        (next (gensym "use-package--next")))
    `((lexical-let (,loaded ,result)
        (lexical-let ((,next (lambda ()
                               (if ,loaded
                                   ,result
                                 (setq ,loaded t)
                                 (setq ,result ,arg)))))
          ,(funcall f ``(funcall ,,next)))))))

(defsubst use-package-normalize-value (label arg)
  "Normalize a value."
  (cond ((null arg) nil)
        ((eq t arg) t)
        ((use-package-non-nil-symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun use-package-normalize-symbols (label arg &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((use-package-non-nil-symbolp arg)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-symbols label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a symbol, or list of symbols")))))

(defun use-package-normalize-symlist (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-symbols))

(defun use-package-normalize-recursive-symbols (label arg)
  "Normalize a list of symbols."
  (cond
   ((use-package-non-nil-symbolp arg)
    arg)
   ((and (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (use-package-normalize-recursive-symbols label x))
            arg))
   (t
    (use-package-error
     (concat label " wants a symbol, or nested list of symbols")))))

(defun use-package-normalize-recursive-symlist (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-recursive-symbols))

(defun use-package-normalize-paths (label arg &optional recursed)
  "Normalize a list of filesystem paths."
  (cond
   ((and arg (or (use-package-non-nil-symbolp arg) (functionp arg)))
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

(defun use-package-normalize-predicate (name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      #'use-package-normalize-value)))

(defun use-package-normalize-form (label args)
  "Given a list of forms, return it wrapped in `progn'."
  (unless (listp (car args))
    (use-package-error (concat label " wants a sexp or list of sexps")))
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (eq (car form) 'use-package))
                  (macroexpand form)
                form)) args))

(defun use-package-normalize-forms (name keyword args)
  (use-package-normalize-form (symbol-name keyword) args))

(defun use-package-normalize-pairs
    (key-pred val-pred name label arg &optional recursed)
  "Normalize a list of pairs.
KEY-PRED and VAL-PRED are predicates recognizing valid keys and
values, respectively.
If RECURSED is non-nil, recurse into sublists."
  (cond
   ((funcall key-pred arg)
    (list (cons arg (use-package-as-symbol name))))
   ((use-package-is-pair arg key-pred val-pred)
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (let (last-item)
      (mapcar
       #'(lambda (x)
           (prog1
               (let ((ret (use-package-normalize-pairs
                           key-pred val-pred name label x t)))
                 ;; Currently, the handling of keyword arguments by
                 ;; `use-package' and `bind-key' is non-uniform and
                 ;; undocumented. As a result, `use-package-normalize-pairs'
                 ;; (as it is currently implemented) does not correctly handle
                 ;; the keyword-argument syntax of `bind-keys'. A permanent
                 ;; solution to this problem will require a careful
                 ;; consideration of the desired keyword-argument interface
                 ;; for `use-package' and `bind-key'. However, in the
                 ;; meantime, we have a quick patch to fix a serious bug in
                 ;; the handling of keyword arguments. Namely, the code below
                 ;; would normally unwrap lists that were passed as keyword
                 ;; arguments (for example, the `:filter' argument in `:bind')
                 ;; without the (not (keywordp last-item)) clause. See #447
                 ;; for further discussion.
                 (if (and (listp ret)
                          (not (keywordp last-item)))
                     (car ret)
                   ret))
             (setq last-item x))) arg)))
   (t arg)))

(defun use-package-recognize-function (v &optional binding additional-pred)
  "A predicate that recognizes functional constructions:
  nil
  sym
  'sym
  (quote sym)
  #'sym
  (function sym)
  (lambda () ...)
  '(lambda () ...)
  (quote (lambda () ...))
  #'(lambda () ...)
  (function (lambda () ...))"
  (pcase v
    ((and x (guard (if binding
                       (symbolp x)
                     (use-package-non-nil-symbolp x)))) t)
    (`(,(or `quote `function)
       ,(pred use-package-non-nil-symbolp)) t)
    ((and x (guard (if binding (commandp x) (functionp x)))) t)
    (_ (and additional-pred
            (funcall additional-pred v)))))

(defun use-package-normalize-function (v)
  "Reduce functional constructions to one of two normal forms:
  sym
  #'(lambda () ...)"
  (pcase v
    ((pred symbolp) v)
    (`(,(or `quote `function)
       ,(and sym (pred symbolp))) sym)
    (`(lambda . ,_) v)
    (`(quote ,(and lam `(lambda . ,_))) lam)
    (`(function ,(and lam `(lambda . ,_))) lam)
    (_ v)))

(defun use-package-normalize-commands (args)
  "Map over ARGS of the form ((_ . F) ...).
Normalizing functional F's and returning a list of F's
representing symbols (that may need to be autloaded)."
  (let ((nargs (mapcar
                #'(lambda (x)
                    (if (consp x)
                        (cons (car x)
                              (use-package-normalize-function (cdr x)))
                      x)) args)))
    (cons nargs
          (delete
           nil (mapcar
                #'(lambda (x)
                    (and (consp x)
                         (use-package-non-nil-symbolp (cdr x))
                         (cdr x))) nargs)))))

(defun use-package-normalize-binder (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (consp arg)
          (use-package-error
           (concat label " a (<string or vector> . <symbol, string or function>)"
                   " or list of these")))
        (use-package-normalize-pairs
         #'(lambda (k)
             (pcase k
               ((pred stringp) t)
               ((pred vectorp) t)))
         #'(lambda (v) (use-package-recognize-function v t #'stringp))
         name label arg))))

;;;###autoload
(defun use-package-autoload-keymap (keymap-symbol package override)
  "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed."
  (if (not (require package nil t))
      (use-package-error (format "Cannot load package.el: %s" package))
    (if (and (boundp keymap-symbol)
             (keymapp (symbol-value keymap-symbol)))
        (let* ((kv (this-command-keys-vector))
               (key (key-description kv))
               (keymap (symbol-value keymap-symbol)))
          (if override
              (bind-key* key keymap)
            (bind-key key keymap))
          (setq unread-command-events
                (listify-key-sequence kv)))
      (use-package-error
       (format "use-package: package.el %s failed to define keymap %s"
               package keymap-symbol)))))

(defun use-package-normalize-mode (name keyword args)
  "Normalize arguments for keywords which add regexp/mode pairs to an alist."
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-pairs
                     #'use-package-regex-p
                     #'use-package-recognize-function
                     name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Handlers
;;

;;;; :disabled

(defalias 'use-package-normalize/:disabled 'ignore)

(defun use-package-handler/:disabled (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;; :if, :when and :unless

(defun use-package-normalize-test (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defalias 'use-package-normalize/:if 'use-package-normalize-test)

(defun use-package-handler/:if (name keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((when ,pred ,@body))))

(defalias 'use-package-normalize/:when 'use-package-normalize-test)

(defalias 'use-package-handler/:when 'use-package-handler/:if)

(defalias 'use-package-normalize/:unless 'use-package-normalize-test)

(defun use-package-handler/:unless (name keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((unless ,pred ,@body))))

;;;; :requires

(defalias 'use-package-normalize/:requires 'use-package-normalize-symlist)

(defun use-package-handler/:requires (name keyword requires rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if (null requires)
        body
      `((when ,(if (> (length requires) 1)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',(car requires)))
          ,@body)))))

;;;; :load-path

(defun use-package-normalize/:load-path (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun use-package-handler/:load-path (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (path)
                 `(eval-and-compile (add-to-list 'load-path ,path))) arg)
     body)))

;;;; :no-require

(defalias 'use-package-normalize/:no-require 'use-package-normalize-predicate)

(defun use-package-handler/:no-require (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;; :preface

(defalias 'use-package-normalize/:preface 'use-package-normalize-forms)

(defun use-package-handler/:preface (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (when arg
       `((eval-and-compile ,@arg)))
     body)))

;;;; :defines

(defalias 'use-package-normalize/:defines 'use-package-normalize-symlist)

(defun use-package-handler/:defines (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;; :functions

(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)

(defun use-package-handler/:functions (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;; :bind, :bind*

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defun use-package-handler/:bind
    (name keyword args rest state &optional bind-macro)
  (cl-destructuring-bind (nargs . commands)
      (use-package-normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands commands))
       state)
     `((ignore
        (,(if bind-macro bind-macro 'bind-keys)
         :package ,name ,@nargs))))))

(defun use-package-handler/:bind* (name keyword arg rest state)
  (use-package-handler/:bind name keyword arg rest state 'bind-keys*))

;;;; :bind-keymap, :bind-keymap*

(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

(defun use-package-handler/:bind-keymap
    (name keyword arg rest state &optional override)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `((ignore
      ,@(mapcar
         #'(lambda (binding)
             `(,(if override
                    'bind-key*
                  'bind-key)
               ,(car binding)
               #'(lambda ()
                   (interactive)
                   (use-package-autoload-keymap
                    ',(cdr binding) ',(use-package-as-symbol name)
                    ,override)))) arg)))))

(defun use-package-handler/:bind-keymap* (name keyword arg rest state)
  (use-package-handler/:bind-keymap name keyword arg rest state t))

;;;; :interpreter

(defun use-package-handle-mode (name alist args rest state)
  "Handle keywords which add regexp/mode pairs to an alist."
  (cl-destructuring-bind (nargs . commands)
      (use-package-normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands commands))
       state)
     `((ignore
        ,@(mapcar
           #'(lambda (thing)
               `(add-to-list
                 ',alist
                 ',(cons (use-package-normalize-regex (car thing))
                         (cdr thing))))
           nargs))))))

(defalias 'use-package-normalize/:interpreter 'use-package-normalize-mode)

(defun use-package-handler/:interpreter (name keyword arg rest state)
  (use-package-handle-mode name 'interpreter-mode-alist arg rest state))

;;;; :mode

(defalias 'use-package-normalize/:mode 'use-package-normalize-mode)

(defun use-package-handler/:mode (name keyword arg rest state)
  (use-package-handle-mode name 'auto-mode-alist arg rest state))

;;;; :magic

(defalias 'use-package-normalize/:magic 'use-package-normalize-mode)

(defun use-package-handler/:magic (name keyword arg rest state)
  (use-package-handle-mode name 'magic-mode-alist arg rest state))

;;;; :magic-fallback

(defalias 'use-package-normalize/:magic-fallback 'use-package-normalize-mode)

(defun use-package-handler/:magic-fallback (name keyword arg rest state)
  (use-package-handle-mode name 'magic-fallback-mode-alist arg rest state))

;;;; :hook

(defun use-package-normalize/:hook (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (or (use-package-non-nil-symbolp arg) (consp arg))
          (use-package-error
           (concat label " a <symbol> or (<symbol or list of symbols> . <symbol or function>)"
                   " or list of these")))
        (use-package-normalize-pairs
         #'(lambda (k)
             (or (use-package-non-nil-symbolp k)
                 (and k (let ((every t))
                          (while (and every k)
                            (if (and (consp k)
                                     (use-package-non-nil-symbolp (car k)))
                                (setq k (cdr k))
                              (setq every nil)))
                          every))))
         #'use-package-recognize-function
         name label arg))))

(defun use-package-handler/:hook (name keyword args rest state)
  "Generate use-package custom keyword code."
  (cl-destructuring-bind (nargs . commands)
      (use-package-normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands commands))
       state)
     `((ignore
        ,@(cl-mapcan
           #'(lambda (def)
               (let ((syms (car def))
                     (fun (cdr def)))
                 (when fun
                   (mapcar
                    #'(lambda (sym)
                        `(add-hook (quote ,(intern (format "%s-hook" sym)))
                                   (function ,fun)))
                    (if (use-package-non-nil-symbolp syms) (list syms) syms)))))
           nargs))))))

;;;; :commands

(defalias 'use-package-normalize/:commands 'use-package-normalize-symlist)

(defun use-package-handler/:commands (name keyword arg rest state)
  (use-package-concat
   (unless (plist-get state :demand)
     ;; Since we deferring load, establish any necessary autoloads, and also
     ;; keep the byte-compiler happy.
     (let ((name-string (use-package-as-string name)))
       (cl-mapcan
        #'(lambda (command)
            (when (symbolp command)
              (append
               `((unless (fboundp ',command)
                   (autoload #',command ,name-string nil t)))
               (when (bound-and-true-p byte-compile-current-file)
                 `((eval-when-compile
                     (declare-function ,command ,name-string)))))))
        (delete-dups arg))))
   (use-package-process-keywords name rest state)))

;;;; :defer

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)

(defun use-package-handler/:defer (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     ;; Load the package after a set amount of idle time, if the argument to
     ;; `:defer' was a number.
     (when (numberp arg)
       `((run-with-idle-timer ,arg nil #'require
                              ',(use-package-as-symbol name) nil t)))
     (if (or (not arg) (null body))
         body
       `((eval-after-load ',name ',(macroexp-progn body)))))))

;;;; :after

(defun use-package-normalize/:after (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun use-package-after-count-uses (features)
  "Count the number of time the body would appear in the result."
  (pcase features
    ((and (pred use-package-non-nil-symbolp) feat)
     1)
    (`(,(or `:or `:any) . ,rest)
     (let ((num 0))
       (dolist (next rest)
         (setq num (+ num (use-package-after-count-uses next))))
       num))
    (`(,(or `:and `:all) . ,rest)
     (apply #'max (mapcar #'use-package-after-count-uses rest)))
    (`(,feat . ,rest)
     (use-package-after-count-uses (cons :all (cons feat rest))))))

(defun use-package-require-after-load (features body)
  "Generate `eval-after-load' statements to represents FEATURES.
FEATURES is a list containing keywords `:and' and `:all', where
no keyword implies `:all'."
  (pcase features
    ((and (pred use-package-non-nil-symbolp) feat)
     `(eval-after-load ',feat
        ,(if (member (car body) '(quote backquote \' \`))
             body
           (list 'quote body))))
    (`(,(or `:or `:any) . ,rest)
     (macroexp-progn
      (mapcar #'(lambda (x) (use-package-require-after-load x body)) rest)))
    (`(,(or `:and `:all) . ,rest)
     (dolist (next rest)
       (setq body (use-package-require-after-load next body)))
     body)
    (`(,feat . ,rest)
     (use-package-require-after-load (cons :all (cons feat rest)) body))))

(defun use-package-handler/:after (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (uses (use-package-after-count-uses arg)))
    (if (or (null uses) (null body))
        body
      (if (<= uses 1)
          (list (use-package-require-after-load
                 arg (list 'quote (macroexp-progn body))))
        (use-package-memoize
         (apply-partially #'use-package-require-after-load arg)
         (macroexp-progn body))))))

;;;; :demand

(defalias 'use-package-normalize/:demand 'use-package-normalize-predicate)

(defun use-package-handler/:demand (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;; :custom

(defun use-package-normalize/:custom (name keyword args)
  "Normalize use-package custom keyword."
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (listp arg)
          (use-package-error
           (concat label " a (<symbol> <value> [comment])"
                   " or list of these")))
        (if (use-package-non-nil-symbolp (car arg))
            (list arg)
          arg))))

(defun use-package-handler/:custom (name keyword args rest state)
  "Generate use-package custom keyword code."
  (use-package-concat
   (mapcar
    #'(lambda (def)
        (let ((variable (nth 0 def))
              (value (nth 1 def))
              (comment (nth 2 def)))
          (unless (and comment (stringp comment))
            (setq comment (format "Customized with use-package %s" name)))
          `(customize-set-variable (quote ,variable) ,value ,comment)))
    args)
   (use-package-process-keywords name rest state)))

;;;; :custom-face

(defun use-package-normalize/:custom-face (name-symbol keyword arg)
  "Normalize use-package custom-face keyword."
  (let ((error-msg
         (format "%s wants a (<symbol> <face-spec>) or list of these"
                 name-symbol)))
    (unless (listp arg)
      (use-package-error error-msg))
    (dolist (def arg arg)
      (unless (listp def)
        (use-package-error error-msg))
      (let ((face (nth 0 def))
            (spec (nth 1 def)))
        (when (or (not face)
                  (not spec)
                  (> (length arg) 2))
          (use-package-error error-msg))))))

(defun use-package-handler/:custom-face (name keyword args rest state)
  "Generate use-package custom-face keyword code."
  (use-package-concat
   (mapcar #'(lambda (def) `(custom-set-faces (quote ,def))) args)
   (use-package-process-keywords name rest state)))

;;;; :init

(defalias 'use-package-normalize/:init 'use-package-normalize-forms)

(defun use-package-handler/:init (name keyword arg rest state)
  (use-package-concat
   (let ((init-body
          (use-package-hook-injector (use-package-as-string name)
                                     :init arg)))
     (when init-body
       (funcall use-package--hush-function
                (if use-package-check-before-init
                    `((when (locate-library ,(use-package-as-string name))
                        ,@init-body))
                  init-body))))
   (use-package-process-keywords name rest state)))

;;;; :load

(defun use-package-normalize/:load (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun use-package-handler/:load (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (dolist (pkg arg)
      (setq body (use-package-require pkg nil body)))
    body))

;;;; :config

(defalias 'use-package-normalize/:config 'use-package-normalize-forms)

(defun use-package-handler/:config (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name)))
    (if (or (null arg) (equal arg '(t)))
        body
      (use-package-with-elapsed-timer
          (format "Configuring package %s" name-symbol)
        (funcall use-package--hush-function
                 (use-package-concat
                  (use-package-hook-injector
                   (symbol-name name-symbol) :config arg)
                  body
                  (list t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The main macro
;;

(defsubst use-package-hush (context body)
  `((condition-case-unless-debug err
        ,(macroexp-progn body)
      (error (funcall ,context err)))))

(defun use-package-core (name args)
  (let* ((context (gensym "use-package--warning"))
         (args* (use-package-normalize-keywords name args))
         (use-package--hush-function #'identity))
    (if use-package-expand-minimally
        (use-package-process-keywords name args*
          (and (plist-get args* :demand)
               (list :demand t)))
      `((let
            ((,context
              #'(lambda (err)
                  (let ((msg (format "%s: %s" ',name (error-message-string err))))
                    ,(when (eq use-package-verbose 'debug)
                       `(progn
                          (with-current-buffer (get-buffer-create "*use-package*")
                            (goto-char (point-max))
                            (insert
                             "-----\n" msg
                             ,(concat
                               "\n\n"
                               (pp-to-string `(use-package ,name ,@args))
                               "\n  -->\n\n"
                               (pp-to-string `(use-package ,name ,@args*))
                               "\n  ==>\n\n"
                               (pp-to-string
                                (macroexp-progn
                                 (let ((use-package-verbose 'errors)
                                       (use-package-expand-minimally t))
                                   (use-package-process-keywords name args*
                                     (and (plist-get args* :demand)
                                          (list :demand t))))))))
                            (emacs-lisp-mode))
                          (setq msg (concat msg " (see the *use-package* buffer)"))))
                    (ignore (display-warning 'use-package msg :error))))))
          ,(let ((use-package--hush-function
                  (apply-partially #'use-package-hush context)))
             (macroexp-progn
              (funcall use-package--hush-function
                       (use-package-process-keywords name args*
                         (and (plist-get args* :demand)
                              (list :demand t)))))))))))

;;;###autoload
(defmacro use-package (name &rest args)
  "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Defer loading of a package until after any of the named
                 features are loaded.
:demand          Prevent deferred loading in all cases.

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `customize-set-variable' with each variable definition.
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive."
  (declare (indent 1))
  (unless (memq :disabled args)
    (macroexp-progn
     (if (eq use-package-verbose 'errors)
         (use-package-core name args)
       (condition-case-unless-debug err
           (use-package-core name args)
         (error
          (ignore
           (display-warning
            'use-package
            (format "Failed to parse package %s: %s"
                    name (error-message-string err)) :error))))))))

(put 'use-package 'lisp-indent-function 'defun)

(provide 'up-core)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; use-package.el ends here
