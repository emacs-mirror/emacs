;;; use-package-core.el --- A configuration macro for simplifying your .emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 29 Nov 2017
;; Version: 2.4
;; Package-Requires: ((emacs "24.3"))
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

(require 'bytecomp)
(require 'cl-lib)
(require 'tabulated-list)

(if (and (eq emacs-major-version 24) (eq emacs-minor-version 3))
    (defsubst hash-table-keys (hash-table)
      "Return a list of keys in HASH-TABLE."
      (cl-loop for k being the hash-keys of hash-table collect k))
  (eval-when-compile (require 'subr-x)))

(eval-when-compile
  (require 'regexp-opt))

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defconst use-package-version "2.4"
  "This version of use-package.")

(defcustom use-package-keywords
  '(:disabled
    :load-path
    :requires
    :defines
    :functions
    :preface
    :if :when :unless
    :no-require
    :catch
    :after
    :custom
    :custom-face
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
    :init
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
  '(:bind-keymap
    :bind-keymap*
    :commands)
  "Unless `:demand' is used, keywords in this list imply deferred loading.
The reason keywords like `:hook' are not in this list is that
they only imply deferred loading if they reference actual
function symbols that can be autoloaded from the module; whereas
the default keywords provided here always defer loading unless
otherwise requested."
  :type '(repeat symbol)
  :group 'use-package)

(defcustom use-package-ignore-unknown-keywords nil
  "If non-nil, issue warning instead of error when unknown
keyword is encountered. The unknown keyword and its associated
arguments will be ignored in the `use-package' expansion."
  :type 'boolean
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
    (:catch t (lambda (name args)
                (not use-package-expand-minimally)))
    (:defer use-package-always-defer
            (lambda (name args)
              (and use-package-always-defer
                   (not (plist-member args :defer))
                   (not (plist-member args :demand)))))
    (:demand use-package-always-demand
             (lambda (name args)
               (and use-package-always-demand
                    (not (plist-member args :defer))
                    (not (plist-member args :demand))))))
  "Default values for specified `use-package' keywords.
Each entry in the alist is a list of three elements:
The first element is the `use-package' keyword.

The second is a form that can be evaluated to get the default
value. It can also be a function that will receive the name of
the use-package declaration and the keyword plist given to
`use-package', in normalized form. The value it returns should
also be in normalized form (which is sometimes *not* what one
would normally write in a `use-package' declaration, so use
caution).

The third element is a form that can be evaluated to determine
whether or not to assign a default value; if it evaluates to nil,
then the default value is not assigned even if the keyword is not
present in the `use-package' form. This third element may also be
a function, in which case it receives the name of the package (as
a symbol) and a list of keywords (in normalized form). It should
return nil or non-nil depending on whether defaulting should be
attempted."
  :type `(repeat
          (list (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  use-package-keywords))
                (choice :tag "Default value" sexp function)
                (choice :tag "Enable if non-nil" sexp function)))
  :group 'use-package)

(defcustom use-package-merge-key-alist
  '((:if    . (lambda (new old) `(and ,new ,old)))
    (:after . (lambda (new old) `(:all ,new ,old)))
    (:defer . (lambda (new old) old))
    (:bind  . (lambda (new old) (append new (list :break) old))))
  "Alist of keys and the functions used to merge multiple values.
For example, if the following form is provided:

  (use-package foo :if pred1 :if pred2)

Then based on the above defaults, the merged result will be:

  (use-package foo :if (and pred1 pred2))

This is done so that, at the stage of invoking handlers, each
handler is called only once."
  :type `(repeat
          (cons (choice :tag "Keyword"
                        ,@(mapcar #'(lambda (k) (list 'const k))
                                  use-package-keywords)
                        (const :tag "Any" t))
                function))
  :group 'use-package)

(defcustom use-package-hook-name-suffix "-hook"
  "Text append to the name of hooks mentioned by :hook.
Set to nil if you don't want this to happen; it's only a
convenience."
  :type '(choice string (const :tag "No suffix" nil))
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
return t if you only wish to add behavior to what the user had
specified."
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
  "If non-nil, cause imenu to see `use-package' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to
include support for finding `use-package' and `require' forms.

Must be set before loading use-package."
  :type 'boolean
  :set
  #'(lambda (_sym value)
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

(defcustom use-package-compute-statistics nil
  "If non-nil, compute statistics concerned use-package declarations.
View the statistical report using `use-package-report'. Note that
if this option is enabled, you must require `use-package' in your
user init file at loadup time, or you will see errors concerning
undefined variables."
  :type 'boolean
  :group 'use-package)

(defvar use-package-statistics (make-hash-table))

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
  "Given some regexp-like thing in RE, resolve to a regular expression."
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
  "Return a form which will load or require NAME.
It does the right thing no matter if NAME is a string or symbol.
Argument NOERROR means to indicate load failures as a warning."
  (if (stringp name)
      `(load ,name ,noerror)
    `(require ',name nil ,noerror)))

(defun use-package-hook-injector (name-string keyword body)
  "Wrap pre/post hook injections around the given BODY for KEYWORD.
The BODY is a list of forms, so `((foo))' if only `foo' is being called."
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
              (display-warning 'use-package
                               (format "Cannot load %s" ',name)
                               :error)
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
    (cl-dolist (x xs)
      (if flip
          (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

(defun use-package-split-list-at-keys (key lst)
  (and lst
       (let ((xs (use-package-split-list (apply-partially #'eq key) lst)))
         (cons (car xs) (use-package-split-list-at-keys key (cddr xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Keywords
;;

(defun use-package-keyword-index (keyword)
  (cl-loop named outer
           with index = 0
           for k in use-package-keywords do
           (if (eq k keyword)
               (cl-return-from outer index))
           (cl-incf index)))

(defun use-package-normalize-plist (name input &optional plist merge-function)
  "Given a pseudo-plist, normalize it to a regular plist.
The normalized key/value pairs from input are added to PLIST,
extending any keys already present."
  (if (null input)
      plist
    (let* ((keyword (car input))
           (xs (use-package-split-list #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer
            (intern-soft (concat "use-package-normalize/"
                                 (symbol-name keyword))))
           (arg (and (functionp normalizer)
                     (funcall normalizer name keyword args)))
           (error-string (format "Unrecognized keyword: %s" keyword)))
      (if (memq keyword use-package-keywords)
          (progn
            (setq plist (use-package-normalize-plist
                         name tail plist merge-function))
            (plist-put plist keyword
                       (if (plist-member plist keyword)
                           (funcall merge-function keyword arg
                                    (plist-get plist keyword))
                         arg)))
        (if use-package-ignore-unknown-keywords
            (progn
              (display-warning 'use-package error-string)
              (use-package-normalize-plist
               name tail plist merge-function))
          (use-package-error error-string))))))

(defun use-package-unalias-keywords (_name args)
  (setq args (cl-nsubstitute :if :when args))
  (let (temp)
    (while (setq temp (plist-get args :unless))
      (setq args (use-package-plist-delete-first args :unless)
            args (append args `(:if (not ,temp))))))
  args)

(defun use-package-merge-keys (key new old)
  (let ((merger (assq key use-package-merge-key-alist)))
    (if merger
        (funcall (cdr merger) new old)
      (append new old))))

(defun use-package-sort-keywords (plist)
  (let (plist-grouped)
    (while plist
      (push (cons (car plist) (cadr plist))
            plist-grouped)
      (setq plist (cddr plist)))
    (let (result)
      (cl-dolist
          (x
           (nreverse
            (sort plist-grouped
                  #'(lambda (l r) (< (use-package-keyword-index (car l))
                                (use-package-keyword-index (car r)))))))
        (setq result (cons (car x) (cons (cdr x) result))))
      result)))

(defun use-package-normalize-keywords (name args)
  (let* ((name-symbol (if (stringp name) (intern name) name))
         (name-string (symbol-name name-symbol)))

    ;; The function `elisp--local-variables' inserts this unbound variable into
    ;; macro forms to determine the locally bound variables for
    ;; `elisp-completion-at-point'. It ends up throwing a lot of errors since it
    ;; can occupy the position of a keyword (or look like a second argument to a
    ;; keyword that takes one). Deleting it when it's at the top level should be
    ;; harmless since there should be no locally bound variables to discover
    ;; here anyway.
    (setq args (delq 'elisp--witness--lisp args))

    ;; Reduce the set of keywords down to its most fundamental expression.
    (setq args (use-package-unalias-keywords name-symbol args))

    ;; Normalize keyword values, coalescing multiple occurrences.
    (setq args (use-package-normalize-plist name-symbol args nil
                                            #'use-package-merge-keys))

    ;; Add default values for keywords not specified, when applicable.
    (cl-dolist (spec use-package-defaults)
      (when (let ((func (nth 2 spec)))
              (if (and func (functionp func))
                  (funcall func name args)
                (eval func)))
        (setq args (use-package-plist-maybe-put
                    args (nth 0 spec)
                    (let ((func (nth 1 spec)))
                      (if (and func (functionp func))
                          (funcall func name args)
                        (eval func)))))))

    ;; Determine any autoloads implied by the keywords used.
    (let ((iargs args)
          commands)
      (while iargs
        (when (keywordp (car iargs))
          (let ((autoloads
                 (intern-soft (concat "use-package-autoloads/"
                                      (symbol-name (car iargs))))))
            (when (functionp autoloads)
              (setq commands
                    ;; jww (2017-12-07): Right now we just ignored the type of
                    ;; the autoload being requested, and assume they are all
                    ;; `command'.
                    (append (mapcar
                             #'car
                             (funcall autoloads name-symbol (car iargs)
                                      (cadr iargs)))
                            commands)))))
        (setq iargs (cddr iargs)))
      (when commands
        (setq args
              ;; Like `use-package-plist-append', but removing duplicates.
              (plist-put args :commands
                         (delete-dups
                          (append commands (plist-get args :commands)))))))

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
                       `(unless (featurep ',name-symbol)
                          (load ,name-string nil t))))))))))

    ;; Certain keywords imply :defer, if :demand was not specified.
    (when (and (not (plist-member args :demand))
               (not (plist-member args :defer))
               (not (or (equal '(t) (plist-get args :load))
                        (equal (list (use-package-as-string name))
                               (mapcar #'use-package-as-string
                                       (plist-get args :load)))))
               (cl-some #'identity
                        (mapcar (apply-partially #'plist-member args)
                                use-package-deferring-keywords)))
      (setq args (append args '(:defer t))))

    ;; The :load keyword overrides :no-require
    (when (and (plist-member args :load)
               (plist-member args :no-require))
      (setq args (use-package-plist-delete args :no-require)))

    ;; If at this point no :load, :defer or :no-require has been seen, then
    ;; :load the package itself.
    (when (and (not (plist-member args :load))
               (not (plist-member args :defer))
               (not (plist-member args :no-require)))
      (setq args (append args `(:load (,name)))))

    ;; Sort the list of keywords based on the order of `use-package-keywords'.
    (use-package-sort-keywords args)))

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

(defun use-package-list-insert (elem xs &optional anchor after test)
  "Insert ELEM into the list XS.
If ANCHOR is also a keyword, place the new KEYWORD before that
one.
If AFTER is non-nil, insert KEYWORD either at the end of the
keywords list, or after the ANCHOR if one has been provided.
If TEST is non-nil, it is the test used to compare ELEM to list
elements. The default is `eq'.
The modified list is returned. The original list is not modified."
  (let (result)
    (dolist (k xs)
      (if (funcall (or test #'eq) k anchor)
          (if after
              (setq result (cons k result)
                    result (cons elem result))
            (setq result (cons elem result)
                  result (cons k result)))
        (setq result (cons k result))))
    (if anchor
        (nreverse result)
      (if after
          (nreverse (cons elem result))
        (cons elem (nreverse result))))))

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
  (let ((loaded (cl-gentemp "use-package--loaded"))
        (result (cl-gentemp "use-package--result"))
        (next   (cl-gentemp "use-package--next")))
    `((defvar ,loaded nil)
      (defvar ,result nil)
      (defvar ,next #'(lambda () (if ,loaded ,result
                              (setq ,loaded t ,result ,arg))))
      ,@(funcall f `((funcall ,next))))))

(defsubst use-package-normalize-value (_label arg)
  "Normalize the Lisp value given by ARG.
The argument LABEL is ignored."
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

(defun use-package-normalize-symlist (_name keyword args)
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

(defun use-package-normalize-recursive-symlist (_name keyword args)
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

(defun use-package-normalize-predicate (_name keyword args)
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
                       (memq (car form)
                             '(use-package bind-key bind-key*
                                unbind-key bind-keys bind-keys*)))
                  (macroexpand form)
                form)) args))

(defun use-package-normalize-forms (_name keyword args)
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
  (or (if binding
          (symbolp v)
        (use-package-non-nil-symbolp v))
      (and (listp v)
           (memq (car v) '(quote function))
           (use-package-non-nil-symbolp (cadr v)))
      (if binding (commandp v) (functionp v))
      (and additional-pred
           (funcall additional-pred v))))

(defun use-package-normalize-function (v)
  "Reduce functional constructions to one of two normal forms:
  sym
  #'(lambda () ...)"
  (cond ((symbolp v) v)
        ((and (listp v)
              (memq (car v) '(quote function))
              (use-package-non-nil-symbolp (cadr v)))
         (cadr v))
        ((and (consp v)
              (eq 'lambda (car v)))
         v)
        ((and (listp v)
              (memq (car v) '(quote function))
              (eq 'lambda (car (cadr v))))
         (cadr v))
        (t v)))

(defun use-package-normalize-commands (args)
  "Map over ARGS of the form ((_ . F) ...), normalizing functional F's."
  (mapcar #'(lambda (x)
              (if (consp x)
                  (cons (car x) (use-package-normalize-function (cdr x)))
                x))
          args))

(defun use-package-normalize-mode (name keyword args)
  "Normalize arguments for keywords which add regexp/mode pairs to an alist."
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-pairs
                     #'use-package-regex-p
                     #'use-package-recognize-function
                     name)))

(defun use-package-autoloads-mode (_name _keyword args)
  (mapcar
   #'(lambda (x) (cons (cdr x) 'command))
   (cl-remove-if-not #'(lambda (x)
                         (and (consp x)
                              (use-package-non-nil-symbolp (cdr x))))
                     args)))

(defun use-package-handle-mode (name alist args rest state)
  "Handle keywords which add regexp/mode pairs to an alist."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    #'(lambda (thing)
        `(add-to-list
          ',alist
          ',(cons (use-package-normalize-regex (car thing))
                  (cdr thing))))
    (use-package-normalize-commands args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Statistics
;;

(defun use-package-reset-statistics ()
  (interactive)
  (setq use-package-statistics (make-hash-table)))

(defun use-package-statistics-status (package)
  "Return loading configuration status of PACKAGE statistics."
  (cond ((gethash :config package)      "Configured")
        ((gethash :init package)        "Initialized")
        ((gethash :preface package)     "Prefaced")
        ((gethash :use-package package) "Declared")))

(defun use-package-statistics-last-event (package)
  "Return the date when PACKAGE's status last changed.
The date is returned as a string."
  (format-time-string "%Y-%m-%d %a %H:%M"
                      (or (gethash :config package)
                          (gethash :init package)
                          (gethash :preface package)
                          (gethash :use-package package))))

(defun use-package-statistics-time (package)
  "Return the time is took for PACKAGE to load."
  (+ (float-time (gethash :config-secs package '(0 0 0 0)))
     (float-time (gethash :init-secs package '(0 0 0 0)))
     (float-time (gethash :preface-secs package '(0 0 0 0)))
     (float-time (gethash :use-package-secs package '(0 0 0 0)))))

(defun use-package-statistics-convert (package)
  "Return information about PACKAGE.

The information is formatted in a way suitable for
`use-package-statistics-mode'."
  (let ((statistics (gethash package use-package-statistics)))
    (list
     package
     (vector
      (symbol-name package)
      (use-package-statistics-status statistics)
      (use-package-statistics-last-event statistics)
      (format "%.2f" (use-package-statistics-time statistics))))))

(defun use-package-report ()
  "Show current statistics gathered about use-package declarations.
In the table that's generated, the status field has the following
meaning:
  Configured        :config has been processed (the package is loaded!)
  Initialized       :init has been processed (load status unknown)
  Prefaced          :preface has been processed
  Declared          the use-package declaration was seen"
  (interactive)
  (with-current-buffer (get-buffer-create "*use-package statistics*")
    (setq tabulated-list-entries
          (mapcar #'use-package-statistics-convert
                  (hash-table-keys use-package-statistics)))
    (use-package-statistics-mode)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(define-derived-mode use-package-statistics-mode tabulated-list-mode
  "use-package statistics"
  "Show current statistics gathered about use-package declarations."
  (setq tabulated-list-format
        ;; The sum of column width is 80 characters:
        #[("Package" 25 t)
          ("Status" 13 t)
          ("Last Event" 23 t)
          ("Time" 10 t)])
  (tabulated-list-init-header))

(defun use-package-statistics-gather (keyword name after)
  (let* ((hash (gethash name use-package-statistics
                        (make-hash-table)))
         (before (and after (gethash keyword hash (current-time)))))
    (puthash keyword (current-time) hash)
    (when after
      (puthash (intern (concat (symbol-name keyword) "-secs"))
               (time-subtract (current-time) before) hash))
    (puthash name hash use-package-statistics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Handlers
;;

;;;; :disabled

;; Don't alias this to `ignore', as that will cause the resulting
;; function to be interactive.
(defun use-package-normalize/:disabled (_name _keyword _arg)
  "Do nothing, return nil.")

(defun use-package-handler/:disabled (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :if, :when and :unless

(defun use-package-normalize-test (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defalias 'use-package-normalize/:if 'use-package-normalize-test)

(defun use-package-handler/:if (name _keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((when ,pred ,@body))))

(defalias 'use-package-normalize/:when 'use-package-normalize-test)

(defalias 'use-package-handler/:when 'use-package-handler/:if)

(defalias 'use-package-normalize/:unless 'use-package-normalize-test)

(defun use-package-handler/:unless (name _keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((unless ,pred ,@body))))

;;;; :requires

(defalias 'use-package-normalize/:requires 'use-package-normalize-symlist)

(defun use-package-handler/:requires (name _keyword requires rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if (null requires)
        body
      `((when ,(if (> (length requires) 1)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',(car requires)))
          ,@body)))))

;;;; :load-path

(defun use-package-normalize/:load-path (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun use-package-handler/:load-path (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (path)
                 `(eval-and-compile (add-to-list 'load-path ,path)))
             arg)
     body)))

;;;; :no-require

(defalias 'use-package-normalize/:no-require 'use-package-normalize-predicate)

(defun use-package-handler/:no-require (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :defines

(defalias 'use-package-normalize/:defines 'use-package-normalize-symlist)

(defun use-package-handler/:defines (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :functions

(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)

(defun use-package-handler/:functions (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :preface

(defalias 'use-package-normalize/:preface 'use-package-normalize-forms)

(defun use-package-handler/:preface (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (when use-package-compute-statistics
       `((use-package-statistics-gather :preface ',name nil)))
     (when arg
       `((eval-and-compile ,@arg)))
     body
     (when use-package-compute-statistics
       `((use-package-statistics-gather :preface ',name t))))))

;;;; :catch

(defvar use-package--form)
(defvar use-package--hush-function #'(lambda (_keyword body) body))

(defsubst use-package-hush (context keyword body)
  `((condition-case-unless-debug err
        ,(macroexp-progn body)
      (error (funcall ,context ,keyword err)))))

(defun use-package-normalize/:catch (_name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      use-package--hush-function)))

(defun use-package-handler/:catch (name keyword arg rest state)
  (let* ((context (cl-gentemp "use-package--warning")))
    (cond
     ((not arg)
      (use-package-process-keywords name rest state))
     ((eq arg t)
      `((defvar ,context
          #'(lambda (keyword err)
              (let ((msg (format "%s/%s: %s" ',name keyword
                                 (error-message-string err))))
                ,@(when (eq use-package-verbose 'debug)
                    `((with-current-buffer
                          (get-buffer-create "*use-package*")
                        (goto-char (point-max))
                        (insert "-----\n" msg ,use-package--form)
                        (emacs-lisp-mode))
                      (setq msg
                            (concat msg
                                    " (see the *use-package* buffer)"))))
                (display-warning 'use-package msg :error))))
        ,@(let ((use-package--hush-function
                 (apply-partially #'use-package-hush context)))
            (funcall use-package--hush-function keyword
                     (use-package-process-keywords name rest state)))))
     ((functionp arg)
      `((defvar ,context ,arg)
        ,@(let ((use-package--hush-function
                 (apply-partially #'use-package-hush context)))
            (funcall use-package--hush-function keyword
                     (use-package-process-keywords name rest state)))))
     (t
      (use-package-error "The :catch keyword expects 't' or a function")))))

;;;; :interpreter

(defalias 'use-package-normalize/:interpreter 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:interpreter 'use-package-autoloads-mode)

(defun use-package-handler/:interpreter (name _keyword arg rest state)
  (use-package-handle-mode name 'interpreter-mode-alist arg rest state))

;;;; :mode

(defalias 'use-package-normalize/:mode 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:mode 'use-package-autoloads-mode)

(defun use-package-handler/:mode (name _keyword arg rest state)
  (use-package-handle-mode name 'auto-mode-alist arg rest state))

;;;; :magic

(defalias 'use-package-normalize/:magic 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:magic 'use-package-autoloads-mode)

(defun use-package-handler/:magic (name _keyword arg rest state)
  (use-package-handle-mode name 'magic-mode-alist arg rest state))

;;;; :magic-fallback

(defalias 'use-package-normalize/:magic-fallback 'use-package-normalize-mode)
(defalias 'use-package-autoloads/:magic-fallback 'use-package-autoloads-mode)

(defun use-package-handler/:magic-fallback (name _keyword arg rest state)
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

(defalias 'use-package-autoloads/:hook 'use-package-autoloads-mode)

(defun use-package-handler/:hook (name _keyword args rest state)
  "Generate use-package custom keyword code."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (cl-mapcan
    #'(lambda (def)
        (let ((syms (car def))
              (fun (cdr def)))
          (when fun
            (mapcar
             #'(lambda (sym)
                 `(add-hook
                   (quote ,(intern
                            (concat (symbol-name sym)
                                    use-package-hook-name-suffix)))
                   (function ,fun)))
             (if (use-package-non-nil-symbolp syms) (list syms) syms)))))
    (use-package-normalize-commands args))))

;;;; :commands

(defalias 'use-package-normalize/:commands 'use-package-normalize-symlist)

(defun use-package-handler/:commands (name _keyword arg rest state)
  (use-package-concat
   ;; Since we deferring load, establish any necessary autoloads, and also
   ;; keep the byte-compiler happy.
   (let ((name-string (use-package-as-string name)))
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             (unless (plist-get state :demand)
               `((unless (fboundp ',command)
                   (autoload #',command ,name-string nil t))))
             (when (bound-and-true-p byte-compile-current-file)
               `((eval-when-compile
                   (declare-function ,command ,name-string)))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))

;;;; :defer

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)

(defun use-package-handler/:defer (name _keyword arg rest state)
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

(defun use-package-after-count-uses (features*)
  "Count the number of time the body would appear in the result."
  (cond ((use-package-non-nil-symbolp features*)
         1)
        ((and (consp features*)
              (memq (car features*) '(:or :any)))
         (let ((num 0))
           (cl-dolist (next (cdr features*))
             (setq num (+ num (use-package-after-count-uses next))))
           num))
        ((and (consp features*)
              (memq (car features*) '(:and :all)))
         (apply #'max (mapcar #'use-package-after-count-uses
                              (cdr features*))))
        ((listp features*)
         (use-package-after-count-uses (cons :all features*)))))

(defun use-package-require-after-load (features* body)
  "Generate `eval-after-load' statements to represents FEATURES*.
FEATURES* is a list containing keywords `:and' and `:all', where
no keyword implies `:all'."
  (cond
   ((use-package-non-nil-symbolp features*)
    `((eval-after-load ',features* ',(macroexp-progn body))))
   ((and (consp features*)
         (memq (car features*) '(:or :any)))
    (cl-mapcan #'(lambda (x) (use-package-require-after-load x body))
               (cdr features*)))
   ((and (consp features*)
         (memq (car features*) '(:and :all)))
    (cl-dolist (next (cdr features*))
      (setq body (use-package-require-after-load next body)))
    body)
   ((listp features*)
    (use-package-require-after-load (cons :all features*) body))))

(defun use-package-handler/:after (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (uses (use-package-after-count-uses arg)))
    (if (or (null uses) (null body))
        body
      (if (<= uses 1)
          (use-package-require-after-load arg body)
        (use-package-memoize
         (apply-partially #'use-package-require-after-load arg)
         (macroexp-progn body))))))

;;;; :demand

(defalias 'use-package-normalize/:demand 'use-package-normalize-predicate)

(defun use-package-handler/:demand (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

;;;; :custom

(defun use-package-normalize/:custom (_name keyword args)
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

(defun use-package-handler/:custom (name _keyword args rest state)
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

(defun use-package-normalize/:custom-face (name-symbol _keyword arg)
  "Normalize use-package custom-face keyword."
  (let ((error-msg
         (format "%s wants a (<symbol> <face-spec>) or list of these"
                 name-symbol)))
    (unless (listp arg)
      (use-package-error error-msg))
    (cl-dolist (def arg arg)
      (unless (listp def)
        (use-package-error error-msg))
      (let ((face (nth 0 def))
            (spec (nth 1 def)))
        (when (or (not face)
                  (not spec)
                  (> (length def) 2))
          (use-package-error error-msg))))))

(defun use-package-handler/:custom-face (name _keyword args rest state)
  "Generate use-package custom-face keyword code."
  (use-package-concat
   (mapcar #'(lambda (def) `(custom-set-faces (backquote ,def))) args)
   (use-package-process-keywords name rest state)))

;;;; :init

(defalias 'use-package-normalize/:init 'use-package-normalize-forms)

(defun use-package-handler/:init (name _keyword arg rest state)
  (use-package-concat
   (when use-package-compute-statistics
     `((use-package-statistics-gather :init ',name nil)))
   (let ((init-body
          (use-package-hook-injector (use-package-as-string name)
                                     :init arg)))
     (when init-body
       (funcall use-package--hush-function :init
                (if use-package-check-before-init
                    `((when (locate-library ,(use-package-as-string name))
                        ,@init-body))
                  init-body))))
   (use-package-process-keywords name rest state)
   (when use-package-compute-statistics
     `((use-package-statistics-gather :init ',name t)))))

;;;; :load

(defun use-package-normalize/:load (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args)
      args
    (list args)))

(defun use-package-handler/:load (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (cl-dolist (pkg arg)
      (setq body (use-package-require (if (eq t pkg) name pkg) nil body)))
    body))

;;;; :config

(defalias 'use-package-normalize/:config 'use-package-normalize-forms)

(defun use-package-handler/:config (name _keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name)))
    (use-package-concat
     (when use-package-compute-statistics
       `((use-package-statistics-gather :config ',name nil)))
     (if (or (null arg) (equal arg '(t)))
         body
       (use-package-with-elapsed-timer
           (format "Configuring package %s" name-symbol)
         (funcall use-package--hush-function :config
                  (use-package-concat
                   (use-package-hook-injector
                    (symbol-name name-symbol) :config arg)
                   body
                   (list t)))))
     (when use-package-compute-statistics
       `((use-package-statistics-gather :config ',name t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The main macro
;;

(defmacro use-package-core (name args)
  `(let* ((args* (use-package-normalize-keywords ,name ,args))
          (use-package--form
           (if (eq use-package-verbose 'debug)
               (concat "\n\n"
                       (pp-to-string `(use-package ,name ,@,args))
                       "\n  -->\n\n"
                       (pp-to-string `(use-package ,name ,@args*))
                       "\n  ==>\n\n"
                       (pp-to-string
                        (macroexp-progn
                         (let ((use-package-verbose 'errors)
                               (use-package-expand-minimally t))
                           (use-package-process-keywords name args*
                             (and (plist-get args* :demand)
                                  (list :demand t)))))))
             "")))
     (use-package-process-keywords name args*
       (and (plist-get args* :demand)
            (list :demand t)))))

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
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
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
     (use-package-concat
      (when use-package-compute-statistics
        `((use-package-statistics-gather :use-package ',name nil)))
      (if (eq use-package-verbose 'errors)
          (use-package-core name args)
        (condition-case-unless-debug err
            (use-package-core name args)
          (error
           (ignore
            (display-warning
             'use-package
             (format "Failed to parse package %s: %s"
                     name (error-message-string err)) :error)))))
      (when use-package-compute-statistics
        `((use-package-statistics-gather :use-package ',name t)))))))

(put 'use-package 'lisp-indent-function 'defun)

(provide 'use-package-core)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; use-package-core.el ends here
