;;; use-package.el --- A use-package declaration for simplifying your .emacs

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

(declare-function package-installed-p "package")
(declare-function package-read-all-archive-contents "package" ())

(defconst use-package-version "2.4"
  "This version of use-package.")

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defcustom use-package-verbose nil
  "Whether to report about loading and configuration details.

If you customize this, then you should require the `use-package'
feature in files that use `use-package', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type '(choice (const :tag "Quiet" nil) (const :tag "Verbose" t)
                 (const :tag "Debug" debug))
  :group 'use-package)

(defcustom use-package-debug nil
  "Whether to display use-package expansions in a *use-package* buffer."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-check-before-init nil
  "If non-nil, check that package exists before executing its `:init' block.
The check is performed by looking for the module using `locate-library'."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-defer nil
  "If non-nil, assume `:defer t` unless `:demand t` is given."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-demand nil
  "If non-nil, assume `:demand t` unless `:defer t` is given."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-always-ensure nil
  "Treat every package as though it had specified `:ensure SEXP`."
  :type 'sexp
  :group 'use-package)

(defcustom use-package-always-pin nil
  "Treat every package as though it had specified `:pin SYM`."
  :type 'symbol
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

This way, you can add to these hooks before evaluation of a
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
    :defines
    :functions
    :preface
    :no-require
    :bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :magic
    :magic-fallback
    :commands
    :hook
    :defer
    :custom
    :custom-face
    :init
    :after
    :demand
    :config
    :diminish
    :delight)
  "Establish which keywords are valid, and the order they are processed in.

Note that `:disabled' is special, in that it causes nothing at all to happen,
even if the rest of the use-package declaration is incorrect."
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

(defcustom use-package-enable-imenu-support nil
  "If non-nil, adjust `lisp-imenu-generic-expression' to include
support for finding `use-package' and `require' forms.

Must be set before loading use-package."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-ensure-function 'use-package-ensure-elpa
  "Function that ensures a package is installed.
This function is called with four arguments: the name of the
package declared in the `use-package' form; the argument passed
to `:ensure'; and the current `state' plist created by previous
handlers.

Note that this function is called whenever `:ensure' is provided,
even if it is nil. It is up to the function to decide on the
semantics of the various values for `:ensure'.

This function should return non-nil if the package is installed.

The default value uses package.el to install the package."
  :type '(choice (const :tag "package.el" use-package-ensure-elpa)
                 (function :tag "Custom"))
  :group 'use-package)

(defcustom use-package-defaults
  '((:config '(t) t)
    (:ensure use-package-always-ensure use-package-always-ensure)
    (:pin use-package-always-pin use-package-always-pin))
  "Alist of default values for `use-package' keywords.
Each entry in the alist is a list of three elements. The first
element is the `use-package' keyword and the second is a form
that can be evaluated to get the default value. The third element
is a form that can be evaluated to determine whether or not to
assign a default value; if it evaluates to nil, then the default
value is not assigned even if the keyword is not present in the
`use-package' form."
  :type '(repeat (list symbol sexp sexp)))

(when use-package-enable-imenu-support
  (eval-after-load 'lisp-mode
    `(let ((sym-regexp (or (bound-and-true-p lisp-mode-symbol-regexp)
                           "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")))
       (add-to-list
        'lisp-imenu-generic-expression
        (list "Packages"
              (concat "^\\s-*("
                      ,(eval-when-compile
                         (regexp-opt '("use-package" "require") t))
                      "\\s-+\\(" sym-regexp "\\)")
              2)))))

(defvar use-package-form-regexp "^\\s-*(\\s-*use-package\\s-+\\_<%s\\_>"
  "Regexp used in `use-package-jump-to-package-form' to find use
package forms in user files.")

(defun use-package--find-require (package)
  "Find file that required PACKAGE by searching
`load-history'. Returns an absolute file path or nil if none is
found."
  (catch 'suspect
    (dolist (filespec load-history)
      (dolist (entry (cdr filespec))
        (when (equal entry (cons 'require package))
          (throw 'suspect (car filespec)))))))

(defun use-package-jump-to-package-form (package)
  "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead."
  (interactive (list (completing-read "Package: " features)))
  (let* ((package (if (stringp package) (intern package) package))
         (requiring-file (use-package--find-require package))
         file location)
    (if (null requiring-file)
        (user-error "Can't find file that requires this feature.")
      (setq file (if (string= (file-name-extension requiring-file) "elc")
                     (concat (file-name-sans-extension requiring-file) ".el")
                   requiring-file))
      (when (file-exists-p file)
        (find-file-other-window file)
        (save-excursion
          (goto-char (point-min))
          (setq location
                (re-search-forward
                 (format use-package-form-regexp package) nil t)))
        (if (null location)
            (message "No use-package form found.")
          (goto-char location)
          (beginning-of-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utility functions
;;

(defun use-package-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defun use-package-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defun use-package-as-mode (string-or-symbol)
  "If STRING-OR-SYMBOL ends in `-mode' (or its name does), return
it as a symbol.  Otherwise, return it as a symbol with `-mode'
appended."
  (let ((string (use-package-as-string string-or-symbol)))
    (intern (if (string-match "-mode\\'" string) string
              (concat string "-mode")))))

(defun use-package-load-name (name &optional noerror)
  "Return a form which will load or require NAME depending on
whether it's a string or symbol."
  (if (stringp name)
      `(load ,name ',noerror)
    `(require ',name nil ',noerror)))

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
                               (format "%s %s: %s"
                                       ,name ,label (error-message-string ,err))
                               :error)))))))))

(put 'use-package-expand 'lisp-indent-function 'defun)

(defun use-package-hook-injector (name-string keyword body)
  "Wrap pre/post hook injections around a given keyword form.
ARGS is a list of forms, so `((foo))' if only `foo' is being called."
  (if (not use-package-inject-hooks)
      (use-package-expand name-string (format "%s" keyword) body)
    (let ((keyword-name (substring (format "%s" keyword) 1)))
      `((when ,(macroexp-progn
                (use-package-expand name-string (format "pre-%s hook" keyword)
                  `((run-hook-with-args-until-failure
                     ',(intern (concat "use-package--" name-string
                                       "--pre-" keyword-name "-hook"))))))
          ,(macroexp-progn
            (use-package-expand name-string (format "%s" keyword) body))
          ,(macroexp-progn
            (use-package-expand name-string (format "post-%s hook" keyword)
              `((run-hooks
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
    (let (result)
      (dolist (x
               (nreverse
                (sort plist-grouped
                      #'(lambda (l r) (< (use-package-keyword-index (car l))
                                    (use-package-keyword-index (car r)))))))
        (setq result (cons (car x) (cons (cdr x) result))))
      result)))

(defsubst use-package-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (apply #'nconc (delete nil (delete (list nil) elems))))

(defsubst use-package--non-nil-symbolp (sym)
  (and sym (symbolp sym)))

(defconst use-package-font-lock-keywords
  '(("(\\(use-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Keyword processing
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Normalization functions
;;

(defsubst use-package-regex-p (re)
  "Return t if RE is some regexp-like thing."
  (or (and (listp re)
           (eq (car re) 'rx))
      (stringp re)))

(defun use-package-normalize-regex (re)
  "Given some regexp-like thing, resolve it down to a regular expression."
  (cond
   ((and (listp re)
         (eq (car re) 'rx))
    (eval re))
   ((stringp re)
    re)
   (t
    (error "Not recognized as regular expression: %s" re))))

(defun use-package-normalize-plist (name input)
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
             ((eq keyword :disabled)
              (use-package-normalize-plist name tail))
             ((functionp normalizer)
              (funcall normalizer name keyword args))
             ((= (length args) 1)
              (car args))
             (t
              args))))
      (if (memq keyword use-package-keywords)
          (cons keyword
                (cons arg (use-package-normalize-plist name tail)))
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
;;; :pin
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

(defun use-package-normalize/:pin (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       ((use-package--non-nil-symbolp arg) (symbol-name arg))
       (t
        (use-package-error
         ":pin wants an archive name (a string)"))))))

(eval-when-compile
  (defvar package-pinned-packages)
  (defvar package-archives))

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

(defun use-package-pin-package (package archive)
  "Pin PACKAGE to ARCHIVE."
  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))
  (let ((archive-symbol (if (symbolp archive) archive (intern archive)))
        (archive-name   (if (stringp archive) archive (symbol-name archive))))
    (if (use-package--archive-exists-p archive-symbol)
        (add-to-list 'package-pinned-packages (cons package archive-name))
      (error "Archive '%s' requested for package '%s' is not available."
             archive-name package))
    (unless (bound-and-true-p package--initialized)
      (package-initialize t))))

(defun use-package-handler/:pin (name keyword archive-name rest state)
  (let ((body (use-package-process-keywords name rest state))
        (pin-form (if archive-name
                      `(use-package-pin-package ',(use-package-as-symbol name)
                                                ,archive-name))))
    ;; Pinning should occur just before ensuring
    ;; See `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        (eval pin-form)              ; Eval when byte-compiling,
      (push pin-form body))          ; or else wait until runtime.
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :ensure
;;
(defvar package-archive-contents)
(defun use-package-normalize/:ensure (name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      (lambda (label arg)
        (if (symbolp arg)
            arg
          (use-package-error
           (concat ":ensure wants an optional package name "
                   "(an unquoted symbol name)")))))))

(defun use-package-ensure-elpa (name ensure state &optional no-refresh)
  (let ((package
         (or (and (eq ensure t) (use-package-as-symbol name))
             ensure)))
    (when package
      (require 'package)
      (unless (package-installed-p package)
        (condition-case-unless-debug err
            (progn
              (when (assoc package (bound-and-true-p
                                    package-pinned-packages))
                (package-read-all-archive-contents))
              (if (assoc package package-archive-contents)
                  (package-install package)
                (package-refresh-contents)
                (when (assoc package (bound-and-true-p
                                      package-pinned-packages))
                  (package-read-all-archive-contents))
                (package-install package))
              t)
          (error
           (ignore
            (display-warning 'use-package
                             (format "Failed to install %s: %s"
                                     name (error-message-string err))
                             :error))))))))

(defun use-package-handler/:ensure (name keyword ensure rest state)
  (let* ((body (use-package-process-keywords name rest state)))
    ;; We want to avoid installing packages when the `use-package' macro is
    ;; being macro-expanded by elisp completion (see `lisp--local-variables'),
    ;; but still install packages when byte-compiling, to avoid requiring
    ;; `package' at runtime.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (funcall use-package-ensure-function name ensure state)
      ;;  or else wait until runtime.
      (push `(,use-package-ensure-function ',name ',ensure ',state)
            body))
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :if, :when and :unless
;;

(defsubst use-package-normalize-value (label arg)
  "Normalize a value."
  (cond ((null arg) nil)
        ((use-package--non-nil-symbolp arg)
         `(symbol-value ',arg))
        ((functionp arg)
         `(funcall #',arg))
        (t arg)))

(defun use-package-normalize-test (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defalias 'use-package-normalize/:if 'use-package-normalize-test)
(defalias 'use-package-normalize/:when 'use-package-normalize-test)
(defalias 'use-package-normalize/:unless 'use-package-normalize-test)

(defun use-package-handler/:if (name keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((when ,pred ,@body))))

(defalias 'use-package-handler/:when 'use-package-handler/:if)

(defun use-package-handler/:unless (name keyword pred rest state)
  (let ((body (use-package-process-keywords name rest state)))
    `((unless ,pred ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :requires
;;

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

(defun use-package-normalize-symbols (label arg &optional recursed)
  "Normalize a list of symbols."
  (cond
   ((use-package--non-nil-symbolp arg)
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
   ((use-package--non-nil-symbolp arg)
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

(defalias 'use-package-normalize/:requires 'use-package-normalize-symlist)

(defun use-package-handler/:requires (name keyword requires rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if (null requires)
        body
      `((when ,(if (listp requires)
                   `(not (member nil (mapcar #'featurep ',requires)))
                 `(featurep ',requires))
          ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :load-path
;;

(defun use-package-normalize-paths (label arg &optional recursed)
  "Normalize a list of filesystem paths."
  (cond
   ((and arg (or (use-package--non-nil-symbolp arg) (functionp arg)))
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

(defun use-package-normalize/:load-path (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun use-package-handler/:load-path (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (path)
                 `(eval-and-compile (add-to-list 'load-path ,path))) arg)
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :no-require
;;

(defun use-package-normalize-predicate (name keyword args)
  (if (null args)
      t
    (use-package-only-one (symbol-name keyword) args
      #'use-package-normalize-value)))

(defalias 'use-package-normalize/:no-require 'use-package-normalize-predicate)

(defun use-package-handler/:no-require (name keyword arg rest state)
  ;; This keyword has no functional meaning.
  (use-package-process-keywords name rest
    (plist-put state :no-require t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :preface
;;

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

(defalias 'use-package-normalize/:preface 'use-package-normalize-forms)

(defun use-package-handler/:preface (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (when arg
       `((eval-and-compile ,@arg)))
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :bind, :bind*
;;

(defsubst use-package-is-pair (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying the given predicates.
CAR-PRED and CDR-PRED are applied to X's `car' and `cdr',
respectively."
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))

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

(defun use-package--recognize-function (v &optional binding additional-pred)
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
                     (use-package--non-nil-symbolp x)))) t)
    (`(,(or `quote `function)
       ,(pred use-package--non-nil-symbolp)) t)
    ((and x (guard (if binding (commandp x) (functionp x)))) t)
    (_ (and additional-pred
            (funcall additional-pred v)))))

(defun use-package--normalize-function (v)
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

(defun use-package--normalize-commands (args)
  "Map over ARGS of the form ((_ . F) ...).
Normalizing functional F's and returning a list of F's
representing symbols (that may need to be autloaded)."
  (let ((nargs (mapcar
                #'(lambda (x)
                    (if (consp x)
                        (cons (car x)
                              (use-package--normalize-function (cdr x)))
                      x)) args)))
    (cons nargs
          (delete
           nil (mapcar
                #'(lambda (x)
                    (and (consp x)
                         (use-package--non-nil-symbolp (cdr x))
                         (cdr x))) nargs)))))

(defun use-package-normalize-binder (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (consp arg)
        (use-package-error
         (concat label " a (<string or vector> . <symbol, string or function>)"
                 " or list of these")))
      (use-package-normalize-pairs
       #'(lambda (k)
           (pcase k
             ((pred stringp) t)
             ((pred vectorp) t)))
       #'(lambda (v) (use-package--recognize-function v t #'stringp))
       name label arg))))

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defun use-package-handler/:bind
    (name keyword args rest state &optional bind-macro)
  (cl-destructuring-bind (nargs . commands)
      (use-package--normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))
     `((ignore
        (,(if bind-macro bind-macro 'bind-keys)
         :package ,name ,@nargs))))))

(defun use-package-handler/:bind* (name keyword arg rest state)
  (use-package-handler/:bind name keyword arg rest state 'bind-keys*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :bind-keymap, :bind-keymap*
;;

(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

;;;###autoload
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

(defun use-package-handler/:bind-keymap
    (name keyword arg rest state &optional override)
  (let ((form
         (mapcar
          #'(lambda (binding)
              `(,(if override
                     'bind-key*
                   'bind-key)
                ,(car binding)
                #'(lambda ()
                    (interactive)
                    (use-package-autoload-keymap
                     ',(cdr binding) ',(use-package-as-symbol name)
                     ,override)))) arg)))
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-maybe-put rest :defer t))
       state)
     `((ignore ,@form)))))

(defun use-package-handler/:bind-keymap* (name keyword arg rest state)
  (use-package-handler/:bind-keymap name keyword arg rest state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :interpreter
;;

(defun use-package-normalize-mode (name keyword args)
  "Normalize arguments for keywords which add regexp/mode pairs to an alist."
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-pairs
                     #'use-package-regex-p
                     #'use-package--recognize-function
                     name)))

(defun use-package-handle-mode (name alist args rest state)
  "Handle keywords which add regexp/mode pairs to an alist."
  (cl-destructuring-bind (nargs . commands)
      (use-package--normalize-commands args)
    (let ((form
           (mapcar
            #'(lambda (thing)
                `(add-to-list
                  ',alist
                  ',(cons (use-package-normalize-regex (car thing))
                          (cdr thing))))
            nargs)))
      (use-package-concat
       (use-package-process-keywords name
         (use-package-sort-keywords
          (use-package-plist-maybe-put rest :defer t))
         (use-package-plist-append state :commands commands))
       `((ignore ,@form))))))

(defalias 'use-package-normalize/:interpreter 'use-package-normalize-mode)

(defun use-package-handler/:interpreter (name keyword arg rest state)
  (use-package-handle-mode name 'interpreter-mode-alist arg rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :mode
;;

(defalias 'use-package-normalize/:mode 'use-package-normalize-mode)

(defun use-package-handler/:mode (name keyword arg rest state)
  (use-package-handle-mode name 'auto-mode-alist arg rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :magic
;;

(defalias 'use-package-normalize/:magic 'use-package-normalize-mode)

(defun use-package-handler/:magic (name keyword arg rest state)
  (use-package-handle-mode name 'magic-mode-alist arg rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :magic-fallback
;;

(defalias 'use-package-normalize/:magic-fallback 'use-package-normalize-mode)

(defun use-package-handler/:magic-fallback (name keyword arg rest state)
  (use-package-handle-mode name 'magic-fallback-mode-alist arg rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :commands
;;

(defalias 'use-package-normalize/:commands 'use-package-normalize-symlist)

(defun use-package-handler/:commands (name keyword arg rest state)
  ;; The actual processing for commands is done in :defer
  (use-package-process-keywords name
    (use-package-sort-keywords
     (use-package-plist-maybe-put rest :defer t))
    (use-package-plist-append state :commands arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :defines
;;

(defalias 'use-package-normalize/:defines 'use-package-normalize-symlist)

(defun use-package-handler/:defines (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :functions
;;

(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)

(defun use-package-handler/:functions (name keyword arg rest state)
  (use-package-process-keywords name rest state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :defer
;;

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)

(defun use-package-handler/:defer (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest
                (plist-put state :deferred t)))
        (name-string (use-package-as-string name)))
    (use-package-concat
     ;; Load the package after a set amount of idle time, if the argument to
     ;; `:defer' was a number.
     (when (numberp arg)
       `((run-with-idle-timer ,arg nil #'require
                              ',(use-package-as-symbol name) nil t)))
     ;; Since we deferring load, establish any necessary autoloads, and also
     ;; keep the byte-compiler happy.
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             `((unless (fboundp ',command)
                 (autoload #',command ,name-string nil t)))
             (when (bound-and-true-p byte-compile-current-file)
               `((eval-when-compile
                   (declare-function ,command ,name-string)))))))
      (delete-dups (plist-get state :commands)))

     body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :after
;;

(defalias 'use-package-normalize/:after 'use-package-normalize-recursive-symlist)

(defun use-package-require-after-load (features)
  "Return form for after any of FEATURES require NAME."
  (pcase features
    ((and (pred use-package--non-nil-symbolp) feat)
     `(lambda (body)
        (list 'eval-after-load (list 'quote ',feat)
              (list 'quote body))))
    (`(,(or :or  :any) . ,rest)
     `(lambda (body)
        (append (list 'progn)
                (mapcar (lambda (form)
                          (funcall form body))
                        (list ,@(use-package-require-after-load rest))))))
    (`(,(or :and :all) . ,rest)
     `(lambda (body)
        (let ((result body))
          (dolist (form (list ,@(use-package-require-after-load rest)))
            (setq result (funcall form result)))
          result)))
    (`(,feat . ,rest)
     (if rest
         (cons (use-package-require-after-load feat)
               (use-package-require-after-load rest))
       (list (use-package-require-after-load feat))))))

(defun use-package-handler/:after (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest
                (plist-put state :deferred t)))
        (name-string (use-package-as-string name)))
    (if (and (consp arg)
             (not (memq (car arg) '(:or :any :and :all))))
        (setq arg (cons :all arg)))
    (use-package-concat
     (when arg
       (list (funcall
              (use-package-require-after-load arg)
              (macroexp-progn
               `((require (quote ,name) nil t))))))
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :demand
;;

(defalias 'use-package-normalize/:demand 'use-package-normalize-predicate)

(defun use-package-handler/:demand (name keyword arg rest state)
  (use-package-process-keywords name rest
    (use-package-plist-delete state :deferred)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :init
;;

(defalias 'use-package-normalize/:init 'use-package-normalize-forms)

(defun use-package-handler/:init (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     ;; The user's initializations
     (let ((init-body
            (use-package-hook-injector (use-package-as-string name)
                                       :init arg)))
       (if use-package-check-before-init
           `((if (locate-library ,(use-package-as-string name))
                 ,(macroexp-progn init-body)))
         init-body))
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :config
;;

(defalias 'use-package-normalize/:config 'use-package-normalize-forms)

(defun use-package-handler/:config (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name))
         (config-body
          (if (equal arg '(t))
              body
            (use-package--with-elapsed-timer
                (format "Configuring package %s" name-symbol)
              (use-package-concat
               (use-package-hook-injector (symbol-name name-symbol)
                                          :config arg)
               body
               (list t))))))
    (if (plist-get state :deferred)
        (unless (or (null config-body) (equal config-body '(t)))
          `((eval-after-load ,(if (symbolp name) `',name name)
              ',(macroexp-progn config-body))))

      (use-package--with-elapsed-timer
          (format "Loading package %s" name)
        (if use-package-expand-minimally
            (use-package-concat
             (unless (plist-get state ':no-require)
               (list (use-package-load-name name)))
             config-body)
          (if (plist-get state ':no-require)
              config-body
            `((if (not ,(use-package-load-name name t))
                  (ignore
                   (message (format "Cannot load %s" ',name)))
                ,@config-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :hook
;;

(defun use-package-normalize/:hook (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (use-package--non-nil-symbolp arg) (consp arg))
        (use-package-error
         (concat label " a <symbol> or (<symbol or list of symbols> . <symbol or function>)"
                 " or list of these")))
      (use-package-normalize-pairs
       #'(lambda (k)
           (or (use-package--non-nil-symbolp k)
               (and k (let ((every t))
                        (while (and every k)
                          (if (and (consp k)
                                   (use-package--non-nil-symbolp (car k)))
                              (setq k (cdr k))
                            (setq every nil)))
                        every))))
       #'use-package--recognize-function
       name label arg))))

(defun use-package-handler/:hook (name keyword args rest state)
  "Generate use-package custom keyword code."
  (cl-destructuring-bind (nargs . commands)
      (use-package--normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (if commands
           (use-package-sort-keywords
            (use-package-plist-maybe-put rest :defer t))
         rest)
       (if commands
           (use-package-plist-append state :commands commands)
         state))
     (cl-mapcan
      (lambda (def)
        (let ((syms (car def))
              (fun (cdr def)))
          (when fun
            (mapcar
             #'(lambda (sym)
                 `(add-hook (quote ,(intern (format "%s-hook" sym)))
                            (function ,fun)))
             (if (use-package--non-nil-symbolp syms) (list syms) syms)))))
      nargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :custom
;;

(defun use-package-normalize/:custom (name keyword args)
  "Normalize use-package custom keyword."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (listp arg)
        (use-package-error
         (concat label " a (<symbol> <value> [comment])"
                 " or list of these")))
      (if (use-package--non-nil-symbolp (car arg))
          (list arg)
        arg))))

(defun use-package-handler/:custom (name keyword args rest state)
  "Generate use-package custom keyword code."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar (lambda (def)
               (let ((variable (nth 0 def))
                     (value (nth 1 def))
                     (comment (nth 2 def)))
                 (unless (and comment (stringp comment))
                   (setq comment (format "Customized with use-package %s" name)))
                 `(customize-set-variable (quote ,variable) ,value ,comment)))
             args)
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :custom-face
;;

(defun use-package-normalize/:custom-face (name-symbol keyword arg)
  "Normalize use-package custom-face keyword."
  (let ((error-msg (format "%s wants a (<symbol> <face-spec>) or list of these" name-symbol)))
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
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar (lambda (def)
               `(custom-set-faces (quote ,def)))
             args)
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :diminish
;;

(defun use-package-normalize-diminish (name label arg &optional recursed)
  "Normalize the arguments to diminish down to a list of one of two forms:
     SYMBOL
     (SYMBOL . STRING)"
  (cond
   ((not arg)
    (list (use-package-as-mode name)))
   ((use-package--non-nil-symbolp arg)
    (list arg))
   ((stringp arg)
    (list (cons (use-package-as-mode name) arg)))
   ((and (consp arg) (stringp (cdr arg)))
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-diminish
                           name label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a string, symbol, "
             "(symbol . string) or list of these")))))

(defun use-package-normalize/:diminish (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-diminish name) t))

(defun use-package-handler/:diminish (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 `(if (fboundp 'diminish)
                      ,(if (consp var)
                           `(diminish ',(car var) ,(cdr var))
                         `(diminish ',var))))
             arg)
     body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :delight
;;

(defun use-package--normalize-delight-1 (name args)
  "Normalize ARGS for a single call to `delight'."
  (when (eq :eval (car args))
    ;; Handle likely common mistake.
    (use-package-error ":delight mode line constructs must be quoted"))
  (cond ((and (= (length args) 1)
              (use-package--non-nil-symbolp (car args)))
         `(,(nth 0 args) nil ,name))
        ((= (length args) 2)
         `(,(nth 0 args) ,(nth 1 args) ,name))
        ((= (length args) 3)
         args)
        (t
         (use-package-error
          ":delight expects `delight' arguments or a list of them"))))

(defun use-package-normalize/:delight (name keyword args)
  "Normalize arguments to delight."
  (cond ((null args)
         `((,(use-package-as-mode name) nil ,name)))
        ((and (= (length args) 1)
              (use-package--non-nil-symbolp (car args)))
         `((,(car args) nil ,name)))
        ((and (= (length args) 1)
              (stringp (car args)))
         `((,(use-package-as-mode name) ,(car args) ,name)))
        ((and (= (length args) 1)
              (listp (car args))
              (eq 'quote (caar args)))
         `((,(use-package-as-mode name) ,@(cdar args) ,name)))
        ((and (= (length args) 2)
              (listp (nth 1 args))
              (eq 'quote (car (nth 1 args))))
         `((,(car args) ,@(cdr (nth 1 args)) ,name)))
        (t (mapcar
            (apply-partially #'use-package--normalize-delight-1 name)
            (if (use-package--non-nil-symbolp (car args))
                (list args)
              args)))))

(defun use-package-handler/:delight (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((if (fboundp 'delight)
           (delight '(,@args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; The main macro
;;

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
  (unless (member :disabled args)
    (let ((name-symbol (if (stringp name) (intern name) name))
          (orig-args args)
          (args (use-package-normalize-plist name args)))
      (dolist (spec use-package-defaults)
        (setq args (if (eval (nth 2 spec))
                       (use-package-plist-maybe-put
                        args (nth 0 spec) (eval (nth 1 spec)))
                     args)))

      ;; When byte-compiling, pre-load the package so all its symbols are in
      ;; scope.
      (if (bound-and-true-p byte-compile-current-file)
          (setq args
                (use-package-plist-append
                 args :preface
                 (use-package-concat
                  (mapcar #'(lambda (var) `(defvar ,var))
                          (plist-get args :defines))
                  (mapcar #'(lambda (fn) `(declare-function
                                      ,fn ,(use-package-as-string name)))
                          (plist-get args :functions))
                  `((eval-when-compile
                      (with-demoted-errors
                          ,(format "Cannot load %s: %%S" name)
                        ,(if (eq use-package-verbose 'debug)
                             `(message "Compiling package %s" ',name-symbol))
                        ,(unless (plist-get args :no-require)
                           `(load ,(if (stringp name)
                                       name
                                     (symbol-name name)) nil t)))))))))

      (let ((body
             `(progn
                ,@(use-package-process-keywords name
                    (let ((args*
                           (use-package-sort-keywords
                            (if (and use-package-always-demand
                                     (not (memq :defer args)))
                                (plist-put args :demand t)
                              args))))
                      ;; The :demand keyword should not override :after
                      (if (and (plist-member args* :after)
                               (plist-member args* :demand))
                          (setq args* (use-package-plist-delete args* :demand)))
                      (when (and use-package-always-ensure
                                 (plist-member args* :load-path)
                                 (not (plist-member orig-args :ensure)))
                        (plist-put args* :ensure nil))
                      (unless (plist-member args* :init)
                        (plist-put args* :init nil))
                      (unless (plist-member args* :config)
                        (plist-put args* :config '(t)))
                      args*)
                    (and use-package-always-defer
                         (list :deferred t))))))
        (when use-package-debug
          (display-buffer
           (save-current-buffer
             (with-current-buffer (get-buffer-create "*use-package*")
               (goto-char (point-max))
               (emacs-lisp-mode)
               (insert (pp-to-string body))
               (current-buffer)))))
        body))))


(put 'use-package 'lisp-indent-function 'defun)

(provide 'use-package)

;; Local Variables:
;; outline-regexp: ";;;\\(;* [^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:

;;; use-package.el ends here
