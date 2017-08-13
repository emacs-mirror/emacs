;;; use-package.el --- A use-package declaration for simplifying your .emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Maintainer: John Wiegley <jwiegley@gmail.com>
;; Created: 17 Jun 2012
;; Modified: 17 Oct 2016
;; Version: 2.3
;; Package-Requires: ((bind-key "1.0") (diminish "0.44"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

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
(eval-when-compile (require 'regexp-opt))

(declare-function package-installed-p "package")
(declare-function package-read-all-archive-contents "package" ())

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

(defcustom use-package-always-defer-install nil
  "If non-nil, assume `:defer-install t` unless `:defer-install nil` is given."
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
    :preface
    :pin
    :defer-install
    :ensure
    :if
    :when
    :unless
    :requires
    :load-path
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
    :defines
    :functions
    :defer
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
to `:ensure'; the current `state' plist created by previous
handlers; and a keyword indicating the context in which the
installation is occurring.

Note that this function is called whenever `:ensure' is provided,
even if it is nil. It is up to the function to decide on the
semantics of the various values for `:ensure'.

This function should return non-nil if the package is installed.

The default value uses package.el to install the package.

Possible values for the context keyword are:

:byte-compile - package installed during byte-compilation
:ensure - package installed normally by :ensure
:autoload - deferred installation triggered by an autoloaded
            function
:after - deferred installation triggered by the loading of a
         feature listed in the :after declaration
:config - deferred installation was specified at the same time
          as :demand, so the installation was triggered
          immediately
:unknown - context not provided

Note that third-party code can provide other values for the
context keyword by calling `use-package-install-deferred-package'
with the appropriate value."
  :type '(choice (const :tag "package.el" use-package-ensure-elpa)
                 (function :tag "Custom"))
  :group 'use-package)

(defcustom use-package-pre-ensure-function 'ignore
  "Function that is called upon installation deferral.
It is called immediately with the first three arguments that
would be passed to `use-package-ensure-function' (the context
keyword is omitted), but only if installation has been deferred.
It is intended for package managers other than package.el which
might want to activate the autoloads of a package immediately, if
it's installed, but otherwise defer installation until later (if
`:defer-install' is specified). The reason it is set to `ignore'
by default is that package.el activates the autoloads for all
known packages at initialization time, rather than one by one
when the packages are actually requested."
  :type '(choice (const :tag "None" ignore)
                 (function :tag "Custom"))
  :group 'use-package)

(defcustom use-package-defaults
  '((:config '(t) t)
    (:ensure use-package-always-ensure use-package-always-ensure)
    (:defer-install
     use-package-always-defer-install
     use-package-always-defer-install)
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
function will jump to the file that orginally required PACKAGE
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
      (when body
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
                                     "--post-" keyword-name "-hook"))))))))))))

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

(defun use-package-regex-p (re)
  "Return t if RE is some regexp-like thing."
  (cond
   ((and (listp re)
         (eq (car re) 'rx))
    t)
   ((stringp re)
    t)
   (t
    nil)))

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
        (use-package-error (format "Unrecognized keyword: %s" keyword))))))

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
       ((symbolp arg) (symbol-name arg))
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
;;; :defer-install
;;

(defvar use-package--deferred-packages (make-hash-table)
  "Hash mapping packages to data about their installation.

The keys are not actually symbols naming packages, but rather
symbols naming the features which are the names of \"packages\"
required by `use-package' forms. Since
`use-package-ensure-function' could be set to anything, it is
actually impossible for `use-package' to determine what package
is supposed to provide the feature being ensured just based on
the value of `:ensure'.

Each value is a cons, with the car being the the value passed to
`:ensure' and the cdr being the `state' plist. See
`use-package-install-deferred-package' for information about how
these values are used to call `use-package-ensure-function'.")

(defun use-package-install-deferred-package (name &optional context)
  "Install a package whose installation has been deferred.
NAME should be a symbol naming a package (actually, a feature).
This is done by calling `use-package-ensure-function' is called
with four arguments: the key (NAME) and the two elements of the
cons in `use-package--deferred-packages' (the value passed to
`:ensure', and the `state' plist), and a keyword providing
information about the context in which the installation is
happening. (This defaults to `:unknown' but can be overridden by
providing CONTEXT.)

Return t if the package is installed, nil otherwise. (This is
determined by the return value of `use-package-ensure-function'.)
If the package is installed, its entry is removed from
`use-package--deferred-packages'. If the package has no entry in
`use-package--deferred-packages', do nothing and return t."
  (interactive
   (let ((packages nil))
     (maphash (lambda (package info)
                (push package packages))
              use-package--deferred-packages)
     (if packages
         (list
          (intern
           (completing-read
            "Select package: "
            packages
            nil
            'require-match))
          :interactive)
       (user-error "No packages with deferred installation"))))
  (let ((spec (gethash name use-package--deferred-packages)))
    (if spec
        (when (funcall use-package-ensure-function
                       name (car spec) (cdr spec)
                       (or context :unknown))
          (remhash name use-package--deferred-packages)
          t)
      t)))

(defalias 'use-package-normalize/:defer-install 'use-package-normalize-test)

(defun use-package-handler/:defer-install (name keyword defer rest state)
  (use-package-process-keywords name rest
    ;; Just specifying `:defer-install' does not do anything; this
    ;; sets up a marker so that if `:ensure' is specified as well then
    ;; it knows to set up deferred installation. But then later, when
    ;; `:config' is processed, it might turn out that `:demand' was
    ;; specified as well, and the deferred installation needs to be
    ;; run immediately. For this we need to know if the deferred
    ;; installation was actually set up or not, so we need to set one
    ;; marker value in `:defer-install', and then change it to a
    ;; different value in `:ensure', if the first one is present. (The
    ;; first marker is `:defer-install', and the second is `:ensure'.)
    (plist-put state :defer-install (when defer :defer-install))))

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

(defun use-package-ensure-elpa (name ensure state context &optional no-refresh)
  (let ((package (or (when (eq ensure t) (use-package-as-symbol name))
                     ensure)))
    (when package
      (require 'package)
      (or (package-installed-p package)
          (not (or
                ;; Contexts in which the confirmation prompt is
                ;; bypassed.
                (member context '(:byte-compile :ensure :config))
                (y-or-n-p (format "Install package %S?" package))))
          (with-demoted-errors (format "Cannot load %s: %%S" name)
            (when (assoc package (bound-and-true-p package-pinned-packages))
              (package-read-all-archive-contents))
            (if (assoc package package-archive-contents)
                (progn (package-install package) t)
              (progn
                (package-refresh-contents)
                (when (assoc package (bound-and-true-p
                                      package-pinned-packages))
                  (package-read-all-archive-contents))
                (package-install package))))))))

(defun use-package-handler/:ensure (name keyword ensure rest state)
  (let* ((body (use-package-process-keywords name rest
                 ;; Here we are conditionally updating the marker
                 ;; value for deferred installation; this will be
                 ;; checked later by `:config'. For more information
                 ;; see `use-package-handler/:defer-install'.
                 (if (eq (plist-get state :defer-install)
                         :defer-install)
                     (plist-put state :defer-install :ensure)
                   state))))
    ;; We want to avoid installing packages when the `use-package'
    ;; macro is being macro-expanded by elisp completion (see
    ;; `lisp--local-variables'), but still do install packages when
    ;; byte-compiling to avoid requiring `package' at runtime.
    (cond
     ((plist-get state :defer-install)
      (push
       `(puthash ',name '(,ensure . ,state)
                 use-package--deferred-packages)
       body)
      (push `(,use-package-pre-ensure-function
              ',name ',ensure ',state)
            body))
     ((bound-and-true-p byte-compile-current-file)
      ;; Eval when byte-compiling,
      (funcall use-package-ensure-function
               name ensure state :byte-compile))
     ;;  or else wait until runtime.
     (t (push `(,use-package-ensure-function
                ',name ',ensure ',state :ensure)
              body)))
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :if, :when and :unless
;;

(defsubst use-package-normalize-value (label arg)
  "Normalize a value."
  (cond ((null arg) nil)
        ((symbolp arg)
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

(defun use-package-normalize-symlist (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'use-package-normalize-symbols))

(defun use-package-normalize-recursive-symbols (label arg)
  "Normalize a list of symbols."
  (cond
   ((symbolp arg)
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
   ((and arg (or (symbolp arg) (functionp arg)))
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
  (use-package-process-keywords name rest state))

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
     (unless (null arg)
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
    (let ((last-item nil))
      (mapcar #'(lambda (x)
                  (prog1
                      (let ((ret (use-package-normalize-pairs
                                  key-pred val-pred name label x t)))
                        ;; Currently, the handling of keyword
                        ;; arguments by `use-package' and `bind-key'
                        ;; is non-uniform and undocumented. As a
                        ;; result, `use-package-normalize-pairs' (as
                        ;; it is currently implemented) does not
                        ;; correctly handle the keyword-argument
                        ;; syntax of `bind-keys'. A permanent solution
                        ;; to this problem will require a careful
                        ;; consideration of the desired
                        ;; keyword-argument interface for
                        ;; `use-package' and `bind-key'. However, in
                        ;; the meantime, we have a quick patch to fix
                        ;; a serious bug in the handling of keyword
                        ;; arguments. Namely, the code below would
                        ;; normally unwrap lists that were passed as
                        ;; keyword arguments (for example, the
                        ;; `:filter' argument in `:bind') without
                        ;; the (not (keywordp last-item)) clause. See
                        ;; #447 for further discussion.
                        (if (and (listp ret) (not (keywordp last-item)))
                            (car ret)
                          ret))
                    (setq last-item x))) arg)))
   (t arg)))

(defun use-package-normalize-binder (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (consp arg)
        (use-package-error
         (concat label " a (<string or vector> . <symbol or string>)"
                 " or list of these")))
      (use-package-normalize-pairs (lambda (k) (or (stringp k) (vectorp k)))
                                   (lambda (b) (or (symbolp b) (stringp b)))
                                   name label arg))))

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defun use-package-handler/:bind
    (name keyword arg rest state &optional bind-macro)
  (let ((commands (remq nil (mapcar #'(lambda (arg)
                                        (if (listp arg)
                                            (cdr arg)
                                          nil)) arg))))
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))
     `((ignore
        ,(macroexpand
          `(,(if bind-macro bind-macro 'bind-keys)
            :package ,name ,@arg)))))))

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
  (let ((form (mapcar
               #'(lambda (binding)
                   `(,(if override
                          'bind-key*
                        'bind-key)
                     ,(car binding)
                     #'(lambda ()
                         (interactive)
                         (use-package-autoload-keymap
                          ',(cdr binding) ',(use-package-as-symbol name) ,override)))) arg)))
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
                     (lambda (m) (and (not (null m)) (symbolp m)))
                     name)))

(defun use-package-handle-mode (name alist arg rest state)
  "Handle keywords which add regexp/mode pairs to an alist."
  (let* (commands
         (form (mapcar #'(lambda (thing)
                           (push (cdr thing) commands)
                           (setcar thing
                                   (use-package-normalize-regex (car thing)))
                           `(add-to-list ',alist ',thing)) arg)))
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))
     `((ignore ,@form)))))

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
  (let ((body (use-package-process-keywords name rest state)))
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :functions
;;

(defalias 'use-package-normalize/:functions 'use-package-normalize-symlist)

(defun use-package-handler/:functions (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if (not (bound-and-true-p byte-compile-current-file))
        body
      (use-package-concat
       (unless (null arg)
         `((eval-when-compile
             ,@(mapcar
                #'(lambda (fn)
                    `(declare-function ,fn ,(use-package-as-string name))) arg))))
       body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :defer
;;

(defalias 'use-package-normalize/:defer 'use-package-normalize-predicate)

(defun use-package--autoload-with-deferred-install
    (command package-name)
  "Return a form defining an autoload supporting deferred install."
  `(let* ((load-list-item '(defun . ,command))
          (already-loaded (member load-list-item current-load-list)))
     (defun ,command (&rest args)
       "[Arg list not available until function definition is loaded.]

\(fn ...)"
       (interactive)
       (if (bound-and-true-p use-package--recursive-autoload)
           (use-package-error
            (format "Autoloading failed to define function %S"
                    ',command))
         (when (use-package-install-deferred-package
                ',package-name :autoload)
           (require ',package-name)
           (let ((use-package--recursive-autoload t))
             (if (called-interactively-p 'any)
                 (call-interactively ',command)
               (apply ',command args))))))
     ;; This prevents the user's init-file from being recorded as the
     ;; definition location for the function before it is actually
     ;; loaded. (Our goal is to leave the `current-load-list'
     ;; unchanged, so we only remove the entry for this function if it
     ;; was not already present.)
     (unless already-loaded
       (setq current-load-list (remove load-list-item current-load-list)))))

(defun use-package-handler/:defer (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest
                (plist-put state :deferred t)))
        (name-string (use-package-as-string name)))
    (use-package-concat
     ;; Load the package after a set amount of idle time, if the argument to
     ;; `:defer' was a number.
     (when (numberp arg)
       `((run-with-idle-timer ,arg nil #'require ',(use-package-as-symbol name) nil t)))

     ;; Since we deferring load, establish any necessary autoloads, and also
     ;; keep the byte-compiler happy.
     (apply
      #'nconc
      (mapcar
       #'(lambda (command)
           (when (not (stringp command))
             (append
              `((unless (fboundp ',command)
                  ;; Here we are checking the marker value set in
                  ;; `use-package-handler/:ensure' to see if deferred
                  ;; installation is actually happening. See
                  ;; `use-package-handler/:defer-install' for more
                  ;; information.
                  ,(if (eq (plist-get state :defer-install) :ensure)
                       (use-package--autoload-with-deferred-install
                        command name)
                     `(autoload #',command ,name-string nil t))))
              (when (bound-and-true-p byte-compile-current-file)
                `((eval-when-compile
                    (declare-function ,command ,name-string)))))))
       (delete-dups (plist-get state :commands))))

     body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :after
;;

(defalias 'use-package-normalize/:after 'use-package-normalize-recursive-symlist)

(defun use-package-require-after-load
    (features)
  "Return form for after any of FEATURES require NAME."
  (pcase features
    ((and (pred symbolp) feat)
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
       (list (funcall (use-package-require-after-load arg)
                      (macroexp-progn
                       ;; Here we are checking the marker value for deferred
                       ;; installation set in `use-package-handler/:ensure'.
                       ;; See also `use-package-handler/:defer-install'.
                       `(,@(when (eq (plist-get state :defer-install) :ensure)
                             `((use-package-install-deferred-package
                                'name :after)))
                         (require (quote ,name) nil t))))))
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
      ;; Here we are checking the marker value for deferred
      ;; installation set in `use-package-handler/:ensure'. See also
      ;; `use-package-handler/:defer-install'.
      (when (eq (plist-get state :defer-install) :ensure)
        (use-package-install-deferred-package name :config))
      (use-package--with-elapsed-timer
          (format "Loading package %s" name)
        (if use-package-expand-minimally
            (use-package-concat
             (list (use-package-load-name name))
             config-body)
          `((if (not ,(use-package-load-name name t))
                (ignore
                 (message (format "Cannot load %s" ',name)))
              ,@config-body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; :diminish
;;

(defun use-package-normalize-diminish (name label arg &optional recursed)
  "Normalize the arguments to diminish down to a list of one of two forms:
     SYMBOL
     (SYMBOL . STRING)"
  (cond
   ((symbolp arg)
    (list arg))
   ((stringp arg)
    (list (cons (intern (concat (use-package-as-string name) "-mode")) arg)))
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
    (apply-partially #'use-package-normalize-diminish name)))

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
  (cond ((and (= (length args) 1) (symbolp (car args)))
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
              (symbolp (car args)))
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
            (if (symbolp (car args)) (list args) args)))))

(defun use-package-handler/:delight (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((delight '(,@args))))))

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
:mode            Form to be added to `auto-mode-alist'.
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
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive."
  (declare (indent 1))
  (unless (member :disabled args)
    (let ((name-symbol (if (stringp name) (intern name) name))
          (args (use-package-normalize-plist name args)))
      (dolist (spec use-package-defaults)
        (setq args (use-package-sort-keywords
                    (if (eval (nth 2 spec))
                        (use-package-plist-maybe-put
                         args (nth 0 spec) (eval (nth 1 spec)))
                      args))))

      ;; When byte-compiling, pre-load the package so all its symbols are in
      ;; scope.
      (if (bound-and-true-p byte-compile-current-file)
          (setq args
                (use-package-plist-cons
                 args :preface
                 `(eval-when-compile
                    ,@(mapcar #'(lambda (var) `(defvar ,var))
                              (plist-get args :defines))
                    (with-demoted-errors
                        ,(format "Cannot load %s: %%S" name)
                      ,(if (eq use-package-verbose 'debug)
                           `(message "Compiling package %s" ',name-symbol))
                      ,(unless (plist-get args :no-require)
                         (use-package-load-name name)))))))

      (let ((body
             (macroexp-progn
              (use-package-process-keywords name
                (if use-package-always-demand
                    (append args '(:demand t))
                  args)
                (and use-package-always-defer (list :deferred t))))))
        (if use-package-debug
            (display-buffer
             (save-current-buffer
               (let ((buf (get-buffer-create "*use-package*")))
                 (with-current-buffer buf
                   (delete-region (point-min) (point-max))
                   (emacs-lisp-mode)
                   (insert (pp-to-string body)))
                 buf))))
        body))))


(put 'use-package 'lisp-indent-function 'defun)

(provide 'use-package)

;; Local Variables:
;; outline-regexp: ";;;\\(;* [^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:

;;; use-package.el ends here
