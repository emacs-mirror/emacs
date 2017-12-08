;;; use-package-ensure.el --- Support for the :ensure and :pin keywords

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 3 Dec 2017
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
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

;; Provides support for the :ensure and :pin keywords, which is made available
;; by default by requiring `use-package'.

;;; Code:

(require 'cl-lib)
(require 'use-package-core)

(defgroup use-package-ensure nil
  "Support for :ensure and :pin keywords in use-package declarations."
  :group 'use-package)

(eval-when-compile
  (declare-function package-installed-p "package")
  (declare-function package-read-all-archive-contents "package" ()))

(defcustom use-package-always-ensure nil
  "Treat every package as though it had specified using `:ensure SEXP'.
See also `use-package-defaults', which uses this value."
  :type 'sexp
  :group 'use-package-ensure)

(defcustom use-package-always-pin nil
  "Treat every package as though it had specified using `:pin SYM'.
See also `use-package-defaults', which uses this value."
  :type 'symbol
  :group 'use-package-ensure)

(defcustom use-package-ensure-function 'use-package-ensure-elpa
  "Function that ensures a package is installed.
This function is called with three arguments: the name of the
package declared in the `use-package' form; the arguments passed
to all `:ensure' keywords (always a list, even if only one); and
the current `state' plist created by previous handlers.

Note that this function is called whenever `:ensure' is provided,
even if it is nil. It is up to the function to decide on the
semantics of the various values for `:ensure'.

This function should return non-nil if the package is installed.

The default value uses package.el to install the package."
  :type '(choice (const :tag "package.el" use-package-ensure-elpa)
                 (function :tag "Custom"))
  :group 'use-package-ensure)

;;;; :pin

(defun use-package-normalize/:pin (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (label arg)
        (cond
         ((stringp arg) arg)
         ((use-package-non-nil-symbolp arg) (symbol-name arg))
         (t
          (use-package-error
           ":pin wants an archive name (a string)"))))))

(eval-when-compile
  (defvar package-pinned-packages)
  (defvar package-archives))

(defun use-package-archive-exists-p (archive)
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
    (if (use-package-archive-exists-p archive-symbol)
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

;;;; :ensure

(defvar package-archive-contents)

;;;###autoload
(defun use-package-normalize/:ensure (name keyword args)
  (if (null args)
      (list t)
    (use-package-only-one (symbol-name keyword) args
      #'(lambda (label arg)
          (cond
           ((symbolp arg)
            (list arg))
           ((and (listp arg) (= 3 (length arg))
                 (symbolp (nth 0 arg))
                 (eq :pin (nth 1 arg))
                 (or (stringp (nth 2 arg))
                     (symbolp (nth 2 arg))))
            (list (cons (nth 0 arg) (nth 2 arg))))
           (t
            (use-package-error
             (concat ":ensure wants an optional package name "
                     "(an unquoted symbol name), or (<symbol> :pin <string>)"))))))))

(defun use-package-ensure-elpa (name args state &optional no-refresh)
  (dolist (ensure args)
    (let ((package
           (or (and (eq ensure t) (use-package-as-symbol name))
               ensure)))
      (when package
        (require 'package)
        (when (consp package)
          (use-package-pin-package (car package) (cdr package))
          (setq package (car package)))
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
             (display-warning 'use-package
                              (format "Failed to install %s: %s"
                                      name (error-message-string err))
                              :error))))))))

;;;###autoload
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

(add-to-list 'use-package-defaults
             '(:ensure (list use-package-always-ensure)
                       (lambda (name args)
                         (and use-package-always-ensure
                              (not (plist-member args :load-path))))) t)

(add-to-list 'use-package-defaults
             '(:pin use-package-always-pin use-package-always-pin) t)

(add-to-list 'use-package-keywords :ensure)
(add-to-list 'use-package-keywords :pin)

(provide 'use-package-ensure)

;;; use-package-ensure.el ends here
