;;; use-package-ensure.el --- Support for the :ensure and :pin keywords  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>

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

;; Provides support for the :ensure and :pin keywords, which are made
;; available by default by requiring `use-package'.
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'cl-lib)
(require 'use-package-core)

(declare-function package-installed-p "package")
(declare-function package-read-all-archive-contents "package" ())

(defvar use-package-ensure-install-during-compile nil
  "If non-nil, `:ensure' causes installation during compilation (deprecated).
If nil, package installation happens only at run-time, which is much more
sane since compilation is usually presumed to be a \"pure function\"
with no other side-effects than saving the resulting `.elc' file.")

;;;; :pin

(defun use-package-normalize/:pin (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label arg)
        (cond
         ((stringp arg) arg)
         ((use-package-non-nil-symbolp arg) (symbol-name arg))
         (t
          (use-package-error
           ":pin wants an archive name (a string)"))))))

(defun use-package-archive-exists-p (archive)
  "Check if a given ARCHIVE is enabled.

ARCHIVE can be a string or a symbol or `manual' to indicate a
manually updated package."
  (if (member archive '(manual "manual"))
      't
    (require 'package)
    (defvar package-archives)
    (let ((valid nil))
      (dolist (pa package-archives)
        (when (member archive (list (car pa) (intern (car pa))))
          (setq valid 't)))
      valid)))

(define-inline use-package-pin-package (package archive)
  ;; Make sure the expansion of calls to `use-package' doesn't need to
  ;; load us at run-time.
  "Pin PACKAGE to ARCHIVE."
  (let* ((package (inline-const-val package))
         (archive (inline-const-val archive))
         (archive-symbol (if (symbolp archive) archive (intern archive)))
         (archive-name   (if (stringp archive) archive (symbol-name archive))))
    (unless (use-package-archive-exists-p archive-symbol)
      (warn "Archive '%s' requested for package '%s' is not available"
            archive-name package))
    (inline-quote
     (progn
       ;; FIXME: After N compiler-macro expansions, we end up with N copies of
       ;; this `(setq package-pinned-packages nil)' business.
       (defvar package-pinned-packages)
       (unless (boundp 'package-pinned-packages)
         (setq package-pinned-packages ()))
       (add-to-list 'package-pinned-packages
                    ',(cons package archive-name))))))

(defun use-package-handler/:pin (name _keyword archive-name rest state)
  (let ((body (use-package-process-keywords name rest state))
        (pin-form (if archive-name
                      `(use-package-pin-package ',(use-package-as-symbol name)
                                                ,archive-name))))
    ;; Pinning should occur just before ensuring
    ;; See `use-package-handler/:ensure'.
    (if (and use-package-ensure-install-during-compile
             (use-package--macroexp-compiling-p))
        (eval pin-form t)               ; Eval when byte-compiling,
      (push pin-form body))             ; or else wait until runtime.
    body))

;;;; :ensure

;;;###autoload
(defun use-package-normalize/:ensure (_name keyword args)
  (if (null args)
      (list t)
    (use-package-only-one (symbol-name keyword) args
      #'(lambda (_label arg)
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

(define-inline use-package-ensure-elpa (name args state &optional _no-refresh)
  ;; Make sure the expansion of calls to `use-package' doesn't need to
  ;; load us at run-time in the common case.
  (let* ((name (inline-const-val name))
         (args (inline-const-val args)))
    (when args
      (let* ((ensure (pop args))
             (package (or (and (eq ensure t)
                               (use-package-as-symbol name))
                          ensure))
             (pin (if (consp package)
                      (prog1 package (setq package (car package))))))
        (inline-quote
         (progn
           ,(if pin (inline-quote
                     (use-package-pin-package ',(car pin) ',(cdr pin))))
           ,(if package
                (inline-quote
                 (unless (package-installed-p ',package)
                   ;; This is assumed to be an uncommon case.
                   (use-package-ensure-installed ',package))))
           (use-package-ensure-elpa ',name ',args ',state)))))))

;;;###autoload
(defun use-package-ensure-installed (package)
  (require 'package)
  (defvar package-archive-contents)
  (defvar package-pinned-packages)
  (condition-case-unless-debug err
      (progn
        ;; FIXME: Fold these `package-read-all-archive-contents' and
        ;; `package-refresh-contents' calls into `package-install'?
        (when (assoc package package-pinned-packages)
          (package-read-all-archive-contents))
        (if (assoc package package-archive-contents)
            nil
          (package-refresh-contents)
          (when (assoc package package-pinned-packages)
            (package-read-all-archive-contents)))
        (package-install package)
        t)
    (error
     (display-warning 'use-package
                      (format "Failed to install %s: %s"
                              package (error-message-string err))
                      :error))))

;;;###autoload
(defun use-package-handler/:ensure (name _keyword ensure rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (ensure (and (not (plist-member rest :vc)) ensure)))
    (if (and use-package-ensure-install-during-compile
             (use-package--macroexp-compiling-p))
        ;; Eval when byte-compiling,
        (funcall use-package-ensure-function name ensure state)
      ;;  or else wait until runtime.
      (push `(,use-package-ensure-function ',name ',ensure ',state)
            body))
    body))

(provide 'use-package-ensure)

;;; use-package-ensure.el ends here
