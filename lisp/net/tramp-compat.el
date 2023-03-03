;;; tramp-compat.el --- Tramp compatibility functions  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2023 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; Tramp's main Emacs version for development is Emacs 30.  This
;; package provides compatibility functions for Emacs 27, Emacs 28 and
;; Emacs 29.

;;; Code:

(require 'auth-source)
(require 'format-spec)
(require 'ls-lisp) ;; Due to `tramp-handle-insert-directory'.
(require 'parse-time)
(require 'shell)
(require 'subr-x)

(declare-function tramp-error "tramp")
(declare-function tramp-tramp-file-p "tramp")
(defvar tramp-temp-name-prefix)

(defconst tramp-compat-emacs-compiled-version (eval-when-compile emacs-version)
  "The Emacs version used for compilation.")

(unless (= emacs-major-version
	   (car (version-to-list tramp-compat-emacs-compiled-version)))
  (warn "Tramp has been compiled with Emacs %s, this is Emacs %s"
	tramp-compat-emacs-compiled-version emacs-version))

(with-eval-after-load 'docker-tramp
  (warn (concat "Package `docker-tramp' has been obsoleted, "
		"please use integrated package `tramp-container'")))
(with-eval-after-load 'kubernetes-tramp
  (warn (concat "Package `kubernetes-tramp' has been obsoleted, "
		"please use integrated package `tramp-container'")))

;; For not existing functions, obsolete functions, or functions with a
;; changed argument list, there are compiler warnings.  We want to
;; avoid them in cases we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS if it exists.  Do not raise compiler warnings."
  `(when (functionp ,function)
     (with-no-warnings (funcall ,function ,@arguments))))

;; We must use a local directory.  If it is remote, we could run into
;; an infloop.
(defconst tramp-compat-temporary-file-directory
  (eval (car (get 'temporary-file-directory 'standard-value)) t)
  "The default value of `temporary-file-directory'.")

(defsubst tramp-compat-make-temp-name ()
  "Generate a local temporary file name (compat function)."
  (make-temp-name
   (expand-file-name
    tramp-temp-name-prefix tramp-compat-temporary-file-directory)))

(defsubst tramp-compat-make-temp-file (f &optional dir-flag)
  "Create a local temporary file (compat function).
Add the extension of F, if existing."
  (make-temp-file
   (expand-file-name
    tramp-temp-name-prefix tramp-compat-temporary-file-directory)
   dir-flag (file-name-extension f t)))

;; `file-modes', `set-file-modes' and `set-file-times' got argument
;; FLAG in Emacs 28.1.
(defalias 'tramp-compat-file-modes
  (if (equal (func-arity #'file-modes) '(1 . 2))
      #'file-modes
    (lambda (filename &optional _flag)
      (file-modes filename))))

(defalias 'tramp-compat-set-file-modes
  (if (equal (func-arity #'set-file-modes) '(2 . 3))
      #'set-file-modes
    (lambda (filename mode &optional _flag)
      (set-file-modes filename mode))))

(defalias 'tramp-compat-set-file-times
  (if (equal (func-arity #'set-file-times) '(1 . 3))
      #'set-file-times
    (lambda (filename &optional timestamp _flag)
      (set-file-times filename timestamp))))

;; `directory-files' and `directory-files-and-attributes' got argument
;; COUNT in Emacs 28.1.
(defalias 'tramp-compat-directory-files
  (if (equal (func-arity #'directory-files) '(1 . 5))
      #'directory-files
    (lambda (directory &optional full match nosort _count)
      (directory-files directory full match nosort))))

(defalias 'tramp-compat-directory-files-and-attributes
  (if (equal (func-arity #'directory-files-and-attributes) '(1 . 6))
      #'directory-files-and-attributes
    (lambda (directory &optional full match nosort id-format _count)
      (directory-files-and-attributes directory full match nosort id-format))))

;; `directory-empty-p' is new in Emacs 28.1.
(defalias 'tramp-compat-directory-empty-p
  (if (fboundp 'directory-empty-p)
      #'directory-empty-p
    (lambda (dir)
      (and (file-directory-p dir)
	   (null (tramp-compat-directory-files
		  dir nil directory-files-no-dot-files-regexp t 1))))))

;; Function `null-device' is new in Emacs 28.1.
(defalias 'tramp-compat-null-device
  (if (fboundp 'null-device)
      #'null-device
    (lambda ()
      (if (tramp-tramp-file-p default-directory) "/dev/null" null-device))))

;; Function `string-replace' is new in Emacs 28.1.
(defalias 'tramp-compat-string-replace
  (if (fboundp 'string-replace)
      #'string-replace
    (lambda (from-string to-string in-string)
      (let (case-fold-search)
        (replace-regexp-in-string
         (regexp-quote from-string) to-string in-string t t)))))

;; Function `string-search' is new in Emacs 28.1.
(defalias 'tramp-compat-string-search
  (if (fboundp 'string-search)
      #'string-search
    (lambda (needle haystack &optional start-pos)
      (let (case-fold-search)
        (string-match-p (regexp-quote needle) haystack start-pos)))))

;; Function `make-lock-file-name' is new in Emacs 28.1.
(defalias 'tramp-compat-make-lock-file-name
  (if (fboundp 'make-lock-file-name)
      #'make-lock-file-name
    (lambda (filename)
      (expand-file-name
       (concat
        ".#" (file-name-nondirectory filename))
       (file-name-directory filename)))))

;; Function `file-name-concat' is new in Emacs 28.1.
(defalias 'tramp-compat-file-name-concat
  (if (fboundp 'file-name-concat)
      #'file-name-concat
    (lambda (directory &rest components)
      (let ((components (cl-remove-if (lambda (el)
                                        (or (null el) (equal "" el)))
                                      components))
	    file-name-handler-alist)
        (if (null components)
	    directory
          (apply #'tramp-compat-file-name-concat
	         (concat (unless (or (equal "" directory) (null directory))
                           (file-name-as-directory directory))
                         (car components))
	         (cdr components)))))))

;; Function `replace-regexp-in-region' is new in Emacs 28.1.
(defalias 'tramp-compat-replace-regexp-in-region
  (if (fboundp 'replace-regexp-in-region)
      #'replace-regexp-in-region
    (lambda (regexp replacement &optional start end)
      (if start
	  (when (< start (point-min))
            (error "Start before start of buffer"))
	(setq start (point)))
      (if end
	  (when (> end (point-max))
            (error "End after end of buffer"))
	(setq end (point-max)))
      (save-excursion
	(let ((matches 0)
              (case-fold-search nil))
	  (goto-char start)
	  (while (re-search-forward regexp end t)
            (replace-match replacement t)
            (setq matches (1+ matches)))
	  (and (not (zerop matches))
               matches))))))

;; `length<', `length>' and `length=' are added to Emacs 28.1.
(defalias 'tramp-compat-length<
  (if (fboundp 'length<)
      #'length<
    (lambda (sequence length)
      (< (length sequence) length))))

(defalias 'tramp-compat-length>
  (if (fboundp 'length>)
      #'length>
    (lambda (sequence length)
      (> (length sequence) length))))

(defalias 'tramp-compat-length=
  (if (fboundp 'length=)
      #'length=
    (lambda (sequence length)
      (= (length sequence) length))))

;; `permission-denied' is introduced in Emacs 29.1.
(defconst tramp-permission-denied
  (if (get 'permission-denied 'error-conditions) 'permission-denied 'file-error)
  "The error symbol for the `permission-denied' error.")

(defsubst tramp-compat-permission-denied (vec file)
  "Emit the `permission-denied' error."
  (if (get 'permission-denied 'error-conditions)
      (tramp-error vec tramp-permission-denied file)
    (tramp-error vec tramp-permission-denied "Permission denied: %s" file)))

;; Function `auth-info-password' is new in Emacs 29.1.
(defalias 'tramp-compat-auth-info-password
  (if (fboundp 'auth-info-password)
      #'auth-info-password
    (lambda (auth-info)
      (let ((secret (plist-get auth-info :secret)))
	(while (functionp secret)
          (setq secret (funcall secret)))
	secret))))

;; Function `take' is new in Emacs 29.1.
(defalias 'tramp-compat-take
  (if (fboundp 'take)
      #'take
    (lambda (n list)
      (when (and (natnump n) (> n 0))
	(if (tramp-compat-length< list n)
	    list (butlast list (- (length list) n)))))))

;; Function `ntake' is new in Emacs 29.1.
(defalias 'tramp-compat-ntake
  (if (fboundp 'ntake)
      #'ntake
    (lambda (n list)
      (when (and (natnump n) (> n 0))
	(if (tramp-compat-length< list n)
	    list (nbutlast list (- (length list) n)))))))

;; Function `string-equal-ignore-case' is new in Emacs 29.1.
(defalias 'tramp-compat-string-equal-ignore-case
  (if (fboundp 'string-equal-ignore-case)
      #'string-equal-ignore-case
    (lambda (string1 string2)
      (eq t (compare-strings string1 nil nil string2 nil nil t)))))

;; Function `auth-source-netrc-parse-all' is new in Emacs 29.1.
;; `netrc-parse' has been obsoleted in parallel.
(defalias 'tramp-compat-auth-source-netrc-parse-all
  (if (fboundp 'auth-source-netrc-parse-all)
      #'auth-source-netrc-parse-all
    (lambda (&optional file)
      (declare-function netrc-parse "netrc")
      (autoload 'netrc-parse "netrc")
      (netrc-parse file))))

;; User option `password-colon-equivalents' is new in Emacs 30.1.
(if (boundp 'password-colon-equivalents)
    (defvaralias
      'tramp-compat-password-colon-equivalents
      'password-colon-equivalents)
  (defvar tramp-compat-password-colon-equivalents
    '(?\N{COLON}
      ?\N{FULLWIDTH COLON}
      ?\N{SMALL COLON}
      ?\N{PRESENTATION FORM FOR VERTICAL COLON}
      ?\N{KHMER SIGN CAMNUC PII KUUH})
    "List of characters equivalent to trailing colon in \"password\" prompts."))

(dolist (elt (all-completions "tramp-compat-" obarray 'functionp))
  (put (intern elt) 'tramp-suppress-trace t))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

(provide 'tramp-compat)

;;; TODO:
;;
;; * Starting with Emacs 27.1, there's no need to escape open
;;   parentheses with a backslash in docstrings anymore.

;;; tramp-compat.el ends here
