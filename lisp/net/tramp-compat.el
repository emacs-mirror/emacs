;;; tramp-compat.el --- Tramp compatibility functions  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

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
;; package provides compatibility functions for Emacs 28, Emacs 29 and
;; Emacs 30.

;;; Code:

(require 'tramp-loaddefs nil t)  ; guard against load during autoload gen
(require 'ansi-color)
(require 'auth-source)
(require 'format-spec)
(require 'parse-time)
(require 'shell)
(require 'xdg)

(declare-function tramp-error "tramp-message")
(declare-function tramp-warning "tramp-message")
(declare-function tramp-tramp-file-p "tramp")
(defvar tramp-temp-name-prefix)

(defconst tramp-compat-emacs-compiled-version (eval-when-compile emacs-version)
  "The Emacs version used for compilation.")

(with-eval-after-load 'tramp
  (unless (= emacs-major-version
	     (car (version-to-list tramp-compat-emacs-compiled-version)))
    (tramp-warning nil
      "Tramp has been compiled with Emacs %s, this is Emacs %s"
      tramp-compat-emacs-compiled-version emacs-version))

  (with-eval-after-load 'docker-tramp
    (tramp-warning nil
      (concat "Package `docker-tramp' has been obsoleted, "
	      "please use integrated package `tramp-container'")))

  (with-eval-after-load 'kubernetes-tramp
    (tramp-warning nil
      (concat "Package `kubernetes-tramp' has been obsoleted, "
	      "please use integrated package `tramp-container'")))

  (with-eval-after-load 'tramp-nspawn
    (tramp-warning nil
      (concat "Package `tramp-nspawn' has been obsoleted, "
	      "please use integrated package `tramp-container'"))))

;; For not existing functions, obsolete functions, or functions with a
;; changed argument list, there are compiler warnings.  We want to
;; avoid them in cases we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS if it exists.  Do not raise compiler warnings."
  (declare (indent 1) (debug t))
  `(when (functionp ,function)
     (with-no-warnings (funcall ,function ,@arguments))))

;; We must use a local directory.  If it is remote, we could run into
;; an infloop.  We try to follow the XDG specification, for security reasons.
(defconst tramp-compat-temporary-file-directory
  (file-name-as-directory
   (if-let* ((xdg (xdg-cache-home))
	     ((file-directory-p xdg))
	     ((file-writable-p xdg)))
       (prog1 (setq xdg (expand-file-name "emacs" xdg))
	 (make-directory xdg t))
     (eval (car (get 'temporary-file-directory 'standard-value)) t)))
  "The default value of `temporary-file-directory' for Tramp.")

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

;; `permission-denied' is introduced in Emacs 29.1.
(defconst tramp-permission-denied
  (if (get 'permission-denied 'error-conditions) 'permission-denied 'file-error)
  "The error symbol for the `permission-denied' error.")

(defsubst tramp-compat-permission-denied (vec file)
  "Emit the `permission-denied' error."
  (if (get 'permission-denied 'error-conditions)
      (tramp-error vec tramp-permission-denied file)
    (tramp-error vec tramp-permission-denied "Permission denied: %s" file)))

;; Function `auth-info-password' is new in Emacs 29.1.  Finally,
;; Bug#49289 is fixed in Emacs 30.1 for the `secrets' and `plstore'
;; auth-sources backends.
(defalias 'tramp-compat-auth-info-password
  (if (>= emacs-major-version 30)
      'auth-info-password
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
	(if (length< list n)
	    list (butlast list (- (length list) n)))))))

;; Function `ntake' is new in Emacs 29.1.
(defalias 'tramp-compat-ntake
  (if (fboundp 'ntake)
      #'ntake
    (lambda (n list)
      (when (and (natnump n) (> n 0))
	(if (length< list n)
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

;; Function `seq-keep' is new in Emacs 29.1.
(defalias 'tramp-compat-seq-keep
  (if (fboundp 'seq-keep)
      #'seq-keep
    (lambda (function sequence)
      (delq nil (seq-map function sequence)))))

;; User option `connection-local-default-application' is new in Emacs 29.1.
(unless (boundp 'connection-local-default-application)
  (defvar connection-local-default-application 'tramp
    "Default application in connection-local functions, a symbol.
This variable must not be changed globally."))

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

;; Macros `connection-local-p' and `connection-local-value' are new in
;; Emacs 30.1.
(if (macrop 'connection-local-p)
    (defalias 'tramp-compat-connection-local-p 'connection-local-p)
  (defmacro tramp-compat-connection-local-p (variable &optional application)
    "Non-nil if VARIABLE has a connection-local binding in `default-directory'.
`default-directory' must be a remote file name.
If APPLICATION is nil, the value of
`connection-local-default-application' is used."
    (declare (debug (symbolp &optional form)))
    (unless (symbolp variable)
      (signal 'wrong-type-argument (list 'symbolp variable)))
    `(let* ((connection-local-default-application
	     (or ,application connection-local-default-application))
	    (criteria (connection-local-criteria-for-default-directory))
            connection-local-variables-alist file-local-variables-alist)
       (when criteria
	 (hack-connection-local-variables criteria)
	 (and (assq ',variable connection-local-variables-alist) t)))))

(if (macrop 'connection-local-value)
    (defalias 'tramp-compat-connection-local-value 'connection-local-value)
  (defmacro tramp-compat-connection-local-value (variable &optional application)
    "Return connection-local VARIABLE for APPLICATION in `default-directory'.
`default-directory' must be a remote file name.
If APPLICATION is nil, the value of
`connection-local-default-application' is used.
If VARIABLE does not have a connection-local binding, the return
value is the default binding of the variable."
    (declare (debug (symbolp &optional form)))
    (unless (symbolp variable)
      (signal 'wrong-type-argument (list 'symbolp variable)))
    `(let* ((connection-local-default-application
	     (or ,application connection-local-default-application))
	    (criteria (connection-local-criteria-for-default-directory))
            connection-local-variables-alist file-local-variables-alist)
       (if (not criteria)
           ,variable
	 (hack-connection-local-variables criteria)
	 (if-let* ((result (assq ',variable connection-local-variables-alist)))
             (cdr result)
           ,variable)))))

(dolist (elt (all-completions "tramp-compat-" obarray #'functionp))
  (function-put (intern elt) 'tramp-suppress-trace t))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

(provide 'tramp-compat)

;;; TODO:
;;
;; * Starting with Emacs 27.1, there's no need to escape open
;;   parentheses with a backslash in docstrings anymore.  However,
;;   `outline-minor-mode' has still problems with this.  Since there
;;   are developers using `outline-minor-mode' in Lisp files, we still
;;   keep this quoting.
;;
;; * Use `with-environment-variables'.
;;
;; * Use `ensure-list'.
;;
;; * Starting with Emacs 29.1, use `buffer-match-p' and `match-buffers'.
;;
;; * Starting with Emacs 29.1, use `string-split'.
;;
;; * Starting with Emacs 30.1, there is `handler-bind'.  Use it
;;   instead of `condition-case' when the origin of an error shall be
;;   kept, for example when the HANDLER propagates the error with
;;   `(signal (car err) (cdr err)'.
;;
;; * Starting with Emacs 30.1, use '(_ VALUEFORM)' instead of
;;   '(VALUEFORM)' in 'if-let*/when-let*/and-let*'.

;;; tramp-compat.el ends here
