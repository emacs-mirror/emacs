;;; tramp-compat.el --- Tramp compatibility functions  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2017 Free Software Foundation, Inc.

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

;; Tramp's main Emacs version for development is Emacs 27.  This
;; package provides compatibility functions for Emacs 24, Emacs 25 and
;; Emacs 26.

;;; Code:

(require 'auth-source)
(require 'advice)
(require 'cl-lib)
(require 'custom)
(require 'format-spec)
(require 'parse-time)
(require 'password-cache)
(require 'shell)
(require 'timer)
(require 'ucs-normalize)

(require 'trampver)
(require 'tramp-loaddefs)

;; For not existing functions, obsolete functions, or functions with a
;; changed argument list, there are compiler warnings.  We want to
;; avoid them in cases we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  "Call FUNCTION if it exists.  Do not raise compiler warnings."
  `(when (functionp ,function)
     (with-no-warnings (funcall ,function ,@arguments))))

(defsubst tramp-compat-temporary-file-directory ()
  "Return name of directory for temporary files.
It is the default value of `temporary-file-directory'."
  ;; We must return a local directory.  If it is remote, we could run
  ;; into an infloop.
  (eval (car (get 'temporary-file-directory 'standard-value))))

(defsubst tramp-compat-make-temp-file (f &optional dir-flag)
  "Create a local temporary file (compat function).
Add the extension of F, if existing."
  (let* (file-name-handler-alist
	 (prefix (expand-file-name
		  (symbol-value 'tramp-temp-name-prefix)
		  (tramp-compat-temporary-file-directory)))
	 (extension (file-name-extension f t)))
    (make-temp-file prefix dir-flag extension)))

;; `temporary-file-directory' as function is introduced with Emacs 26.1.
(defalias 'tramp-compat-temporary-file-directory-function
  (if (fboundp 'temporary-file-directory)
      'temporary-file-directory
    'tramp-handle-temporary-file-directory))

(defun tramp-compat-process-running-p (process-name)
  "Returns t if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (cond
     ;; GNU Emacs 22 on w32.
     ((fboundp 'w32-window-exists-p)
      (tramp-compat-funcall 'w32-window-exists-p process-name process-name))

     ;; GNU Emacs 23.
     ((and (fboundp 'list-system-processes) (fboundp 'process-attributes))
      (let (result)
	(dolist (pid (tramp-compat-funcall 'list-system-processes) result)
	  (let ((attributes (process-attributes pid)))
	    (when (and (string-equal
                        (cdr (assoc 'user attributes)) (user-login-name))
                       (let ((comm (cdr (assoc 'comm attributes))))
                         ;; The returned command name could be truncated
                         ;; to 15 characters.  Therefore, we cannot check
                         ;; for `string-equal'.
                         (and comm (string-match
                                    (concat "^" (regexp-quote comm))
                                    process-name))))
	      (setq result t)))))))))

;; `user-error' has appeared in Emacs 24.3.
(defsubst tramp-compat-user-error (vec-or-proc format &rest args)
  "Signal a pilot error."
  (apply
   'tramp-error vec-or-proc
   (if (fboundp 'user-error) 'user-error 'error) format args))

;; `default-toplevel-value' has been declared in Emacs 24.4.
(unless (fboundp 'default-toplevel-value)
  (defalias 'default-toplevel-value 'symbol-value))

;; `file-attribute-*' are introduced in Emacs 25.1.

(if (fboundp 'file-attribute-type)
    (defalias 'tramp-compat-file-attribute-type 'file-attribute-type)
  (defsubst tramp-compat-file-attribute-type (attributes)
    "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
    (nth 0 attributes)))

(if (fboundp 'file-attribute-link-number)
    (defalias 'tramp-compat-file-attribute-link-number
      'file-attribute-link-number)
  (defsubst tramp-compat-file-attribute-link-number (attributes)
    "Return the number of links in ATTRIBUTES returned by `file-attributes'."
    (nth 1 attributes)))

(if (fboundp 'file-attribute-user-id)
    (defalias 'tramp-compat-file-attribute-user-id 'file-attribute-user-id)
  (defsubst tramp-compat-file-attribute-user-id (attributes)
    "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
    (nth 2 attributes)))

(if (fboundp 'file-attribute-group-id)
    (defalias 'tramp-compat-file-attribute-group-id 'file-attribute-group-id)
  (defsubst tramp-compat-file-attribute-group-id (attributes)
    "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
    (nth 3 attributes)))

(if (fboundp 'file-attribute-modification-time)
    (defalias 'tramp-compat-file-attribute-modification-time
      'file-attribute-modification-time)
  (defsubst tramp-compat-file-attribute-modification-time (attributes)
    "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a list of integers (HIGH LOW USEC PSEC) in the same style
as (current-time)."
    (nth 5 attributes)))

(if (fboundp 'file-attribute-size)
    (defalias 'tramp-compat-file-attribute-size 'file-attribute-size)
  (defsubst tramp-compat-file-attribute-size (attributes)
    "The size (in bytes) in ATTRIBUTES returned by `file-attributes'.
This is a floating point number if the size is too large for an integer."
    (nth 7 attributes)))

(if (fboundp 'file-attribute-modes)
    (defalias 'tramp-compat-file-attribute-modes 'file-attribute-modes)
  (defsubst tramp-compat-file-attribute-modes (attributes)
    "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
    (nth 8 attributes)))

;; `format-message' is new in Emacs 25.1.
(unless (fboundp 'format-message)
  (defalias 'format-message 'format))

;; `directory-name-p' is new in Emacs 25.1.
(if (fboundp 'directory-name-p)
    (defalias 'tramp-compat-directory-name-p 'directory-name-p)
  (defsubst tramp-compat-directory-name-p (name)
    "Return non-nil if NAME ends with a directory separator character."
    (let ((len (length name))
          (lastc ?.))
      (if (> len 0)
          (setq lastc (aref name (1- len))))
      (or (= lastc ?/)
          (and (memq system-type '(windows-nt ms-dos))
               (= lastc ?\\))))))

;; `file-missing' is introduced in Emacs 26.1.
(defconst tramp-file-missing
  (if (get 'file-missing 'error-conditions) 'file-missing 'file-error)
  "The error symbol for the `file-missing' error.")

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

;; `file-name-quoted-p', `file-name-quote' and `file-name-unquote' are
;; introduced in Emacs 26.
(eval-and-compile
  (if (fboundp 'file-name-quoted-p)
      (defalias 'tramp-compat-file-name-quoted-p 'file-name-quoted-p)
    (defsubst tramp-compat-file-name-quoted-p (name)
      "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name, check the local part of NAME."
      (string-match "^/:" (or (file-remote-p name 'localname) name))))

  (if (fboundp 'file-name-quote)
      (defalias 'tramp-compat-file-name-quote 'file-name-quote)
    (defsubst tramp-compat-file-name-quote (name)
      "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name, the local part of NAME is quoted."
      (concat
       (file-remote-p name) "/:" (or (file-remote-p name 'localname) name))))

  (if (fboundp 'file-name-unquote)
      (defalias 'tramp-compat-file-name-unquote 'file-name-unquote)
    (defsubst tramp-compat-file-name-unquote (name)
      "Remove quotation prefix \"/:\" from file NAME.
If NAME is a remote file name, the local part of NAME is unquoted."
      (save-match-data
	(let ((localname (or (file-remote-p name 'localname) name)))
	  (when (tramp-compat-file-name-quoted-p localname)
	    (setq
	     localname
	     (replace-match
	      (if (= (length localname) 2) "/" "") nil t localname)))
	  (concat (file-remote-p name) localname))))))

;; `tramp-syntax' has changed its meaning in Emacs 26.  We still
;; support old settings.
(defsubst tramp-compat-tramp-syntax ()
  "Return proper value of `tramp-syntax'."
  (cond ((eq tramp-syntax 'ftp) 'default)
	((eq tramp-syntax 'sep) 'separate)
	(t tramp-syntax)))

;; `cl-struct-slot-info' has been introduced with Emacs 25.
(defmacro tramp-compat-tramp-file-name-slots ()
  (if (fboundp 'cl-struct-slot-info)
      `(cdr (mapcar 'car (cl-struct-slot-info 'tramp-file-name)))
    `(cdr (mapcar 'car (get 'tramp-file-name 'cl-struct-slots)))))

(provide 'tramp-compat)

;;; TODO:

;;; tramp-compat.el ends here
