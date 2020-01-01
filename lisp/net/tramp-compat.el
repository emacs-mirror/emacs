;;; tramp-compat.el --- Tramp compatibility functions  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2020 Free Software Foundation, Inc.

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

;; In Emacs 24 and 25, `tramp-unload-file-name-handlers' is not
;; autoloaded.  So we declare it here in order to avoid recursive
;; load.  This will be overwritten in tramp.el.
(defun tramp-unload-file-name-handlers () ".")

(require 'auth-source)
(require 'format-spec)
(require 'ls-lisp)  ;; Due to `tramp-handle-insert-directory'.
(require 'parse-time)
(require 'shell)
(require 'subr-x)

(declare-function tramp-handle-temporary-file-directory "tramp")

;; For not existing functions, obsolete functions, or functions with a
;; changed argument list, there are compiler warnings.  We want to
;; avoid them in cases we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS if it exists.  Do not raise compiler warnings."
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
      #'temporary-file-directory
    #'tramp-handle-temporary-file-directory))

(defun tramp-compat-process-running-p (process-name)
  "Return t if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (cond
     ;; GNU Emacs 22 on w32.
     ((fboundp 'w32-window-exists-p)
      (tramp-compat-funcall 'w32-window-exists-p process-name process-name))

     ;; GNU Emacs 23+.
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
                         (and comm (string-match-p
                                    (concat "^" (regexp-quote comm))
                                    process-name))))
	      (setq result t)))))))))

;; `file-attribute-*' are introduced in Emacs 25.1.

(defalias 'tramp-compat-file-attribute-type
  (if (fboundp 'file-attribute-type)
      #'file-attribute-type
    (lambda (attributes)
      "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
      (nth 0 attributes))))

(defalias 'tramp-compat-file-attribute-link-number
  (if (fboundp 'file-attribute-link-number)
      #'file-attribute-link-number
    (lambda (attributes)
      "Return the number of links in ATTRIBUTES returned by `file-attributes'."
      (nth 1 attributes))))

(defalias 'tramp-compat-file-attribute-user-id
  (if (fboundp 'file-attribute-user-id)
      #'file-attribute-user-id
    (lambda (attributes)
      "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
      (nth 2 attributes))))

(defalias 'tramp-compat-file-attribute-group-id
  (if (fboundp 'file-attribute-group-id)
      #'file-attribute-group-id
    (lambda (attributes)
      "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
      (nth 3 attributes))))

(defalias 'tramp-compat-file-attribute-access-time
  (if (fboundp 'file-attribute-access-time)
      #'file-attribute-access-time
    (lambda (attributes)
      "The last access time in ATTRIBUTES returned by `file-attributes'.
This a Lisp timestamp in the style of `current-time'."
      (nth 4 attributes))))

(defalias 'tramp-compat-file-attribute-modification-time
  (if (fboundp 'file-attribute-modification-time)
      #'file-attribute-modification-time
    (lambda (attributes)
      "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the style of `current-time'."
      (nth 5 attributes))))

(defalias 'tramp-compat-file-attribute-status-change-time
  (if (fboundp 'file-attribute-status-change-time)
      #'file-attribute-status-change-time
    (lambda (attributes)
      "The status modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of last change to the file's attributes: owner
and group, access mode bits, etc., and is a Lisp timestamp in the
style of `current-time'."
      (nth 6 attributes))))

(defalias 'tramp-compat-file-attribute-size
  (if (fboundp 'file-attribute-size)
      #'file-attribute-size
    (lambda (attributes)
      "The size (in bytes) in ATTRIBUTES returned by `file-attributes'.
If the size is too large for a fixnum, this is a bignum in Emacs 27
and later, and is a float in Emacs 26 and earlier."
      (nth 7 attributes))))

(defalias 'tramp-compat-file-attribute-modes
  (if (fboundp 'file-attribute-modes)
      #'file-attribute-modes
    (lambda (attributes)
      "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
      (nth 8 attributes))))

;; `format-message' is new in Emacs 25.1.
(unless (fboundp 'format-message)
  (defalias 'format-message #'format))

;; `directory-name-p' is new in Emacs 25.1.
(defalias 'tramp-compat-directory-name-p
  (if (fboundp 'directory-name-p)
      #'directory-name-p
    (lambda (name)
      "Return non-nil if NAME ends with a directory separator character."
      (let ((len (length name))
            (lastc ?.))
	(if (> len 0)
            (setq lastc (aref name (1- len))))
	(or (= lastc ?/)
            (and (memq system-type '(windows-nt ms-dos))
		 (= lastc ?\\)))))))

;; `file-missing' is introduced in Emacs 26.1.
(defconst tramp-file-missing
  (if (get 'file-missing 'error-conditions) 'file-missing 'file-error)
  "The error symbol for the `file-missing' error.")

;; `file-local-name', `file-name-quoted-p', `file-name-quote' and
;; `file-name-unquote' are introduced in Emacs 26.
(defalias 'tramp-compat-file-local-name
  (if (fboundp 'file-local-name)
      #'file-local-name
    (lambda (name)
      "Return the local name component of NAME.
It returns a file name which can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
      (or (file-remote-p name 'localname) name))))

;; `file-name-quoted-p' got a second argument in Emacs 27.1.
(defalias 'tramp-compat-file-name-quoted-p
  (if (and
       (fboundp 'file-name-quoted-p)
       (equal (tramp-compat-funcall 'func-arity #'file-name-quoted-p) '(1 . 2)))
      #'file-name-quoted-p
    (lambda (name &optional top)
      "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name and TOP is nil, check the local part of NAME."
      (let ((file-name-handler-alist (unless top file-name-handler-alist)))
	(string-prefix-p "/:" (tramp-compat-file-local-name name))))))

(defalias 'tramp-compat-file-name-quote
  (if (and
       (fboundp 'file-name-quote)
       (equal (tramp-compat-funcall 'func-arity #'file-name-quote) '(1 . 2)))
      #'file-name-quote
    (lambda (name &optional top)
      "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name and TOP is nil, the local part of NAME is quoted."
      (let ((file-name-handler-alist (unless top file-name-handler-alist)))
	(if (tramp-compat-file-name-quoted-p name top)
            name
	  (concat
	   (file-remote-p name) "/:" (tramp-compat-file-local-name name)))))))

(defalias 'tramp-compat-file-name-unquote
  (if (and
       (fboundp 'file-name-unquote)
       (equal (tramp-compat-funcall 'func-arity #'file-name-unquote) '(1 . 2)))
      #'file-name-unquote
    (lambda (name &optional top)
      "Remove quotation prefix \"/:\" from file NAME.
If NAME is a remote file name and TOP is nil, the local part of
NAME is unquoted."
      (let* ((file-name-handler-alist (unless top file-name-handler-alist))
             (localname (tramp-compat-file-local-name name)))
	(when (tramp-compat-file-name-quoted-p localname top)
	  (setq
	   localname (if (= (length localname) 2) "/" (substring localname 2))))
	(concat (file-remote-p name) localname)))))

;; `tramp-syntax' has changed its meaning in Emacs 26.  We still
;; support old settings.
(defsubst tramp-compat-tramp-syntax ()
  "Return proper value of `tramp-syntax'."
  (defvar tramp-syntax)
  (cond ((eq tramp-syntax 'ftp) 'default)
	((eq tramp-syntax 'sep) 'separate)
	(t tramp-syntax)))

;; `cl-struct-slot-info' has been introduced with Emacs 25.
(defmacro tramp-compat-tramp-file-name-slots ()
  "Return a list of slot names."
  (if (fboundp 'cl-struct-slot-info)
      '(cdr (mapcar #'car (cl-struct-slot-info 'tramp-file-name)))
    '(cdr (mapcar #'car (get 'tramp-file-name 'cl-struct-slots)))))

;; The signature of `tramp-make-tramp-file-name' has been changed.
;; Therefore, we cannot use `url-tramp-convert-url-to-tramp' prior
;; Emacs 26.1.  We use `temporary-file-directory' as indicator.
(defconst tramp-compat-use-url-tramp-p (fboundp 'temporary-file-directory)
  "Whether to use url-tramp.el.")

;; `exec-path' is new in Emacs 27.1.
(defalias 'tramp-compat-exec-path
  (if (fboundp 'exec-path)
      #'exec-path
    (lambda ()
      "List of directories to search programs to run in remote subprocesses."
      (let ((handler (find-file-name-handler default-directory 'exec-path)))
	(if handler
	    (funcall handler 'exec-path)
	  exec-path)))))

;; `time-equal-p' has appeared in Emacs 27.1.
(defalias 'tramp-compat-time-equal-p
  (if (fboundp 'time-equal-p)
      #'time-equal-p
    (lambda (t1 t2)
      "Return non-nil if time value T1 is equal to time value T2.
A nil value for either argument stands for the current time."
      (equal (or t1 (current-time)) (or t2 (current-time))))))

;; `flatten-tree' has appeared in Emacs 27.1.
(defalias 'tramp-compat-flatten-tree
  (if (fboundp 'flatten-tree)
      #'flatten-tree
    (lambda (tree)
      "Take TREE and \"flatten\" it."
      (let (elems)
	(setq tree (list tree))
	(while (let ((elem (pop tree)))
		 (cond ((consp elem)
			(setq tree (cons (car elem) (cons (cdr elem) tree))))
                       (elem
			(push elem elems)))
		 tree))
	(nreverse elems)))))

;; `progress-reporter-update' got argument SUFFIX in Emacs 27.1.
(defalias 'tramp-compat-progress-reporter-update
  (if (equal (tramp-compat-funcall 'func-arity #'progress-reporter-update)
	     '(1 . 3))
      #'progress-reporter-update
    (lambda (reporter &optional value _suffix)
      (progress-reporter-update reporter value))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

;;; TODO:
;;
;; * Starting with Emacs 25.1, replace `tramp-message-show-message' by
;;   the reverse of `inhibit-message'.

(provide 'tramp-compat)

;;; tramp-compat.el ends here
