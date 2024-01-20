;;; tramp-compat.el --- Tramp compatibility functions  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2024 Free Software Foundation, Inc.

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

;; Tramp's main Emacs version for development is Emacs 29.  This
;; package provides compatibility functions for Emacs 26, Emacs 27 and
;; Emacs 28.

;;; Code:

(require 'ansi-color)
(require 'auth-source)
(require 'format-spec)
(require 'parse-time)
(require 'shell)
(require 'subr-x)

(declare-function tramp-compat-rx "tramp")
(declare-function tramp-error "tramp")
(declare-function tramp-file-name-handler "tramp")
(declare-function tramp-tramp-file-p "tramp")
(defvar tramp-syntax)
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
  (declare (indent 1) (debug t))
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

;; `file-name-quoted-p', `file-name-quote' and `file-name-unquote' got
;; a second argument in Emacs 27.1.
;;;###tramp-autoload
(defalias 'tramp-compat-file-name-quoted-p
  (if (equal (func-arity #'file-name-quoted-p) '(1 . 2))
      #'file-name-quoted-p
    (lambda (name &optional top)
      "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name and TOP is nil, check the local part of NAME."
      (let ((file-name-handler-alist (unless top file-name-handler-alist)))
	(string-prefix-p "/:" (file-local-name name))))))

(defalias 'tramp-compat-file-name-quote
  (if (equal (func-arity #'file-name-quote) '(1 . 2))
      #'file-name-quote
    (lambda (name &optional top)
      "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name and TOP is nil, the local part of NAME is quoted."
      (let ((file-name-handler-alist (unless top file-name-handler-alist)))
	(if (tramp-compat-file-name-quoted-p name top)
            name
	  (concat (file-remote-p name) "/:" (file-local-name name)))))))

(defalias 'tramp-compat-file-name-unquote
  (if (equal (func-arity #'file-name-unquote) '(1 . 2))
      #'file-name-unquote
    (lambda (name &optional top)
      "Remove quotation prefix \"/:\" from file NAME.
If NAME is a remote file name and TOP is nil, the local part of
NAME is unquoted."
      (let* ((file-name-handler-alist (unless top file-name-handler-alist))
             (localname (file-local-name name)))
	(when (tramp-compat-file-name-quoted-p localname top)
	  (setq
	   localname
	   (if (tramp-compat-length= localname 2) "/" (substring localname 2))))
	(concat (file-remote-p name) localname)))))

;; `tramp-syntax' has changed its meaning in Emacs 26.1.  We still
;; support old settings.
(defsubst tramp-compat-tramp-syntax ()
  "Return proper value of `tramp-syntax'."
  (cond ((eq tramp-syntax 'ftp) 'default)
	((eq tramp-syntax 'sep) 'separate)
	(t tramp-syntax)))

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
      (if (tramp-tramp-file-p default-directory)
	  (tramp-file-name-handler 'exec-path)
	exec-path))))

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
  (if (equal (func-arity #'progress-reporter-update) '(1 . 3))
      #'progress-reporter-update
    (lambda (reporter &optional value _suffix)
      (progress-reporter-update reporter value))))

;; `ignore-error' is new in Emacs 27.1.
(defmacro tramp-compat-ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

;; `rx' in Emacs 26 doesn't know the `literal', `anychar' and
;; `multibyte' constructs.  The `not' construct requires an `any'
;; construct as argument.  The `regexp' construct requires a literal
;; string.
(defvar tramp-compat-rx--runtime-params)

(defun tramp-compat-rx--transform-items (items)
  (mapcar #'tramp-compat-rx--transform-item items))

;; There is an error in Emacs 26.  `(rx "a" (? ""))' => "a?".
;; We must protect the string in regexp and literal, therefore.
(defun tramp-compat-rx--transform-item (item)
  (pcase item
    ('anychar 'anything)
    ('multibyte 'nonascii)
    (`(not ,expr)
     (if (consp expr) item (list 'not (list 'any expr))))
    (`(regexp ,expr)
     (setq tramp-compat-rx--runtime-params t)
     `(regexp ,(list '\, `(concat "\\(?:" ,expr "\\)"))))
    (`(literal ,expr)
     (setq tramp-compat-rx--runtime-params t)
     `(regexp ,(list '\, `(concat "\\(?:" (regexp-quote ,expr) "\\)"))))
    (`(eval . ,_) item)
    (`(,head . ,rest) (cons head (tramp-compat-rx--transform-items rest)))
    (_ item)))

(defun tramp-compat-rx--transform (items)
  (let* ((tramp-compat-rx--runtime-params nil)
         (new-rx (cons ': (tramp-compat-rx--transform-items items))))
    (if tramp-compat-rx--runtime-params
        `(rx-to-string ,(list '\` new-rx) t)
      (rx-to-string new-rx t))))

(if (ignore-errors (rx-to-string '(literal "a"))) ;; Emacs 27+.
    (defalias 'tramp-compat-rx #'rx)
  (defmacro tramp-compat-rx (&rest items)
    (tramp-compat-rx--transform items)))

;; This is needed for compilation in the Emacs source tree.
;;;###autoload (defalias 'tramp-compat-rx #'rx)

(put #'tramp-compat-rx 'tramp-autoload t)

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
;;
;; * Starting with Emacs 27.1, there's `make-empty-file'.  Could be
;;   used instead of `(write-region "" ...)'.

;;; tramp-compat.el ends here
