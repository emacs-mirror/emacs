;;; tramp-compat.el --- Tramp compatibility functions

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tramp's main Emacs version for development is GNU Emacs 23.  This
;; package provides compatibility functions for GNU Emacs 21, GNU
;; Emacs 22 and XEmacs 21.4+.

;;; Code:

(eval-when-compile

  ;; Pacify byte-compiler.
  (require 'cl))

(eval-and-compile

  (require 'custom)

  ;; Load the appropriate timer package.
  (if (featurep 'xemacs)
      (require 'timer-funcs)
    (require 'timer))

  ;; tramp-util offers integration into other (X)Emacs packages like
  ;; compile.el, gud.el etc.  Not necessary in Emacs 23.
  (eval-after-load "tramp"
    ;; We check whether `start-file-process' is an alias.
    '(when (or (not (fboundp 'start-file-process))
	       (symbolp (symbol-function 'start-file-process)))
       (require 'tramp-util)
       (add-hook 'tramp-unload-hook
		 '(lambda ()
		    (when (featurep 'tramp-util)
		      (unload-feature 'tramp-util 'force))))))

  ;; Make sure that we get integration with the VC package.  When it
  ;; is loaded, we need to pull in the integration module.  Not
  ;; necessary in Emacs 23.
  (eval-after-load "vc"
    (eval-after-load "tramp"
      ;; We check whether `start-file-process' is an alias.
      '(when (or (not (fboundp 'start-file-process))
		 (symbolp (symbol-function 'start-file-process)))
	 (require 'tramp-vc)
	 (add-hook 'tramp-unload-hook
		   '(lambda ()
		      (when (featurep 'tramp-vc)
			(unload-feature 'tramp-vc 'force)))))))

  ;; Avoid byte-compiler warnings if the byte-compiler supports this.
  ;; Currently, XEmacs supports this.
  (when (featurep 'xemacs)
    (unless (boundp 'byte-compile-default-warnings)
      (defvar byte-compile-default-warnings nil))
    (delq 'unused-vars byte-compile-default-warnings))

  ;; `last-coding-system-used' is unknown in XEmacs.
  (unless (boundp 'last-coding-system-used)
    (defvar last-coding-system-used nil))

  ;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
  ;; used in XEmacs, so we set it here and there.  The following is
  ;; needed to pacify Emacs byte-compiler.
  (unless (boundp 'byte-compile-not-obsolete-var)
    (defvar byte-compile-not-obsolete-var nil))
  (setq byte-compile-not-obsolete-var 'directory-sep-char)

  ;; `with-temp-message' does not exists in XEmacs.
  (condition-case nil
      (with-temp-message (current-message) nil)
    (error (defmacro with-temp-message (message &rest body) `(progn ,@body))))

  ;; `set-buffer-multibyte' comes from Emacs Leim.
  (unless (fboundp 'set-buffer-multibyte)
    (defalias 'set-buffer-multibyte 'ignore))

  ;; `font-lock-add-keywords' does not exist in XEmacs.
  (unless (fboundp 'font-lock-add-keywords)
    (defalias 'font-lock-add-keywords 'ignore))

  ;; `file-remote-p' has been introduced with Emacs 22.  The version
  ;; of XEmacs is not a magic file name function (yet); this is
  ;; corrected in tramp-util.el.  Here it is sufficient if the
  ;; function exists.
  (unless (fboundp 'file-remote-p)
    (defalias 'file-remote-p 'tramp-handle-file-remote-p))

  ;; `process-file' exists since Emacs 22.
  (unless (fboundp 'process-file)
    (defalias 'process-file 'tramp-handle-process-file))

  ;; `start-file-process' is new in Emacs 23.
  (unless (fboundp 'start-file-process)
    (defalias 'start-file-process 'tramp-handle-start-file-process))

  ;; `set-file-times' is also new in Emacs 23.
  (unless (fboundp 'set-file-times)
    (defalias 'set-file-times 'tramp-handle-set-file-times)))

(defsubst tramp-compat-line-end-position ()
  "Return point at end of line (compat function).
Calls `line-end-position' or `point-at-eol' if defined, else
own implementation."
  (cond
   ((fboundp 'line-end-position) (funcall (symbol-function 'line-end-position)))
   ((fboundp 'point-at-eol) 	 (funcall (symbol-function 'point-at-eol)))
   (t (save-excursion (end-of-line) (point)))))

(defsubst tramp-compat-temporary-file-directory ()
  "Return name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond
   ((boundp 'temporary-file-directory) (symbol-value 'temporary-file-directory))
   ((fboundp 'temp-directory) (funcall (symbol-function 'temp-directory)))
   ((let ((d (getenv "TEMP"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TEMP")))
   ((let ((d (getenv "TMP"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TMP")))
   ((let ((d (getenv "TMPDIR"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TMPDIR")))
   ((file-exists-p "c:/temp") (file-name-as-directory "c:/temp"))
   (t (message (concat "Neither `temporary-file-directory' nor "
		       "`temp-directory' is defined -- using /tmp."))
      (file-name-as-directory "/tmp"))))

;; `make-temp-file' exists in Emacs only.  The third parameter SUFFIX
;; has been introduced with Emacs 22.  We try it, if it fails, we fall
;; back to `make-temp-name', creating the temporary file immediately
;; in order to avoid a security hole.
(defsubst tramp-compat-make-temp-file (filename)
  "Create a temporary file (compat function).
Add the extension of FILENAME, if existing."
  (let ((prefix (expand-file-name
		 (symbol-value 'tramp-temp-name-prefix)
		 (tramp-compat-temporary-file-directory)))
	(extension (file-name-extension filename t))
	result)
    (condition-case nil
	(setq result
	      (funcall (symbol-function 'make-temp-file) prefix nil extension))
      (error
       ;; We use our own implementation, taken from files.el.
       (while
	   (condition-case ()
	       (progn
		 (setq result (concat (make-temp-name prefix) extension))
		 (write-region
		  "" nil result nil 'silent nil
		  ;; 7th parameter is MUSTBENEW in Emacs, and
		  ;; CODING-SYSTEM in XEmacs.  It is not a security
		  ;; hole in XEmacs if we cannot use this parameter,
		  ;; because XEmacs uses a user-specific subdirectory
		  ;; with 0700 permissions.
		  (when (not (featurep 'xemacs)) 'excl))
		 nil)
	     (file-already-exists t))
	 ;; The file was somehow created by someone else between
	 ;; `make-temp-name' and `write-region', let's try again.
	 nil)))
    result))

;; `most-positive-fixnum' arrived in Emacs 22.  Before, and in XEmacs,
;; it is a fixed value.
(defsubst tramp-compat-most-positive-fixnum ()
  "Return largest positive integer value (compat function)."
  (cond
   ((boundp 'most-positive-fixnum) (symbol-value 'most-positive-fixnum))
   ;; Default value in XEmacs and Emacs 21.
   (t 134217727)))

;; ID-FORMAT exists since Emacs 22.
(defun tramp-compat-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files (compat function)."
  (cond
   ((or (null id-format) (eq id-format 'integer))
    (file-attributes filename))
   ;; FIXME: shouldn't that be tramp-file-p or somesuch?
   ((file-remote-p filename)
    (funcall (symbol-function 'tramp-handle-file-attributes)
	     filename id-format))
   (t (condition-case nil
	  (funcall (symbol-function 'file-attributes) filename id-format)
	(error (file-attributes filename))))))

;; PRESERVE-UID-GID has been introduced with Emacs 23.  It does not
;; hurt to ignore it for other (X)Emacs versions.
(defun tramp-compat-copy-file
  (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Like `copy-file' for Tramp files (compat function)."
  (if preserve-uid-gid
      (funcall
       (symbol-function 'copy-file)
       filename newname ok-if-already-exists keep-date preserve-uid-gid)
    (copy-file filename newname ok-if-already-exists keep-date)))

;; `copy-tree' is a built-in function in XEmacs.  In Emacs 21, it is
;; an auoloaded function in cl-extra.el.  Since Emacs 22, it is part
;; of subr.el.  There are problems when autoloading, therefore we test
;; for for `subrp' and `symbol-file'.  Implementation is taken from Emacs23.
(defun tramp-compat-copy-tree (tree)
  "Make a copy of TREE (compat function)."
  (if (or (subrp 'copy-tree) (symbol-file 'copy-tree))
      (funcall (symbol-function 'copy-tree) tree)
    (let (result)
      (while (consp tree)
	(let ((newcar (car tree)))
	  (if (consp (car tree))
	      (setq newcar (tramp-compat-copy-tree (car tree))))
	  (push newcar result))
	(setq tree (cdr tree)))
      (nconc (nreverse result) tree))))

(provide 'tramp-compat)

;;; TODO:

;; arch-tag: 0e724b18-6699-4f87-ad96-640b272e5c85
;;; tramp-compat.el ends here
