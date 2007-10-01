;;; tramp-gw.el --- Tramp compatibility functions

;; Copyright (C) 2007 Free Software Foundation, Inc.

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

;; Pacify byte-compiler
(eval-when-compile
  (require 'cl)
  (require 'custom))

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
;(eval-when-compile
;  (when (featurep 'xemacs)
;    (byte-compiler-options (warnings (- unused-vars)))))

;; `last-coding-system-used' is unknown in XEmacs.
(eval-when-compile
  (unless (boundp 'last-coding-system-used)
    (defvar last-coding-system-used nil)))

;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
;; used in XEmacs, so we set it here and there.  The following is needed
;; to pacify Emacs byte-compiler.
(eval-when-compile
  (unless (boundp 'byte-compile-not-obsolete-var)
    (defvar byte-compile-not-obsolete-var nil))
  (setq byte-compile-not-obsolete-var 'directory-sep-char))

;; `with-temp-message' does not exists in XEmacs.
(eval-and-compile
  (condition-case nil
      (with-temp-message (current-message) nil)
    (error (defmacro with-temp-message (message &rest body) `(progn ,@body)))))

;; `set-buffer-multibyte' comes from Emacs Leim.
(eval-and-compile
  (unless (fboundp 'set-buffer-multibyte)
    (defalias 'set-buffer-multibyte 'ignore)))

;; `font-lock-add-keywords' does not exist in XEmacs.
(eval-and-compile
  (unless (fboundp 'font-lock-add-keywords)
    (defalias 'font-lock-add-keywords 'ignore)))

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
   ((boundp 'temporary-file-directory)
    (symbol-value 'temporary-file-directory))
   ((fboundp 'temp-directory)
    (funcall (symbol-function 'temp-directory))) ;pacify byte-compiler
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

;; `most-positive-fixnum' arrived in Emacs 22.
(defsubst tramp-compat-most-positive-fixnum ()
  "Return largest positive integer value (compat function)."
  (cond ((boundp 'most-positive-fixnum)
         (symbol-value 'most-positive-fixnum))
	(t 134217727)))

;; ID-FORMAT exists since Emacs 22.
(defun tramp-compat-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files (compat function)."
  (cond
   ((or (null id-format) (eq id-format 'integer))
    (file-attributes filename))
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

;; `copy-tree' is introduced with Emacs 22.  We've adapted the
;; implementation from Emacs 23.
(defun tramp-compat-copy-tree (tree)
  "Make a copy of TREE (compat function)."
  (if (functionp 'copy-tree)
      (funcall (symbol-function 'copy-tree) tree)
    (let (result)
      (while (consp tree)
	(let ((newcar (car tree)))
	  (if (consp (car tree))
	      (setq newcar (tramp-compat-copy-tree (car tree))))
	  (push newcar result))
	(setq tree (cdr tree)))
      (nconc (nreverse result) tree))))

(eval-and-compile
  (unless (fboundp 'file-remote-p)
    (defalias 'file-remote-p 'tramp-handle-file-remote-p))

  (unless (fboundp 'process-file)
    (defalias 'process-file 'tramp-handle-process-file))

  (unless (fboundp 'start-file-process)
    (defalias 'start-file-process 'tramp-handle-start-file-process))

  (unless (fboundp 'set-file-times)
    (defalias 'set-file-times 'tramp-handle-set-file-times)))

(provide 'tramp-compat)

;;; TODO:

;; arch-tag: 0e724b18-6699-4f87-ad96-640b272e5c85
;;; tramp-compat.el ends here
