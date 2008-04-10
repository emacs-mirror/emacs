;;; patcomp.el --- used by patch files to update Emacs releases -*- no-byte-compile: t -*-

;; This file is part of GNU Emacs.

;;; Commentary:

;;; Code:

(defun batch-byte-recompile-emacs ()
  "Recompile the Emacs `lisp' directory.
This is used after installing the patches for a new version."
  (let ((load-path (list (expand-file-name "lisp"))))
    (byte-recompile-directory "lisp")))

(defun batch-byte-compile-emacs ()
  "Compile new files installed in the Emacs `lisp' directory.
This is used after installing the patches for a new version.
It uses the command line arguments to specify the files to compile."
  (let ((load-path (list (expand-file-name "lisp"))))
    (batch-byte-compile)))

;; arch-tag: cb299b78-1d6c-4c02-945b-12fa2e856d6f
;;; patcomp.el ends here
