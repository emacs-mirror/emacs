;;; This function is used by the patch files to update Emacs releases.

(defun batch-byte-recompile-emacs ()
  "Recompile the Emacs `lisp' directory.
This is used after installing the patches for a new version."
  (let ((load-path (list (expand-file-name "lisp"))))
    (byte-recompile-directory "lisp")))
