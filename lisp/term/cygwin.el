;;; cygwin.el --- support for the Cygwin terminal -*- no-byte-compile: t -*-

;;; The Cygwin terminal can't really display underlines.

(defun terminal-init-cygwin ()
  "Terminal initialization function for cygwin."  
  (tty-no-underline))

;; arch-tag: ca81ce67-3c41-4883-a29b-4c3d64a21191
;;; cygwin.el ends here
