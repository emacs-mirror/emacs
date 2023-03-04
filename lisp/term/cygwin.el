;;; cygwin.el --- support for the Cygwin terminal  -*- lexical-binding:t -*-

;;; The Cygwin terminal can't really display underlines.

(defun terminal-init-cygwin ()
  "Terminal initialization function for cygwin."
  (tty-no-underline))

(provide 'term/cygwin)

;;; cygwin.el ends here
