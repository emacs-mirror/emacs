;;; konsole.el --- terminal initialization for konsole
;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

(require 'term/xterm)

(defun terminal-init-konsole ()
  "Terminal initialization function for konsole."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(provide 'term/konsole)

;; konsole.el ends here
