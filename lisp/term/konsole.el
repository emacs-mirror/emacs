;;; konsole.el --- terminal initialization for konsole  -*- lexical-binding:t -*-
;; Copyright (C) 2017-2022 Free Software Foundation, Inc.

(require 'term/xterm)

(defun terminal-init-konsole ()
  "Terminal initialization function for konsole."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(provide 'term/konsole)

;;; konsole.el ends here
