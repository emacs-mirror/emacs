;;; kmscon.el  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

(require 'term/xterm)

(defun terminal-init-kmscon ()
  "Terminal initialization function for kmscon."

  ;; kmscon supports 24-bit color mode.
  (xterm-register-default-colors xterm-standard-colors))

(provide 'term/kmscon)

;;; kmscon.el ends here
