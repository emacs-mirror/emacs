;;; ghostty.el --- terminal initialization for Ghostty  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Support for Ghostty terminal.
;; https://ghostty.org/

(require 'term/xterm)

(defun terminal-init-ghostty ()
  "Terminal initialization function for Ghostty."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(provide 'term/ghostty)

;;; ghostty.el ends here
