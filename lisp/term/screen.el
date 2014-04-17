;;; screen.el --- terminal initialization for screen and tmux  -*- lexical-binding: t -*-
;; Copyright (C) 1995, 2001-2014 Free Software Foundation, Inc.

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Treat a screen terminal similar to an xterm.
  (tty-run-terminal-initialization (selected-frame) "xterm"))

;; screen.el ends here
