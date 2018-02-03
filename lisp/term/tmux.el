;;; tmux.el --- terminal initialization for tmux  -*- lexical-binding: t -*-
;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

(require 'term/xterm)

(defcustom xterm-tmux-extra-capabilities '(modifyOtherKeys)
  "Extra capabilities supported under \"tmux\".
Some features of tmux depend on the terminal emulator in which
it runs, which can change when the tmux session is moved to another tty."
  :version "26.1"
  :type xterm--extra-capabilities-type
  :group 'xterm)

(defun terminal-init-tmux ()
  "Terminal initialization function for tmux."
  ;; Treat a tmux terminal similar to an xterm, but don't use
  ;; xterm-extra-capabilities's `check' setting since that doesn't seem
  ;; to work so well (it depends too much on the surrounding terminal
  ;; emulator, which can change during the session, bug#20356).
  (let ((xterm-extra-capabilities xterm-tmux-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "xterm")))

(provide 'term/tmux)

;; tmux.el ends here
