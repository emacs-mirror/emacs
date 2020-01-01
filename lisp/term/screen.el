;;; screen.el --- terminal initialization for screen and tmux  -*- lexical-binding: t -*-
;; Copyright (C) 1995, 2001-2020 Free Software Foundation, Inc.

(require 'term/xterm)

(defcustom xterm-screen-extra-capabilities '(modifyOtherKeys)
  "Extra capabilities supported under \"screen\".
Some features of screen depend on the terminal emulator in which
it runs, which can change when the screen session is moved to another tty."
  :version "25.1"
  :type xterm--extra-capabilities-type
  :group 'xterm)

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Treat a screen terminal similar to an xterm, but don't use
  ;; xterm-extra-capabilities's `check' setting since that doesn't seem
  ;; to work so well (it depends too much on the surrounding terminal
  ;; emulator, which can change during the session, bug#20356).
  (let ((xterm-extra-capabilities xterm-screen-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "xterm")))

(provide 'term/screen)

;; screen.el ends here
