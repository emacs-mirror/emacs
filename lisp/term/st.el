;;; st.el --- terminal initialization for st  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;;; Commentary:

;; Support for the st terminal emulator.
;; https://st.suckless.org/

;;; Code:

(require 'term/xterm)

(defcustom xterm-st-extra-capabilities '( modifyOtherKeys getSelection
                                          setSelection)
  "Extra capabilities supported under \"stterm\".
Some features of screen depend on the terminal emulator in which
it runs, which can change when the screen session is moved to another tty."
  :version "28.1"
  :type xterm--extra-capabilities-type
  :group 'xterm)

(defun terminal-init-st ()
  "Terminal initialization function for st."
  ;; Using `check' leads to a two-second timeout.
  (let ((xterm-extra-capabilities xterm-st-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "xterm")))

(provide 'term/st)

;;; st.el ends here
