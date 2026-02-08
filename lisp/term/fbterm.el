;;; fbterm.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Note that, in some versions of fbterm, the TERM environment
;; variable is set to "linux".  When that's the case, the code below
;; will not be executed, and only 8 colors will be available.  It is
;; therefore necessary, with these versions of fbterm, to set that
;; environment variable to "fbterm" to enable its 256 color mode
;; extension.  See also the node "Emacs in a Linux console" of the
;; Emacs FAQ.

(require 'term/xterm)

(defun terminal-init-fbterm ()
  "Terminal initialization function for fbterm."

  ;; fbterm can't display underlines, even though its terminfo data
  ;; says it can.
  (tty-no-underline)

  ;; fbterm supports xterm's 256 color mode extension.
  (xterm-register-default-colors xterm-standard-colors))

(provide 'term/fbterm)

;;; fbterm.el ends here
