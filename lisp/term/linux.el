;;; linux.el  -*- lexical-binding:t -*-

;; The Linux console handles Latin-1 by default.

(defun terminal-init-linux ()
  "Terminal initialization function for linux."
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'iso-latin-1))

  ;; It can't really display underlines.
  (tty-no-underline)

  ;; Compositions confuse cursor movement.
  (setq-default auto-composition-mode "linux")

  ;; Ensure additional terminal setup is done when `gpm-mouse-mode' is
  ;; enabled.
  (ignore-errors (when gpm-mouse-mode (gpm-mouse-mode 1)))

  ;; Don't translate ESC TAB to backtab as directed by ncurses-6.3.
  (define-key input-decode-map "\e\t" nil)

  ;; Make Latin-1 input characters work, too.
  ;; Meta will continue to work, because the kernel turns that into Escape.

  ;; The arg only matters in that it is not t or nil.
  (set-input-meta-mode 'iso-latin-1))

(provide 'term/linux)

;;; linux.el ends here
