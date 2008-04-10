;; -*- no-byte-compile: t -*-
;; The Linux console handles Latin-1 by default.

(defun terminal-init-linux ()
  "Terminal initialization function for linux."  
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'iso-latin-1))

  ;; It can't really display underlines.
  (tty-no-underline)

  (condition-case nil (t-mouse-mode 1)
    (error nil))

  ;; Make Latin-1 input characters work, too.
  ;; Meta will continue to work, because the kernel
  ;; turns that into Escape.

  ;; The arg only matters in that it is not t or nil.
  (set-input-meta-mode 'iso-latin-1))

;; arch-tag: 5d0c4f63-739b-4862-abf3-041fe42adb8f
;;; linux.el ends here
