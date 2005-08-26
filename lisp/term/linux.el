;; -*- no-byte-compile: t -*-
;; The Linux console handles Latin-1 by default.

(defun terminal-init-linux ()
  "Terminal initialization function for linux."  
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'iso-latin-1))

  ;; It can't really display underlines.
  (tty-no-underline)

  ;; Make Latin-1 input characters work, too.
  ;; Meta will continue to work, because the kernel
  ;; turns that into Escape.

  (let ((value (current-input-mode)))
    ;; The third arg only matters in that it is not t or nil.
    (set-input-mode (nth 0 value) (nth 1 value) 'iso-latin-1 (nth 3 value))))

;;; arch-tag: 5d0c4f63-739b-4862-abf3-041fe42adb8f
;;; linux.el ends here
