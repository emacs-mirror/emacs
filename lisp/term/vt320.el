;; -*- no-byte-compile: t -*-
(defun terminal-init-vt320 ()
  "Terminal initialization function for vt320."
  (terminal-init-vt100)
  ;; Make F11 an escape key.
  (define-key local-function-key-map "\e[23~" [?\e]))

;;; arch-tag: f9f4c954-0b9e-45f9-b450-a320d32abd9c
;;; vt320.el ends here
