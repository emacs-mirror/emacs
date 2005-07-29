;; -*- no-byte-compile: t -*-
(defun terminal-init-vt400 ()
  "Terminal initialization function for vt400."  
  (load "term/vt100" nil t)
  ;; Make F11 an escape key.
  (define-key function-key-map "\e[23~" [?\e]))

;;; arch-tag: a70809c5-6b21-42cc-ba20-536683e5e7d5
;;; vt400.el ends here
