;; -*- no-byte-compile: t -*-
(defun terminal-init-vt300 ()
  "Terminal initialization function for vt300."
  (terminal-init-vt100)
  ;; Make F11 an escape key.
  (define-key (terminal-local-value 'local-function-key-map nil) "\e[23~" [?\e]))

;;; arch-tag: 876831c9-a6f2-444a-b033-706e6fbc149f
;;; vt300.el ends here
