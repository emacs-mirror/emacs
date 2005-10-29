;; -*- no-byte-compile: t -*-
(defun terminal-init-vt420
  "Terminal initialization function for vt420."
  (terminal-init-vt100)
  ;; Make F11 an escape key.
  (define-key local-function-key-map "\e[23~" [?\e]))

;;; arch-tag: df2f897c-3a12-4b3c-9259-df089f96c160
;;; vt420.el ends here
