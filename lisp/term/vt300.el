;; -*- no-byte-compile: t -*-
(defun terminal-init-vt300 ()
  "Terminal initialization function for vt300."
  (tty-run-terminal-initialization (selected-frame) "vt100")
  ;; Make F11 an escape key.
  (define-key input-decode-map "\e[23~" [f11]) ;Probably redundant.
  (define-key local-function-key-map [f11] [?\e]))

;; arch-tag: 876831c9-a6f2-444a-b033-706e6fbc149f
;;; vt300.el ends here
