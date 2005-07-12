;; -*- no-byte-compile: t -*-
(load "term/vt100" nil t)

;; Make F11 an escape key.
(define-key (terminal-local-value 'local-function-key-map nil) "\e[23~" [?\e])

;;; arch-tag: 876831c9-a6f2-444a-b033-706e6fbc149f
;;; vt300.el ends here
