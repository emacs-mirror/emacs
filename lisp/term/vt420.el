;; -*- no-byte-compile: t -*-
(load "term/vt100" nil t)

;; Make F11 an escape key.
(define-key (terminal-local-value 'local-function-key-map nil) "\e[23~" [?\e])

;;; arch-tag: df2f897c-3a12-4b3c-9259-df089f96c160
;;; vt420.el ends here
