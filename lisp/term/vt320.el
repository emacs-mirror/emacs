;; -*- no-byte-compile: t -*-
(load "term/vt100" nil t)

;; Make F11 an escape key.
(define-key function-key-map "\e[23~" [?\e])

;;; arch-tag: f9f4c954-0b9e-45f9-b450-a320d32abd9c
;;; vt320.el ends here
