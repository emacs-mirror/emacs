(load "term/vt100" nil t)

;; Make F11 an escape key.
(define-key function-key-map "\e[23~" [?\e])

;;; vt400.el ends here
