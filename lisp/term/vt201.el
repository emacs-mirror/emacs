;; For our purposes we can treat the vt200 and vt100 almost alike.
;; Most differences are handled by the termcap entry.
(load "term/vt100" nil t)

;; Make F11 an escape key.
(define-key function-key-map "\e[23~" [?\e])

;;; vt201.el ends here
