;; -*- no-byte-compile: t -*-
;; For our purposes we can treat the vt200 and vt100 almost alike.
;; Most differences are handled by the termcap entry.
(defun terminal-init-vt240 ()
  "Terminal initialization function for vt240."
  (terminal-init-vt100)
  ;; Make F11 an escape key.
  (define-key local-function-key-map "\e[23~" [?\e]))

;;; arch-tag: d9f88e9c-02dc-49ff-871c-a415f08e4eb7
;;; vt240.el ends here
