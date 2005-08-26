;; -*- no-byte-compile: t -*-
;; For our purposes we can treat the vt200 and vt100 almost alike.
;; Most differences are handled by the termcap entry.

(defun terminal-init-vt200 ()
  "Terminal initialization function for vt200."  
  (load "term/vt100" nil t)
  ;; Make F11 an escape key.
  (define-key function-key-map "\e[23~" [?\e]))

;;; arch-tag: 0f78f583-9f32-4237-b106-28bcfff21d89
;;; vt200.el ends here
