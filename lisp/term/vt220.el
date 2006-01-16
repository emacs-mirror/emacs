;; -*- no-byte-compile: t -*-
;; For our purposes we can treat the vt200 and vt100 almost alike.
;; Most differences are handled by the termcap entry.
(defun terminal-init-vt220 ()
  "Terminal initialization function for vt220."  
  (load "term/vt100" nil t)
  ;; Make F11 an escape key.
  (define-key function-key-map "\e[23~" [?\e]))

;;; arch-tag: 98fc4867-a20d-46a1-a276-d7be31e49871
;;; vt220.el ends here
