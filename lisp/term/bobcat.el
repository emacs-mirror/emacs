;; -*- no-byte-compile: t -*-

(defun terminal-init-bobcat ()
  "Terminal initialization function for bobcat."  
  ;; HP terminals usually encourage using ^H as the rubout character
  (keyboard-translate ?\177 ?\^h)
  (keyboard-translate ?\^h ?\177))

;;; arch-tag: 754e4520-0a3e-4e6e-8ca5-9481b1f85cf7
;;; bobcat.el ends here
