
(defun terminal-init-bobcat ()
  "Terminal initialization function for bobcat."
  ;; HP terminals usually encourage using ^H as the rubout character
  (keyboard-translate ?\177 ?\^h)
  (keyboard-translate ?\^h ?\177))

(provide 'term/bobcat)

;;; bobcat.el ends here
