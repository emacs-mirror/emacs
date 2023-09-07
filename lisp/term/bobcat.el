;;; bobcat.el  -*- lexical-binding:t -*-

(defun terminal-init-bobcat ()
  "Terminal initialization function for bobcat."
  ;; HP terminals usually encourage using ^H as the rubout character
  (key-translate "DEL" "C-h")
  (key-translate "C-h" "DEL"))

(provide 'term/bobcat)

;;; bobcat.el ends here
