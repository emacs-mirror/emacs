(defun terminal-80-columns ()
  (interactive)
  (send-string-to-terminal "\033[?3l")
  (set-frame-width 80))

(defun terminal-132-columns ()
  (interactive)
  (send-string-to-terminal "\033[?3h")
  (set-frame-width 132))
