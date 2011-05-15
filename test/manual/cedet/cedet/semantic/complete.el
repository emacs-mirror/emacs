(defun semantic-complete-test ()
  "Test completion mechanisms."
  (interactive)
  (message "%S"
           (semantic-format-tag-prototype
            (semantic-complete-read-tag-project "Jump to symbol: "))))
