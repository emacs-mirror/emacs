(defun semanticdb-test-current-database-list ()
  "Call and output results of `semanticdb-current-database-list'.
Uses the `default-directory' to derive results."
  (interactive)
  (require 'data-debug)
  (let ((start (current-time))
        (p (semanticdb-current-database-list)))
    (data-debug-new-buffer "*SEMANTICDB Current Database List*")
    (data-debug-insert-stuff-list p "*")))
