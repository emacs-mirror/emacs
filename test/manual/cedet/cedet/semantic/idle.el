(defun semantic-idle-pnf-test ()
  "Test `semantic-idle-scheduler-work-parse-neighboring-files' and time it."
  (interactive)
  (let ((start (current-time))
        (junk (semantic-idle-scheduler-work-parse-neighboring-files))
        (end (current-time)))
    (message "Work took %.2f seconds." (semantic-elapsed-time start end))))
