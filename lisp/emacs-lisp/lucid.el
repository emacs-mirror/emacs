(defun add-timeout (secs function object &optional resignal)
  (run-at-time secs resignal function object))

(defun disable-timeout (timeout)
  (cancel-timer timeout))
