(in-package :lem-core)

(defun invoke-save-excursion (function)
  (let ((point (copy-point (current-point) :right-inserting))
        (mark (when (buffer-mark-p (current-buffer))
                (copy-point (buffer-mark (current-buffer))
                            :right-inserting))))
    (unwind-protect (funcall function)
      (setf (current-buffer) (point-buffer point))
      (move-point (current-point) point)
      (delete-point point)
      (when mark
        (set-current-mark mark)
        (delete-point mark)))))

(defmacro save-excursion (&body body)
  "Saves the current `point` and `mark`, restores them after evaluation of `body` and returns the result of `body`."
  `(invoke-save-excursion (lambda () ,@body)))
