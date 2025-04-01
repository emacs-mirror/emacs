(in-package :lem-core)

(defclass movable-advice () ())
(defclass jump-cursor-advice () ())
(defclass editable-advice () ())

;;; multiple cursors
(defun process-each-cursors (function)
  (let ((buffer (current-buffer)))
    (dolist (point (sort (copy-list (buffer-fake-cursors buffer)) #'point<))
      (with-buffer-point (buffer point)
        (with-current-killring (fake-cursor-killring point)
          (handler-case
              (save-continue-flags
                (funcall function))
            (move-cursor-error ())))))
    (funcall function)))

(defmacro do-each-cursors (() &body body)
  `(process-each-cursors (lambda () ,@body)))

(defmethod execute :around (mode
                            (command movable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command editable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command jump-cursor-advice)
                            argument)
  (prog1 (call-next-method)
    (clear-cursors (current-buffer))))
