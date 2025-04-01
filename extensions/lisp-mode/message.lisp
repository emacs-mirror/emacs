(in-package :lem-lisp-mode/internal)

(defun display-message (control-string &rest format-arguments)
  (show-message (apply #'format nil control-string format-arguments)
                :timeout nil))
