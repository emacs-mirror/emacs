(defpackage :lem-sdl2/log
  (:use :cl)
  (:export :do-log
           :with-debug))
(in-package :lem-sdl2/log)

(defun do-log (value)
  (let ((log-file (merge-pathnames "logs/error.log" (lem:lem-home))))
    (ensure-directories-exist log-file)
    (with-open-file (out log-file
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (uiop:println value out))))

(defun call-with-debug (log-function body-function)
  (funcall log-function)
  (handler-bind ((error (lambda (e)
                          (log:info "~A"
                                    (with-output-to-string (out)
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :condition e :stream out))))))
    (funcall body-function)))

(defmacro with-debug ((&rest args) &body body)
  `(call-with-debug (lambda () (log:debug ,@args))
                    (lambda () ,@body)))
