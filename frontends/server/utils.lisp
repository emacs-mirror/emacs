(defpackage :lem-server/utils
  (:use :cl)
  (:export :hash
           :with-error-handler
           :json-equal
           :pretty-json))
(in-package :lem-server/utils)

(defun hash (&rest args)
  (alexandria:plist-hash-table args :test #'equal))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (log:info "~A"
                                         (with-output-to-string (stream)
                                           (format stream "~A~%" c)
                                           (uiop:print-backtrace :stream stream
                                                                 :condition c)
                                           (force-output stream))))))
         ,@body)
     (error ())))

(defun json-equal (x y)
  (string= (with-output-to-string (out) (yason:encode x out))
           (with-output-to-string (out) (yason:encode y out))))

(defun pretty-json (value)
  (with-output-to-string (out)
    (yason:encode value
                  (yason:make-json-output-stream out))))
