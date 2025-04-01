(defpackage :lem/buffer/buffer-list-manager
  (:use :cl)
  (:export
   :delete-buffer-using-manager
   :buffer-list-manager
   :with-current-buffers))
(in-package :lem/buffer/buffer-list-manager)

(defgeneric delete-buffer-using-manager (buffer-list-manager buffer))

(defclass buffer-list-manager ()
  ((buffers
    :initarg :buffers
    :initform '()
    :accessor buffer-list-manager-buffers)))

(defvar *buffer-list-manager* (make-instance 'buffer-list-manager))

(defun buffer-list-manager ()
  *buffer-list-manager*)

(defun call-with-current-buffers (buffer-list function)
  (let ((buffers (buffer-list-manager-buffers (buffer-list-manager))))
    (setf (buffer-list-manager-buffers (buffer-list-manager)) buffer-list)
    (unwind-protect (funcall function)
      (setf (buffer-list-manager-buffers (buffer-list-manager)) buffers))))

(defmacro with-current-buffers (buffer-list &body body)
  `(call-with-current-buffers ,buffer-list (lambda () ,@body)))
