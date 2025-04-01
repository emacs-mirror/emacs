(uiop:define-package :lem/read-only-sources
  (:use :cl :lem)
  (:export :read-only-sources
           :define-read-only-source))
(in-package :lem/read-only-sources)

(define-editor-variable read-only-sources t)

(defvar *patterns* (make-hash-table :test 'equal))

(defun register-pattern (name function)
  (setf (gethash name *patterns*) function))

(defmacro define-read-only-source (name (directory) &body body)
  `(register-pattern ',name (lambda (,directory) ,@body)))

(defun read-only-source-p (directory)
  (maphash (lambda (name pattern)
             (declare (ignore name))
             (when (funcall pattern directory)
               (return-from read-only-source-p t)))
           *patterns*))

(defun on-switch-to-buffer (buffer)
  (when (and (variable-value 'read-only-sources :default buffer)
             (read-only-source-p (buffer-directory buffer)))
    (setf (buffer-read-only-p buffer) t)))

(add-hook *switch-to-buffer-hook* 'on-switch-to-buffer)
