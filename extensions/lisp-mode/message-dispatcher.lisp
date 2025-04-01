(defpackage :lem-lisp-mode/message-dispatcher
  (:use :cl)
  (:export :get-message-dispatcher
           :define-message
           :dispatch-message))
(in-package :lem-lisp-mode/message-dispatcher)

(defvar *message-dispatcher* (make-hash-table :test 'eq))

(defun get-message-dispatcher (name)
  (gethash name *message-dispatcher*))

(defmacro define-message ((name &rest params) &body body)
  (alexandria:with-unique-names (message)
    (let ((fn-name (alexandria:symbolicate "$$MESSAGE-DISPATCHER-" name)))
      `(progn
         (defun ,fn-name (,message)
           (destructuring-bind (,@params) (rest ,message)
             ,@body))
         (setf (gethash ,name *message-dispatcher*)
               ',fn-name)))))

(defvar *event-log* '())

(defun log-message (string)
  "Log a message."
  (push string *event-log*))

(defvar *event-hooks* '())

(defun dispatch-message (message)
  (log-message (prin1-to-string message))
  (dolist (e *event-hooks*)
    (when (funcall e message)
      (return-from dispatch-message)))
  (alexandria:when-let (dispatcher (get-message-dispatcher (first message)))
    (funcall dispatcher message)))
