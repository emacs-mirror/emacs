(defpackage :lem-copilot/logger
  (:use :cl)
  (:local-nicknames (:ring :lem/common/ring))
  (:export :do-log
           :write-log))
(in-package :lem-copilot/logger)

(defvar *logs* (ring:make-ring 1000))
(defvar *log-lock* (bt2:make-lock :name "copilot log lock"))

(defun do-log (output)
  (bt2:with-lock-held (*log-lock*)
    (ring:ring-push *logs* output)))

(defun write-log (stream)
  (bt2:with-lock-held (*log-lock*)
    (loop :for i :downfrom (1- (ring:ring-length *logs*)) :to 0
          :for log := (ring:ring-ref *logs* i)
          :do (write-line log stream))))
