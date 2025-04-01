(defpackage :lem-ncurses/mainloop
  (:use :cl :lem)
  (:export :invoke))
(in-package :lem-ncurses/mainloop)

(define-condition exit (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(defun input-loop (editor-thread)
  (handler-case
      (loop
        (handler-case
            (progn
              (unless (bt2:thread-alive-p editor-thread) (return))
              (let ((event (lem-ncurses/input:get-event)))
                (if (eq event :abort)
                    (send-abort-event editor-thread nil)
                    (send-event event))))
          #+sbcl
          (sb-sys:interactive-interrupt (c)
            (declare (ignore c))
            (send-abort-event editor-thread t))))
    (exit (c) (return-from input-loop c))))

(defun invoke (function)
  (let ((result nil)
        (input-thread (bt2:current-thread)))
    (unwind-protect
         (when (lem-ncurses/term:term-init)
           (let ((*standard-output* (make-broadcast-stream))
                 (*error-output* (make-broadcast-stream))
                 (*terminal-io* (make-broadcast-stream)))
             (let ((editor-thread
                     (funcall function
                              nil
                              (lambda (report)
                                (bt2:interrupt-thread
                                 input-thread
                                 (lambda () (error 'exit :value report)))))))
               (setf result (input-loop editor-thread)))))
      (lem-ncurses/term:term-finalize))
    (when (and (typep result 'exit)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))
