(in-package :lem-core)

(defvar *editor-event-queue* (make-concurrent-queue))

(defun event-queue-length ()
  (len *editor-event-queue*))

(defun send-event (obj)
  (enqueue *editor-event-queue* obj))

(defun send-abort-event (editor-thread force)
  (bt2:interrupt-thread editor-thread
                       (lambda ()
                         (interrupt force))))

(defun receive-event (timeout)
  (loop
    (let ((e (dequeue *editor-event-queue*
                      :timeout timeout
                      :timeout-value :timeout)))
      (cond ((null e)
             (return nil))
            ((eql e :timeout)
             (assert timeout)
             (return nil))
            ((eql e :resize)
             (when (>= 1 (event-queue-length))
               (update-on-display-resized)))
            ((or (functionp e) (symbolp e))
             (funcall e))
            (t
             (return e))))))
