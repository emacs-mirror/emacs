(defpackage :lem/common/queue
  (:use :cl)
  (:export :len
           :enqueue
           :dequeue
           :empty-p
           :make-queue
           :make-concurrent-queue))
(in-package :lem/common/queue)

(defgeneric len (queue))
(defgeneric enqueue (queue obj))
(defgeneric dequeue (queue &key &allow-other-keys))
(defgeneric empty-p (queue))

(defstruct (queue (:constructor %make-queue))
  list
  last)

(defun make-queue ()
  (%make-queue))

(defmethod len ((queue queue))
  (length (queue-list queue)))

(defmethod enqueue ((queue queue) obj)
  (cond ((null (queue-list queue))
         (setf (queue-last queue)
               (setf (queue-list queue) (list obj))))
        (t
         (setf (cdr (queue-last queue)) (list obj))
         (setf (queue-last queue) (cdr (queue-last queue)))))
  obj)

(defmethod dequeue ((queue queue) &key &allow-other-keys)
  (pop (queue-list queue)))

(defmethod empty-p ((queue queue))
  (null (queue-list queue)))

(defstruct (concurrent-queue (:constructor %make-concurrent-queue))
  (wait (bt2:make-condition-variable))
  (lock (bt2:make-lock))
  (queue (make-queue)))

(defun make-concurrent-queue ()
  (%make-concurrent-queue))

(defmethod len ((queue concurrent-queue))
  (bt2:with-lock-held ((concurrent-queue-lock queue))
    (len (concurrent-queue-queue queue))))

(defmethod enqueue ((queue concurrent-queue) obj)
  (bt2:with-lock-held ((concurrent-queue-lock queue))
    (enqueue (concurrent-queue-queue queue) obj)
    (bt2:condition-notify (concurrent-queue-wait queue))))

(defmethod dequeue ((queue concurrent-queue) &key timeout timeout-value)
  (bt2:with-lock-held ((concurrent-queue-lock queue))
    (if (not (empty-p (concurrent-queue-queue queue)))
        (dequeue (concurrent-queue-queue queue))
        (cond ((if timeout
                   (bt2:condition-wait (concurrent-queue-wait queue) (concurrent-queue-lock queue)
                                      :timeout timeout)
                   (bt2:condition-wait (concurrent-queue-wait queue) (concurrent-queue-lock queue)))
               (dequeue (concurrent-queue-queue queue)))
              (t
               timeout-value)))))
