(defpackage :lem/common/timer
  (:use :cl :alexandria)
  (:export
   :timer-manager
   :send-timer-notification
   :with-timer-manager
   :running-timer
   :timer-error
   :timer
   :timer-name
   :timer-expired-p
   :make-timer
   :make-idle-timer
   :start-timer
   :stop-timer
   :with-idle-timers
   :update-idle-timers
   :timer-ms
   :get-next-timer-timing-ms)
  #+sbcl
  (:lock t))
(in-package :lem/common/timer)

(defvar *running-timer*)
(defun running-timer () *running-timer*)

(defvar *timer-manager*)

(defgeneric send-timer-notification (timer-manager continue))

(defclass timer-manager ()
  ())

(defmethod get-microsecond-time ((timer-manager timer-manager))
  (values
   (floor (/ (get-internal-real-time)
             (load-time-value (/ internal-time-units-per-second 1000))))))

(defun call-with-timer-manager (timer-manager function)
  (setf *timer-manager* timer-manager)
  (funcall function))

(defmacro with-timer-manager (timer-manager &body body)
  `(call-with-timer-manager ,timer-manager (lambda () ,@body)))

(define-condition timer-error (error)
  ((timer :initarg :timer)
   (condition :initarg :condition))
  (:report (lambda (c s)
             (with-slots (timer condition) c
               (format s "Error running timer ~S: ~A" (timer-name timer) condition)))))

(defgeneric timer-expired-p (timer))
(defgeneric expire-timer (timer))
(defgeneric inspire-timer (timer))

(defclass <timer> ()
  ((name
    :initarg :name
    :reader timer-name
    :type (or null string))
   (function
    :initarg :function
    :reader timer-function
    :type (or symbol function))
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))
   (ms
    :accessor timer-ms
    :type (integer 1 *))
   (repeat-p
    :accessor timer-repeat-p
    :type boolean)
   (expired-p
    :initform nil
    :reader timer-expired-p
    :writer set-timer-expired-p
    :type boolean)))

(defmethod print-object ((object <timer>) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (prin1 (timer-name object) stream)))

(defun guess-function-name (function)
  (etypecase function
    (function (format nil "~a"
		      (micros::function-name function)))
    (symbol (symbol-name function))))

(defun make-timer-instance (timer-class function name handle-function)
  (make-instance timer-class
                 :name (or name
			   (guess-function-name function))
                 :function (ensure-function function)
                 :handle-function (when handle-function
                                    (ensure-function handle-function))))

(defun call-timer-function (timer)
  (let ((*running-timer* timer))
    (handler-case
        (if (timer-handle-function timer)
            (handler-bind ((error (timer-handle-function timer)))
              (funcall (timer-function timer)))
            (funcall (timer-function timer)))
      (error (condition)
        (error 'timer-error :timer timer :condition condition)))))


;;; timer
(defclass timer (<timer>)
  ((mutex
    :accessor timer-mutex
    :type bt2:lock)
   (stop-mailbox
    :accessor timer-stop-mailbox
    :type lem-mailbox:mailbox)
   (thread
    :accessor timer-thread
    :type bt2:thread)))

(defmethod timer-expired-p ((timer timer))
  (bt2:with-lock-held ((timer-mutex timer))
    (call-next-method)))

(defmethod expire-timer ((timer timer))
  (bt2:with-lock-held ((timer-mutex timer))
    (set-timer-expired-p t timer)))

(defmethod inspire-timer ((timer timer))
  (bt2:with-lock-held ((timer-mutex timer))
    (set-timer-expired-p nil timer)))

(defun make-timer (function &key name handle-function)
  (make-timer-instance 'timer function name handle-function))

(defmethod start-timer ((timer timer) ms &key repeat)
  (setf (timer-ms timer) ms
        (timer-repeat-p timer) repeat
        (timer-mutex timer)
        (bt2:make-lock :name "timer internal mutex"))
  (start-timer-thread timer ms repeat)
  timer)

(defmethod stop-timer ((timer timer))
  (stop-timer-thread timer))

(defun start-timer-thread (timer ms repeat-p)
  (let ((stop-mailbox (lem-mailbox:make-mailbox))
        (timer-manager *timer-manager*)
        (seconds (float (/ ms 1000))))
    (setf (timer-stop-mailbox timer)
          stop-mailbox)
    (setf (timer-thread timer)
          (bt2:make-thread
           (lambda ()
             (loop
               (let ((recv-stop-msg
                       (nth-value 1
                                  (lem-mailbox:receive-message stop-mailbox
							       :timeout seconds))))
                 (when recv-stop-msg
                   (expire-timer timer)
                   (return)))
               (send-timer-notification timer-manager
                                        (lambda () (call-timer-function timer)))
               (unless repeat-p
                 (return))))
           :name (format nil "Timer ~A" (timer-name timer))))))

(defun stop-timer-thread (timer)
  (lem-mailbox:send-message (timer-stop-mailbox timer) t))


;;; idle-timer

(defvar *idle-timer-list* nil)
(defvar *processed-idle-timer-list* nil)
(defvar *idle-p* nil)

(defclass idle-timer (<timer>)
  ((last-time
    :initform nil
    :initarg :last-time
    :accessor timer-last-time
    :type (or null (integer 1 *)))))

(defmethod set-timer-last-time (value (timer idle-timer))
  (check-type value (integer 0 *))
  (setf (timer-last-time timer) value))

(defmethod timer-next-time ((timer idle-timer))
  (+ (timer-last-time timer)
     (timer-ms timer)))

(defmethod expire-timer ((timer idle-timer))
  (set-timer-expired-p t timer))

(defmethod inspire-timer ((timer idle-timer))
  (set-timer-expired-p nil timer))

(defun make-idle-timer (function &key name handle-function)
  (make-timer-instance 'idle-timer function name handle-function))

(defmethod start-timer ((timer idle-timer) ms &key repeat)
  (setf (timer-ms timer) ms)
  (setf (timer-repeat-p timer) repeat)
  (when *idle-p*
    (setf (timer-last-time timer)
          (get-microsecond-time *timer-manager*)))
  (push timer *idle-timer-list*)
  timer)

(defmethod stop-timer ((timer idle-timer))
  (stop-idle-timer timer))

(defun stop-idle-timer (timer)
  (expire-timer timer)
  (setf *idle-timer-list* (delete timer *idle-timer-list*)))

(defun start-idle-timers ()
  (flet ((update-last-time-in-idle-timers ()
           (loop :with last-time := (get-microsecond-time *timer-manager*)
                 :for timer :in *idle-timer-list*
                 :do (set-timer-last-time last-time timer)))
         (inspire-idle-timers ()
           (mapc #'inspire-timer *idle-timer-list*)))
    (update-last-time-in-idle-timers)
    (inspire-idle-timers)))

(defun stop-idle-timers ()
  (setf *idle-timer-list* (nconc *processed-idle-timer-list* *idle-timer-list*))
  (setf *processed-idle-timer-list* '()))

(defun call-with-idle-timers (function)
  (start-idle-timers)
  (prog1 (let ((*idle-p* t))
           (funcall function))
    (stop-idle-timers)))

(defmacro with-idle-timers (() &body body)
  `(call-with-idle-timers (lambda () ,@body)))

(defun update-idle-timers ()
  (let* ((tick-time (get-microsecond-time *timer-manager*))
         (updating-timers (remove-if-not (lambda (timer)
                                           (< (timer-next-time timer) tick-time))
                                         *idle-timer-list*))
         (deleting-timers (remove-if #'timer-repeat-p
                                     updating-timers))
         (updating-idle-timers (remove-if-not #'timer-repeat-p
                                              updating-timers)))
    (mapc #'expire-timer deleting-timers)

    (setf *idle-timer-list* (set-difference *idle-timer-list* deleting-timers))
    (setf *idle-timer-list* (set-difference *idle-timer-list* updating-idle-timers))
    (setf *processed-idle-timer-list* (nconc updating-idle-timers *processed-idle-timer-list*))

    (dolist (timer updating-timers)
      (unless (timer-repeat-p timer)
        (set-timer-last-time tick-time timer)))
    (mapc #'call-timer-function updating-timers)
    (not (null updating-timers))))

(defun get-next-timer-timing-ms ()
  (when-let (timers *idle-timer-list*)
    (- (loop :for timer :in timers
             :minimize (timer-next-time timer))
       (get-microsecond-time *timer-manager*))))
