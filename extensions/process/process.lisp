(in-package :lem-process)

(defclass process ()
  ((pointer
    :initarg :pointer
    :reader process-pointer)
   (name
    :initarg :name
    :reader process-name)
   (command
    :initarg :command
    :reader process-command)
   (buffer-stream
    :initarg :buffer-stream
    :reader process-buffer-stream)
   (read-thread
    :initarg :read-thread
    :writer set-process-read-thread
    :reader process-read-thread)
   (output-callback
    :initarg :output-callback
    :reader process-output-callback)
   (callback-type
    :initarg :output-callback-type
    :reader process-output-callback-type)))

(defun run-process (command &key name output-callback output-callback-type directory)
  (setf command (uiop:ensure-list command))
  (let ((buffer-stream (make-string-output-stream)))
    (let* ((pointer (async-process:create-process command :nonblock nil :directory directory))
           (process (make-instance 'process
                                   :pointer pointer
                                   :name name
                                   :command command
                                   :buffer-stream buffer-stream
                                   ;; :read-thread thread
                                   :output-callback output-callback
                                   :output-callback-type output-callback-type))
           (thread (bt2:make-thread
                    (lambda ()
                      (loop
                        (unless (async-process:process-alive-p pointer)
                          (return))
                        (alexandria:when-let
                            (string (async-process:process-receive-output pointer))
                          (send-event (lambda ()
                                        (write-to-buffer process string))))))
                    :name (format nil "run-process ~{~A~^ ~}" command))))
      (set-process-read-thread thread process)
      process)))

(defun get-process-output-string (process)
  (get-output-stream-string (process-buffer-stream process)))

(defun write-to-buffer (process string)
  (let ((buffer-stream (process-buffer-stream process))
        (output-callback (process-output-callback process))
        (output-callback-type (process-output-callback-type process)))
    (write-string string buffer-stream)
    (when output-callback
      (case output-callback-type
        (:process-input
         (funcall output-callback process string))
        (otherwise
         (funcall output-callback string))))))

(defun delete-process (process)
  (when (bt2:thread-alive-p (process-read-thread process))
    (bt2:destroy-thread (process-read-thread process)))
  (async-process:delete-process (process-pointer process)))

(defun process-alive-p (process)
  (async-process:process-alive-p (process-pointer process)))

(defun process-send-input (process string)
  (async-process:process-send-input (process-pointer process) string))
