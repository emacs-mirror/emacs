(defpackage :lem-language-server/micros-client
  (:nicknames :micros/client)
  (:use :cl)
  (:import-from :lem/common/socket
                :random-available-port)
  (:export :*write-string-function*
           :connection-port
           :connection-swank-port
           :new-request-id
           :remote-eval
           :remote-eval-sync
           :interrupt
           :start-server-and-connect
           :connect
           :stop-server))
(in-package :lem-language-server/micros-client)

(defvar *write-string-function* nil)

(define-condition give-up-connection-to-server (error)
  ((hostname :initarg :hostname)
   (port :initarg :port))
  (:report (lambda (c s)
             (with-slots (hostname port retry-count retry-interval) c
               (format s
                       "Give Up connection to server ~A:~D"
                       hostname port)))))

(define-condition remote-eval-abort (error)
  ((condition :initarg :condition))
  (:report (lambda (c s)
             (with-slots (condition) c
               (format s "Evaluation aborted on ~A." condition)))))

(defstruct continuation id function)

(defclass connection ()
  ((socket
    :initarg :socket
    :reader connection-socket)
   (hostname
    :initarg :hostname
    :reader connection-hostname)
   (port
    :initarg :port
    :reader connection-port)
   (package
    :initform "COMMON-LISP-USER"
    :accessor connection-package)
   (request-id-counter
    :initform 0
    :accessor connection-request-id-counter)
   (request-id-counter-mutex
    :initform (sb-thread:make-mutex)
    :reader connection-request-id-counter-mutex)
   (continuations
    :initform '()
    :accessor connection-continuations)
   (continuations-mutex
    :initform (sb-thread:make-mutex :name "continuations-mutex")
    :reader connection-continuations-mutex)
   (message-dispatcher-thread
    :accessor connection-message-dispatcher-thread)
   (server-process
    :initform nil
    :accessor connection-server-process)
   (swank-port
    :initform nil
    :accessor connection-swank-port)))

(defun new-request-id (connection)
  (sb-thread:with-mutex ((connection-request-id-counter-mutex connection))
    (incf (connection-request-id-counter connection))))

(defun add-continuation (connection request-id continuation)
  (sb-thread:with-mutex ((connection-continuations-mutex connection))
    (push (make-continuation :id request-id :function continuation)
          (connection-continuations connection))))

(defun get-and-drop-continuation (connection request-id)
  (sb-thread:with-mutex ((connection-continuations-mutex connection))
    (let ((continuation
            (find request-id
                  (connection-continuations connection)
                  :key #'continuation-id)))
      (when continuation
        (setf (connection-continuations connection)
              (remove request-id
                      (connection-continuations connection)
                      :key #'continuation-id))
        continuation))))

(defun socket-connect (hostname port)
  (let ((socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8))))
    (setf (sb-bsd-sockets:sockopt-keep-alive (usocket:socket socket)) t)
    (log:debug "socket connected" hostname port)
    socket))

(defun create-connection (hostname port)
  (let* ((socket (socket-connect hostname port))
         (connection (make-instance 'connection
                                    :socket socket
                                    :hostname hostname
                                    :port port)))
    connection))

(defun message-waiting-p (connection &key (timeout 0))
  "t if there's a message in the connection waiting to be read, nil otherwise."
  (let* ((socket (connection-socket connection))
         (stream (usocket:socket-stream socket)))

    ;; check stream buffer
    (when (listen stream)
      (return-from message-waiting-p t))

    ;; workaround for windows
    ;;  (usocket:wait-for-input needs WSAResetEvent before call)
    #+(and sbcl win32)
    (when (usocket::wait-list socket)
      (wsa-reset-event
       (usocket::os-wait-list-%wait
        (usocket::wait-list socket))))

    ;; check socket status
    (if (usocket:wait-for-input socket
                                :ready-only t
                                :timeout timeout)
        t
        nil)))

(defun read-message (connection)
  (micros/rpc:read-message (usocket:socket-stream (connection-socket connection))
                           micros::*swank-io-package*
                           :validate-input t))

(defun send-message (connection message)
  (micros/rpc:write-message message
                            micros::*swank-io-package*
                            (usocket:socket-stream (connection-socket connection))))

(defun remote-eval (connection
                    expression
                    &key (package-name (connection-package connection))
                         (thread t)
                         (request-id (new-request-id connection))
                         callback)
  (check-type callback function)
  (add-continuation connection request-id callback)
  (send-message connection
                `(:emacs-rex
                  ,expression
                  ,package-name
                  ,thread
                  ,request-id)))

(defun remote-eval-sync (connection
                         expression
                         &key (package-name (connection-package connection)))
  (let ((mailbox (sb-concurrency:make-mailbox)))
    (remote-eval connection
                 expression
                 :package-name package-name
                 :thread t
                 :callback (lambda (value)
                             (sb-concurrency:send-message mailbox value)))
    (let ((value (sb-concurrency:receive-message mailbox)))
      (alexandria:destructuring-ecase value
        ((:ok result)
         result)
        ((:abort condition)
         (error 'remote-eval-abort :condition condition))))))

(defun interrupt (connection request-id)
  (send-message connection `(:interrupt-thread ,request-id)))

(defun setup-repl (connection)
  (let ((result
          (remote-eval-sync connection
                            '(micros/repl:create-repl nil :coding-system "utf-8-unix")
                            :package-name "CL-USER")))
    (destructuring-bind (package-name package-string-for-prompt) result
      (declare (ignore package-string-for-prompt))
      (setf (connection-package connection) package-name)))
  (values))

(defun call-continuation (connection value request-id)
  (let ((continuation (get-and-drop-continuation connection request-id)))
    (when continuation
      (funcall (continuation-function continuation) value))))

(defun micros-write-string (string &key target (info :log))
  (check-type info (member :log :error))
  (when *write-string-function*
    (funcall *write-string-function* string target info)))

(defun dispatch-message (connection message)
  (log:debug message)
  (alexandria:destructuring-case message
    ((:return value request-id)
     (call-continuation connection value request-id))
    ((:write-string string &optional target)
     (micros-write-string string :target target))
    ((:debug thread level condition restarts frames conts)
     (declare (ignore level condition restarts frames conts))
     (remote-eval connection
                  '(micros:sldb-abort)
                  :thread thread
                  :callback (lambda (value)
                              (declare (ignore value)))))))

(defun dispatch-waiting-messages (connection)
  (loop :while (message-waiting-p connection)
        :for message := (read-message connection)
        :do (dispatch-message connection message)))

(defun dispatch-message-loop (connection)
  (loop
    (dispatch-waiting-messages connection)))

(defun connect-until-successful (hostname port)
  (loop :for second :in '(0.1 0.2 0.4 0.8 1.6 3.2 6.4)
        :do (sleep second)
            (handler-case (create-connection hostname port)
              (usocket:connection-refused-error ())
              (:no-error (connection)
                (return connection)))
        :finally (error 'give-up-connection-to-server
                        :hostname hostname
                        :port port)))

(defun create-server-process (port &key (swank-port nil))
  (let ((command
          `("ros"
            "run"
            "-s"
            ,@(when swank-port
                (list "swank"
                      "-e"
                      (format nil "(swank:create-server :dont-close t :port ~D)" swank-port)))
            "-s"
            "micros"
            "-e"
            ,(format nil "(micros:create-server :dont-close t :port ~D)" port))))
    (log:debug "create-process" command)
    (async-process:create-process command)))

(defun receive-process-output-loop (process)
  (loop
    (unless (async-process:process-alive-p process)
      (return))
    (let ((output-string (async-process:process-receive-output process)))
      (when output-string
        (log:debug "process output" output-string)))))

(defun make-dispatch-message-loop-thread (connection)
  (sb-thread:make-thread #'dispatch-message-loop
                         :name "micros/client dispatch-message-loop"
                         :arguments (list connection)))

(defun setup (connection &key server-process swank-port)
  (let ((thread (make-dispatch-message-loop-thread connection)))
    (setf (connection-message-dispatcher-thread connection) thread
          (connection-server-process connection) server-process
          (connection-swank-port connection) swank-port)
    (setup-repl connection)
    connection))

(defun start-server-and-connect (deny-port)
  (let* ((micros-port (random-available-port deny-port))
         (swank-port (random-available-port deny-port micros-port))
         (process (create-server-process micros-port :swank-port swank-port)))
    (log:debug process (async-process::process-pid process))
    (log:debug "swank port: ~D, micros port: ~D" swank-port micros-port)
    (let ((connection (connect-until-successful "localhost" micros-port)))
      (setup connection :server-process process :swank-port swank-port)
      (sb-thread:make-thread (lambda ()
                               (receive-process-output-loop process)))
      connection)))

(defun connect (hostname port)
  (let ((connection (connect-until-successful hostname port)))
    (setup connection)
    connection))

(defun stop-server (connection)
  (sb-thread:terminate-thread (connection-message-dispatcher-thread connection))
  (async-process:delete-process (connection-server-process connection))
  (values))
