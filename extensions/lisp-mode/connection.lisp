(uiop:define-package :lem-lisp-mode/connection
  (:use :cl
        :lem-lisp-mode/errors)
  (:import-from :trivial-types
                :association-list
                :proper-list)
  (:import-from :lem-lisp-mode/rpc
                :write-message-to-stream
                :read-message-from-stream)
  (:import-from :lem-lisp-mode/reader
                :read-from-string*)
  (:export :current-connection)
  (:export :*broadcast*
           :with-broadcast-connections)
  (:export :connection
           :connection-hostname
           :connection-port
           :connection-package
           :connection-prompt-string
           :connection-features
           :connection-command
           :connection-process
           :connection-process-directory
           :connection-system-file-patterns
           :connection-plist
           :self-connection-p)
  (:export :new-connection
           :read-message-string
           :send-message-string
           :send-message
           :message-waiting-p
           :new-request-id
           :remote-eval-from-string
           :remote-eval
           :finish-evaluated
           :abort-all
           :request-connection-info
           :request-listener-eval
           :read-message
           :read-all-messages)
  (:export :connection-package
           :connection-pid
           :connection-implementation-name
           :connection-implementation-version
           :connection-machine-type
           :connection-machine-version
           :connection-swank-version
           :connection-value))
(in-package :lem-lisp-mode/connection)

(defmacro with-swank-syntax (() &body body)
  `(with-standard-io-syntax
     (let ((*package* (find-package :micros/io-package))
           (*print-case* :downcase)
           (*print-readably* nil))
       ,@body)))

(defvar *connection* nil)

(defun current-connection ()
  *connection*)

(defun (setf current-connection) (connection)
  (setf *connection* connection))

(defgeneric self-connection-p (connection))

(defclass <connection> () ())

(defclass connection (<connection>)
  ((hostname
    :reader connection-hostname
    :initarg :hostname
    :type string
    :documentation "The host to connect to.")
   (port
    :reader connection-port
    :initarg :port
    :type integer
    :documentation "The port to connect to.")
   (socket
    :reader connection-socket
    :initarg :socket
    :type usocket:stream-usocket
    :documentation "The usocket socket.")
   (package
    :accessor connection-package
    :initform "COMMON-LISP-USER"
    :type string
    :documentation "The name of the connection's package.")
   (prompt-string
    :accessor connection-prompt-string
    :type string)
   (continuations
    :accessor connection-continuations
    :initform nil)
   (debug-level :accessor connection-debug-level
                :initform 0
                :type integer
                :documentation "The depth at which the debugger is called.")
   (pid :accessor connection-pid
        :type integer
        :documentation "The PID of the Swank server process.")
   (implementation-name :accessor connection-implementation-name
                        :type string
                        :documentation "The name of the implementation running
 the Swank server.")
   (implementation-version :accessor connection-implementation-version
                           :type string
                           :documentation "The version string of the
 implementation running the Swank server.")
   (machine-type :accessor connection-machine-type
                 :type string
                 :documentation "The server machine's architecture.")
   (machine-version :accessor connection-machine-version
                    :type string
                    :documentation "The server machine's processor type.")
   (swank-version :accessor connection-swank-version
                  :type string
                  :documentation "The server's Swank version.")
   (features :accessor connection-features)
   (system-file-patterns :accessor connection-system-file-patterns)
   (info :accessor connection-info)
   (command :initform nil :accessor connection-command)
   (process :initform nil :accessor connection-process)
   (connection-process-directory
    :initform nil
    :accessor connection-process-directory)
   (plist :initform nil :accessor connection-plist))
  (:documentation "A connection to a remote Lisp."))

(defmethod self-connection-p ((connection <connection>))
  nil)

(defmethod connection-value ((connection connection) key)
  (getf (connection-plist connection) key))

(defmethod (setf connection-value) (value (connection connection) key)
  (setf (getf (connection-plist connection) key) value))

(defun new-connection (hostname port)
  (log:debug "Connecting to SWANK"
             hostname
             port)
  (let* ((socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8)))
         (connection (make-instance 'connection
                                    :hostname hostname
                                    :port port
                                    :socket socket)))
    (setup connection)

    (when (and (member (connection-hostname connection) '("127.0.0.1" "localhost") :test 'equal)
               (equal (connection-pid connection)
                      (micros/backend:getpid)))
      (change-class connection 'self-connection))

    connection))

(defun read-return-message (connection &key (timeout 5))
  "Read only ':return' message. Other messages such as ':indentation-update' are dropped."
  (log:debug "Waiting for response")

  (loop :with info
        :for waiting := (message-waiting-p connection :timeout timeout)
        :do (unless waiting
              (log:debug "Read timeout")
              (return nil))
            (setf info (read-message connection))
            (log:debug "Received" info)
            (when (eq (car info) :return)
              (log:debug "Exiting from read-return-message")
              (return info))))

(defun setup (connection)
  (log:debug "Setup connection")

  (remote-eval connection `(micros:connection-info))

  (loop :for message := (read-message connection)
        :repeat 1000 ;; avoid infinite loop just in case
        :do (log:debug "Received" message)
            (when (eq (first message) :return)
              (let* ((info message)
                     (data (getf (getf info :return) :ok))
                     (impl (getf data :lisp-implementation))
                     (machine (getf data :machine)))
                (setf (connection-info connection) info)
                (setf (connection-pid connection)
                      (getf data :pid)
                      (connection-implementation-name connection)
                      (getf impl :name)
                      (connection-implementation-version connection)
                      (getf impl :version)
                      (connection-machine-type connection)
                      (getf machine :type)
                      (connection-machine-version connection)
                      (getf machine :version)
                      (connection-swank-version connection)
                      (getf data :version)
                      (connection-features connection)
                      (getf data :features)
                      (connection-package connection)
                      (getf (getf data :package) :name)
                      (connection-prompt-string connection)
                      (getf (getf data :package) :prompt)
                      (connection-system-file-patterns connection)
                      (getf data :system-file-patterns)))
              (return)))

  (log:debug "Creating the REPL" connection)
  (remote-eval-from-string connection "(micros/repl:create-repl nil :coding-system \"utf-8-unix\")")
  ;; Wait for startup
  (read-return-message connection)

  (log:debug "Reading rest messages")
  ;; Read all the other messages, dumping them
  (read-all-messages connection)
  (log:debug "Setup is done now"))

(defun read-message-string (connection)
  "Read a message string from a Swank connection.

This function will block until it reads everything. Consider message-waiting-p
to check if input is available."
  (let* ((socket (connection-socket connection))
         (stream (usocket:socket-stream socket)))
    (read-message-from-stream stream)))

(defun send-message-string (connection message)
  "Send a message string to a Swank connection."
  (let* ((socket (connection-socket connection))
         (stream (usocket:socket-stream socket)))
    (write-message-to-stream stream message)
    (force-output stream)
    message))

(defun send-message (connection form)
  (send-message-string connection
                       (with-swank-syntax ()
                         (prin1-to-string form))))

;; workaround for windows
;;  (usocket:wait-for-input needs WSAResetEvent before call)
#+(and sbcl win32)
(sb-alien:define-alien-routine ("WSAResetEvent" wsa-reset-event)
    (boolean #.(sb-alien:alien-size sb-alien:int))
  (event-object usocket::ws-event))

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

;;; Sending messages

(defvar *request-id-counter* 0)

(defun new-request-id ()
  (incf *request-id-counter*))

(defun remote-eval-from-string (connection
                                string
                                &key continuation
                                     thread
                                     package
                                     request-id)
  (let* ((request-id (or request-id (new-request-id)))
         (msg (format nil
                      "(:emacs-rex ~A ~S ~A ~A)"
                      string
                      (or package
                          (connection-package connection))
                      (or thread t)
                      request-id)))
    (log:debug "Sending string to Swank"
               msg
               continuation
               thread
               package)

    (when continuation
      (push (cons request-id continuation)
            (connection-continuations connection)))
    (send-message-string connection msg)))

(defun remote-eval (connection form &key continuation thread package request-id)
  (remote-eval-from-string connection
                           (with-swank-syntax ()
                             (prin1-to-string form))
                           :continuation continuation
                           :thread thread
                           :package package
                           :request-id request-id))

(defun finish-evaluated (connection value id)
  (let ((elt (assoc id (connection-continuations connection))))
    (when elt
      (setf (connection-continuations connection)
            (remove id (connection-continuations connection) :key #'car))
      (funcall (cdr elt) value))))

(defun abort-all (connection condition)
  (loop :for (id . fn) :in (connection-continuations connection)
        :do (funcall fn `(:abort ,condition)))
  (setf (connection-continuations connection) nil))

(defun request-listener-eval (connection string &optional continuation window-width)
  "Request that Swank evaluate a string of code in the REPL."
  (remote-eval-from-string connection
                           (if window-width
                               (format nil "(micros/repl:listener-eval ~S :window-width ~A)" string window-width)
                               (format nil "(micros/repl:listener-eval ~S)" string))
                           :continuation continuation
                           :thread ":repl-thread"))

;;; Reading/parsing messages

(defun read-message (connection)
  "Read an arbitrary message from a connection."
  (with-swank-syntax ()
    (read-from-string* (read-message-string connection))))

(defun read-all-messages (connection)
  (loop while (message-waiting-p connection) collecting
    (read-message connection)))


;;; self connection

(defclass self-connection (connection)
  ())

(defmethod self-connection-p ((connection self-connection))
  t)

;;; broadcast

(defvar *broadcast* nil)

(defun call-with-broadcast-connections (function)
  (if (or (self-connection-p (current-connection))
          (not *broadcast*))
      (funcall function (current-connection))
      (dolist (connection (lem-lisp-mode/connections:connection-list))
        (unless (self-connection-p connection)
          (funcall function connection)))))

(defmacro with-broadcast-connections ((connection) &body body)
  `(call-with-broadcast-connections (lambda (,connection) ,@body)))
