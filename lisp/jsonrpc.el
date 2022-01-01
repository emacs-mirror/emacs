;;; jsonrpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: processes, languages, extensions
;; Version: 1.0.14
;; Package-Requires: ((emacs "25.2"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the JSONRPC 2.0 specification as described
;; in https://www.jsonrpc.org/.  As the name suggests, JSONRPC is a
;; generic Remote Procedure Call protocol designed around JSON
;; objects.  To learn how to write JSONRPC programs with this library,
;; see Info node `(elisp)JSONRPC'."
;;
;; This library was originally extracted from eglot.el, an Emacs LSP
;; client, which you should see for an example usage.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(eval-when-compile (require 'subr-x))
(require 'warnings)
(require 'pcase)
(require 'ert) ; to escape a `condition-case-unless-debug'


;;; Public API
;;;

(defclass jsonrpc-connection ()
  ((name
    :accessor jsonrpc-name
    :initarg :name
    :documentation "A name for the connection")
   (-request-dispatcher
    :accessor jsonrpc--request-dispatcher
    :initform #'ignore
    :initarg :request-dispatcher
    :documentation "Dispatcher for remotely invoked requests.")
   (-notification-dispatcher
    :accessor jsonrpc--notification-dispatcher
    :initform #'ignore
    :initarg :notification-dispatcher
    :documentation "Dispatcher for remotely invoked notifications.")
   (last-error
    :accessor jsonrpc-last-error
    :documentation "Last JSONRPC error message received from endpoint.")
   (-request-continuations
    :initform (make-hash-table)
    :accessor jsonrpc--request-continuations
    :documentation "A hash table of request ID to continuation lambdas.")
   (-events-buffer
    :accessor jsonrpc--events-buffer
    :documentation "A buffer pretty-printing the JSONRPC events")
   (-events-buffer-scrollback-size
    :initarg :events-buffer-scrollback-size
    :accessor jsonrpc--events-buffer-scrollback-size
    :documentation "Max size of events buffer.  0 disables, nil means infinite.")
   (-deferred-actions
    :initform (make-hash-table :test #'equal)
    :accessor jsonrpc--deferred-actions
    :documentation "Map (DEFERRED BUF) to (FN TIMER ID).  FN is\
a saved DEFERRED `async-request' from BUF, to be sent not later\
than TIMER as ID.")
   (-next-request-id
    :initform 0
    :accessor jsonrpc--next-request-id
    :documentation "Next number used for a request"))
  :documentation "Base class representing a JSONRPC connection.
The following initargs are accepted:

:NAME (mandatory), a string naming the connection

:REQUEST-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC requests.
CONN is a `jsonrpc-connection' object, method is a symbol, and
PARAMS is a plist representing a JSON object.  The function is
expected to return a JSONRPC result, a plist of (:result
RESULT) or signal an error of type `jsonrpc-error'.

:NOTIFICATION-DISPATCHER (optional), a function of three
arguments (CONN METHOD PARAMS) for handling JSONRPC
notifications.  CONN, METHOD and PARAMS are the same as in
:REQUEST-DISPATCHER.")

;;; API mandatory
(cl-defgeneric jsonrpc-connection-send (conn &key id method params result error)
  "Send a JSONRPC message to connection CONN.
ID, METHOD, PARAMS, RESULT and ERROR.")

;;; API optional
(cl-defgeneric jsonrpc-shutdown (conn)
  "Shutdown the JSONRPC connection CONN.")

;;; API optional
(cl-defgeneric jsonrpc-running-p (conn)
  "Tell if the JSONRPC connection CONN is still running.")

;;; API optional
(cl-defgeneric jsonrpc-connection-ready-p (connection what)
  "Tell if CONNECTION is ready for WHAT in current buffer.
If it isn't, a request which was passed a value to the
`:deferred' keyword argument will be deferred to the future.
WHAT is whatever was passed the as the value to that argument.

By default, all connections are ready for sending all requests
immediately."
  (:method (_s _what)   ;; by default all connections are ready
           t))


;;; Convenience
;;;
(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(defun jsonrpc-events-buffer (connection)
  "Get or create JSONRPC events buffer for CONNECTION."
  (let ((probe (jsonrpc--events-buffer connection)))
    (if (buffer-live-p probe)
        probe
      (with-current-buffer
          (get-buffer-create (format "*%s events*" (jsonrpc-name connection)))
        (buffer-disable-undo)
        (setq buffer-read-only t)
        (setf (jsonrpc--events-buffer connection)
              (current-buffer))))))

(defun jsonrpc-forget-pending-continuations (connection)
  "Stop waiting for responses from the current JSONRPC CONNECTION."
  (clrhash (jsonrpc--request-continuations connection)))

(defun jsonrpc-connection-receive (connection message)
  "Process MESSAGE just received from CONNECTION.
This function will destructure MESSAGE and call the appropriate
dispatcher in CONNECTION."
  (cl-destructuring-bind (&key method id error params result _jsonrpc)
      message
    (let (continuations)
      (jsonrpc--log-event connection message 'server)
      (setf (jsonrpc-last-error connection) error)
      (cond
       (;; A remote request
        (and method id)
        (let* ((debug-on-error (and debug-on-error (not (ert-running-test))))
               (reply
                (condition-case-unless-debug _ignore
                    (condition-case oops
                        `(:result ,(funcall (jsonrpc--request-dispatcher connection)
                                            connection (intern method) params))
                      (jsonrpc-error
                       `(:error
                         (:code
                          ,(or (alist-get 'jsonrpc-error-code (cdr oops)) -32603)
                          :message ,(or (alist-get 'jsonrpc-error-message
                                                   (cdr oops))
                                        "Internal error")))))
                  (error
                   '(:error (:code -32603 :message "Internal error"))))))
          (apply #'jsonrpc--reply connection id reply)))
       (;; A remote notification
        method
        (funcall (jsonrpc--notification-dispatcher connection)
                 connection (intern method) params))
       (;; A remote response
        (setq continuations
              (and id (gethash id (jsonrpc--request-continuations connection))))
        (let ((timer (nth 2 continuations)))
          (when timer (cancel-timer timer)))
        (remhash id (jsonrpc--request-continuations connection))
        (if error (funcall (nth 1 continuations) error)
          (funcall (nth 0 continuations) result))))
      (jsonrpc--call-deferred connection))))


;;; Contacting the remote endpoint
;;;
(defun jsonrpc-error (&rest args)
  "Error out with FORMAT and ARGS.
If invoked inside a dispatcher function, this function is suitable
for replying to the remote endpoint with an error message.

ARGS can be of the form (FORMAT-STRING . MOREARGS) for replying
with a -32603 error code and a message formed by formatting
FORMAT-STRING with MOREARGS.

Alternatively ARGS can be plist representing a JSONRPC error
object, using the keywords `:code', `:message' and `:data'."
  (if (stringp (car args))
      (let ((msg
             (apply #'format-message (car args) (cdr args))))
        (signal 'jsonrpc-error
                `(,msg
                  (jsonrpc-error-code . ,32603)
                  (jsonrpc-error-message . ,msg))))
    (cl-destructuring-bind (&key code message data) args
      (signal 'jsonrpc-error
              `("[jsonrpc] error "
                (jsonrpc-error-code . ,code)
                (jsonrpc-error-message . ,message)
                (jsonrpc-error-data . ,data))))))

(cl-defun jsonrpc-async-request (connection
                                 method
                                 params
                                 &rest args
                                 &key _success-fn _error-fn
                                 _timeout-fn
                                 _timeout _deferred)
  "Make a request to CONNECTION, expecting a reply, return immediately.
The JSONRPC request is formed by METHOD, a symbol, and PARAMS a
JSON object.

The caller can expect SUCCESS-FN or ERROR-FN to be called with a
JSONRPC `:result' or `:error' object, respectively.  If this
doesn't happen after TIMEOUT seconds (defaults to
`jrpc-default-request-timeout'), the caller can expect TIMEOUT-FN
to be called with no arguments.  The default values of SUCCESS-FN,
ERROR-FN and TIMEOUT-FN simply log the events into
`jsonrpc-events-buffer'.

If DEFERRED is non-nil, maybe defer the request to a future time
when the server is thought to be ready according to
`jsonrpc-connection-ready-p' (which see).  The request might
never be sent at all, in case it is overridden in the meantime by
a new request with identical DEFERRED and for the same buffer.
However, in that situation, the original timeout is kept.

Returns nil."
  (apply #'jsonrpc--async-request-1 connection method params args)
  nil)

(cl-defun jsonrpc-request (connection
                           method params &key
                           deferred timeout
                           cancel-on-input
                           cancel-on-input-retval)
  "Make a request to CONNECTION, wait for a reply.
Like `jsonrpc-async-request' for CONNECTION, METHOD and PARAMS,
but synchronous.

Except in the case of a non-nil CANCEL-ON-INPUT (explained
below), this function doesn't exit until anything interesting
happens (success reply, error reply, or timeout).  Furthermore,
it only exits locally (returning the JSONRPC result object) if
the request is successful, otherwise it exits non-locally with an
error of type `jsonrpc-error'.

DEFERRED and TIMEOUT as in `jsonrpc-async-request', which see.

If CANCEL-ON-INPUT is non-nil and the user inputs something while
the function is waiting, then it exits immediately, returning
CANCEL-ON-INPUT-RETVAL.  Any future replies (normal or error) are
ignored."
  (let* ((tag (cl-gensym "jsonrpc-request-catch-tag")) id-and-timer
         cancelled
         (retval
          (unwind-protect
              (catch tag
                (setq
                 id-and-timer
                 (apply
                  #'jsonrpc--async-request-1
                  connection method params
                  :success-fn (lambda (result)
                                (unless cancelled
                                  (throw tag `(done ,result))))
                  :error-fn
                  (jsonrpc-lambda
                      (&key code message data)
                    (unless cancelled
                      (throw tag `(error (jsonrpc-error-code . ,code)
                                         (jsonrpc-error-message . ,message)
                                         (jsonrpc-error-data . ,data)))))
                  :timeout-fn
                  (lambda ()
                    (unless cancelled
                      (throw tag '(error (jsonrpc-error-message . "Timed out")))))
                  `(,@(when deferred `(:deferred ,deferred))
                    ,@(when timeout  `(:timeout  ,timeout)))))
                (cond (cancel-on-input
                       (unwind-protect
                           (let ((inhibit-quit t)) (while (sit-for 30)))
                         (setq cancelled t))
                       `(cancelled ,cancel-on-input-retval))
                      (t (while t (accept-process-output nil 30)))))
            ;; In normal operation, cancellation is handled by the
            ;; timeout function and response filter, but we still have
            ;; to protect against user-quit (C-g) or the
            ;; `cancel-on-input' case.
            (pcase-let* ((`(,id ,timer) id-and-timer))
              (remhash id (jsonrpc--request-continuations connection))
              (remhash (list deferred (current-buffer))
                       (jsonrpc--deferred-actions connection))
              (when timer (cancel-timer timer))))))
    (when (eq 'error (car retval))
      (signal 'jsonrpc-error
              (cons
               (format "request id=%s failed:" (car id-and-timer))
               (cdr retval))))
    (cadr retval)))

(cl-defun jsonrpc-notify (connection method params)
  "Notify CONNECTION of something, don't expect a reply."
  (jsonrpc-connection-send connection
                           :method method
                           :params params))

(define-obsolete-variable-alias 'jrpc-default-request-timeout
  'jsonrpc-default-request-timeout "28.1")

(defconst jsonrpc-default-request-timeout 10
  "Time in seconds before timing out a JSONRPC request.")


;;; Specific to `jsonrpc-process-connection'
;;;

(defclass jsonrpc-process-connection (jsonrpc-connection)
  ((-process
    :initarg :process :accessor jsonrpc--process
    :documentation "Process object wrapped by the this connection.")
   (-expected-bytes
    :accessor jsonrpc--expected-bytes
    :documentation "How many bytes declared by server.")
   (-on-shutdown
    :accessor jsonrpc--on-shutdown
    :initform #'ignore
    :initarg :on-shutdown
    :documentation "Function run when the process dies."))
  :documentation "A JSONRPC connection over an Emacs process.
The following initargs are accepted:

:PROCESS (mandatory), a live running Emacs process object or a
function of no arguments producing one such object.  The process
represents either a pipe connection to locally running process or
a stream connection to a network host.  The remote endpoint is
expected to understand JSONRPC messages with basic HTTP-style
enveloping headers such as \"Content-Length:\".

:ON-SHUTDOWN (optional), a function of one argument, the
connection object, called when the process dies.")

(cl-defmethod initialize-instance ((conn jsonrpc-process-connection) slots)
  (cl-call-next-method)
  (cl-destructuring-bind (&key ((:process proc)) name &allow-other-keys) slots
    ;; FIXME: notice the undocumented bad coupling in the stderr
    ;; buffer name, it must be named exactly like this we expect when
    ;; calling `make-process'.  If there were a `set-process-stderr'
    ;; like there is `set-process-buffer' we wouldn't need this and
    ;; could use a pipe with a process filter instead of
    ;; `after-change-functions'.  Alternatively, we need a new initarg
    ;; (but maybe not a slot).
    (let ((calling-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create (format "*%s stderr*" name))
        (let ((inhibit-read-only t)
              (hidden-name (concat " " (buffer-name))))
          (erase-buffer)
          (buffer-disable-undo)
          (add-hook
           'after-change-functions
           (lambda (beg _end _pre-change-len)
             (cl-loop initially (goto-char beg)
                      do (forward-line)
                      when (bolp)
                      for line = (buffer-substring
                                  (line-beginning-position 0)
                                  (line-end-position 0))
                      do (with-current-buffer (jsonrpc-events-buffer conn)
                           (goto-char (point-max))
                           (let ((inhibit-read-only t))
                             (insert (format "[stderr] %s\n" line))))
                      until (eobp)))
           nil t)
          ;; If we are correctly coupled to the client, the process
          ;; now created should pick up the current stderr buffer,
          ;; which we immediately rename
          (setq proc (if (functionp proc)
                         (with-current-buffer calling-buffer (funcall proc))
                       proc))
          (ignore-errors (kill-buffer hidden-name))
          (rename-buffer hidden-name)
          (process-put proc 'jsonrpc-stderr (current-buffer))
          (setq buffer-read-only t))))
    (setf (jsonrpc--process conn) proc)
    (set-process-buffer proc (get-buffer-create (format " *%s output*" name)))
    (set-process-filter proc #'jsonrpc--process-filter)
    (set-process-sentinel proc #'jsonrpc--process-sentinel)
    (with-current-buffer (process-buffer proc)
      (buffer-disable-undo)
      (set-marker (process-mark proc) (point-min))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only t))
    (process-put proc 'jsonrpc-connection conn)))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-process-connection)
                                       &rest args
                                       &key
                                       _id
                                       method
                                       _params
                                       _result
                                       _error
                                       _partial)
  "Send MESSAGE, a JSON object, to CONNECTION."
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ( (message `(:jsonrpc "2.0" ,@args))
          (json (jsonrpc--json-encode message))
          (headers
           `(("Content-Length" . ,(format "%d" (string-bytes json)))
             ;; ("Content-Type" . "application/vscode-jsonrpc; charset=utf-8")
             )))
    (process-send-string
     (jsonrpc--process connection)
     (cl-loop for (header . value) in headers
              concat (concat header ": " value "\r\n") into header-section
              finally return (format "%s\r\n%s" header-section json)))
    (jsonrpc--log-event connection message 'client)))

(defun jsonrpc-process-type (conn)
  "Return the `process-type' of JSONRPC connection CONN."
  (process-type (jsonrpc--process conn)))

(cl-defmethod jsonrpc-running-p ((conn jsonrpc-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (process-live-p (jsonrpc--process conn)))

(cl-defmethod jsonrpc-shutdown ((conn jsonrpc-process-connection)
                                &optional cleanup)
  "Wait for JSONRPC connection CONN to shutdown.
With optional CLEANUP, kill any associated buffers."
  (unwind-protect
      (cl-loop
       with proc = (jsonrpc--process conn) for i from 0
       while (not (process-get proc 'jsonrpc-sentinel-cleanup-started))
       unless (zerop i) do
       (jsonrpc--warn "Sentinel for %s still hasn't run, deleting it!" proc)
       do
       (delete-process proc)
       (accept-process-output nil 0.1))
    (when cleanup
      (kill-buffer (process-buffer (jsonrpc--process conn)))
      (kill-buffer (jsonrpc-stderr-buffer conn)))))

(defun jsonrpc-stderr-buffer (conn)
  "Get CONN's standard error buffer, if any."
  (process-get (jsonrpc--process conn) 'jsonrpc-stderr))


;;; Private stuff
;;;
(define-error 'jsonrpc-error "jsonrpc-error")

(defalias 'jsonrpc--json-read
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'plist
                           :null-object nil
                           :false-object :json-false))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-object-type 'plist))
        (json-read))))
  "Read JSON object in buffer, move point to end of buffer.")

(defalias 'jsonrpc--json-encode
  (if (fboundp 'json-serialize)
      (lambda (object)
        (json-serialize object
                        :false-object :json-false
                        :null-object nil))
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    (lambda (object)
      (let ((json-false :json-false)
            (json-null nil))
        (json-encode object))))
  "Encode OBJECT into a JSON string.")

(cl-defun jsonrpc--reply
    (connection id &key (result nil result-supplied-p) (error nil error-supplied-p))
  "Reply to CONNECTION's request ID with RESULT or ERROR."
  (apply #'jsonrpc-connection-send connection
         `(:id ,id
               ,@(and result-supplied-p `(:result ,result))
               ,@(and error-supplied-p `(:error ,error)))))

(defun jsonrpc--call-deferred (connection)
  "Call CONNECTION's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (jsonrpc--deferred-actions connection))))
    (jsonrpc--debug connection `(:maybe-run-deferred
                                 ,(mapcar (apply-partially #'nth 2) actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defun jsonrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((connection (process-get proc 'jsonrpc-connection)))
    (jsonrpc--debug connection `(:message "Connection state changed" :change ,change))
    (when (not (process-live-p proc))
      (with-current-buffer (jsonrpc-events-buffer connection)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers
      (maphash (lambda (_id triplet)
                 (pcase-let ((`(,_success ,_error ,timeout) triplet))
                   (when timeout (cancel-timer timeout))))
               (jsonrpc--request-continuations connection))
      (process-put proc 'jsonrpc-sentinel-cleanup-started t)
      (unwind-protect
          ;; Call all outstanding error handlers
          (maphash (lambda (_id triplet)
                     (pcase-let ((`(,_success ,error ,_timeout) triplet))
                       (funcall error '(:code -1 :message "Server died"))))
                   (jsonrpc--request-continuations connection))
        (jsonrpc--message "Server exited with status %s" (process-exit-status proc))
        (delete-process proc)
        (funcall (jsonrpc--on-shutdown connection) connection)))))

(defun jsonrpc--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((inhibit-read-only t)
             (connection (process-get proc 'jsonrpc-connection))
             (expected-bytes (jsonrpc--expected-bytes connection)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        ;; Loop (more than one message might have arrived)
        ;;
        (unwind-protect
            (let (done)
              (while (not done)
                (cond
                 ((not expected-bytes)
                  ;; Starting a new message
                  ;;
                  (setq expected-bytes
                        (and (search-forward-regexp
                              "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                              (+ (point) 100)
                              t)
                             (string-to-number (match-string 1))))
                  (unless expected-bytes
                    (setq done :waiting-for-new-message)))
                 (t
                  ;; Attempt to complete a message body
                  ;;
                  (let ((available-bytes (- (position-bytes (process-mark proc))
                                            (position-bytes (point)))))
                    (cond
                     ((>= available-bytes
                          expected-bytes)
                      (let* ((message-end (byte-to-position
                                           (+ (position-bytes (point))
                                              expected-bytes))))
                        (unwind-protect
                            (save-restriction
                              (narrow-to-region (point) message-end)
                              (let* ((json-message
                                      (condition-case-unless-debug oops
                                          (jsonrpc--json-read)
                                        (error
                                         (jsonrpc--warn "Invalid JSON: %s %s"
                                                        (cdr oops) (buffer-string))
                                         nil))))
                                (when json-message
                                  ;; Process content in another
                                  ;; buffer, shielding proc buffer from
                                  ;; tamper
                                  (with-temp-buffer
                                    (jsonrpc-connection-receive connection
                                                                json-message)))))
                          (goto-char message-end)
                          (delete-region (point-min) (point))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (jsonrpc--expected-bytes connection) expected-bytes))))))

(cl-defun jsonrpc--async-request-1 (connection
                                    method
                                    params
                                    &rest args
                                    &key success-fn error-fn timeout-fn
                                    (timeout jsonrpc-default-request-timeout)
                                    (deferred nil))
  "Does actual work for `jsonrpc-async-request'.

Return a list (ID TIMER).  ID is the new request's ID, or nil if
the request was deferred.  TIMER is a timer object set (or nil, if
TIMEOUT is nil)."
  (pcase-let* ((buf (current-buffer)) (point (point))
               (`(,_ ,timer ,old-id)
                (and deferred (gethash (list deferred buf)
                                       (jsonrpc--deferred-actions connection))))
               (id (or old-id (cl-incf (jsonrpc--next-request-id connection))))
               (make-timer
                (lambda ( )
                  (when timeout
                    (run-with-timer
                     timeout nil
                     (lambda ()
                       (remhash id (jsonrpc--request-continuations connection))
                       (remhash (list deferred buf)
                                (jsonrpc--deferred-actions connection))
                       (if timeout-fn (funcall timeout-fn)
                         (jsonrpc--debug
                          connection `(:timed-out ,method :id ,id
                                                  :params ,params)))))))))
    (when deferred
      (if (jsonrpc-connection-ready-p connection deferred)
          ;; Server is ready, we jump below and send it immediately.
          (remhash (list deferred buf) (jsonrpc--deferred-actions connection))
        ;; Otherwise, save in `jsonrpc--deferred-actions' and exit non-locally
        (unless old-id
          (jsonrpc--debug connection `(:deferring ,method :id ,id :params
                                                  ,params)))
        (puthash (list deferred buf)
                 (list (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (save-excursion (goto-char point)
                                             (apply #'jsonrpc-async-request
                                                    connection
                                                    method params args)))))
                       (or timer (setq timer (funcall make-timer))) id)
                 (jsonrpc--deferred-actions connection))
        (cl-return-from jsonrpc--async-request-1 (list id timer))))
    ;; Really send it
    ;;
    (jsonrpc-connection-send connection
                             :id id
                             :method method
                             :params params)
    (puthash id
             (list (or success-fn
                       (jsonrpc-lambda (&rest _ignored)
                         (jsonrpc--debug
                          connection (list :message "success ignored"
                                           :id id))))
                   (or error-fn
                       (jsonrpc-lambda (&key code message &allow-other-keys)
                         (jsonrpc--debug
                          connection (list
                                      :message
                                      (format "error ignored, status set (%s)"
                                              message)
                                      :id id :error code))))
                   (setq timer (funcall make-timer)))
             (jsonrpc--request-continuations connection))
    (list id timer)))

(defun jsonrpc--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[jsonrpc] %s" (apply #'format format args)))

(defun jsonrpc--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (jsonrpc--log-event
   server (if (stringp format)`(:message ,(format format args)) format)))

(defun jsonrpc--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jsonrpc--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jsonrpc
                     (apply #'format format args)
                     :warning)))

(defun jsonrpc--log-event (connection message &optional type)
  "Log a JSONRPC-related event.
CONNECTION is the current connection.  MESSAGE is a JSON-like
plist.  TYPE is a symbol saying if this is a client or server
originated."
  (let ((max (jsonrpc--events-buffer-scrollback-size connection)))
    (when (or (null max) (cl-plusp max))
      (with-current-buffer (jsonrpc-events-buffer connection)
        (cl-destructuring-bind (&key method id error &allow-other-keys) message
          (let* ((inhibit-read-only t)
                 (subtype (cond ((and method id)       'request)
                                (method                'notification)
                                (id                    'reply)
                                (t                     'message)))
                 (type
                  (concat (format "%s" (or type 'internal))
                          (if type
                              (format "-%s" subtype)))))
            (goto-char (point-max))
            (prog1
                (let ((msg (format "[%s]%s%s %s:\n%s"
                                   type
                                   (if id (format " (id:%s)" id) "")
                                   (if error " ERROR" "")
                                   (current-time-string)
                                   (pp-to-string message))))
                  (when error
                    (setq msg (propertize msg 'face 'error)))
                  (insert-before-markers msg))
              ;; Trim the buffer if it's too large
              (when max
                (save-excursion
                  (goto-char (point-min))
                  (while (> (buffer-size) max)
                    (delete-region (point) (progn (forward-line 1)
                                                  (forward-sexp 1)
                                                  (forward-line 2)
                                                  (point)))))))))))))

(provide 'jsonrpc)
;;; jsonrpc.el ends here
