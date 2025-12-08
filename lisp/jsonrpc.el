;;; jsonrpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: processes, languages, extensions
;; Version: 1.0.27
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


;;; Public API
;;;

(defclass jsonrpc-connection ()
  ((name
    :accessor jsonrpc-name
    :initform "anonymous"
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
    :initform nil
    :accessor jsonrpc-last-error
    :documentation "Last JSONRPC error message received from endpoint.")
   (-continuations
    :initform nil
    :accessor jsonrpc--continuations
    :documentation "An alist of request IDs to continuation specs.")
   (-events-buffer
    :initform nil
    :accessor jsonrpc--events-buffer
    :documentation "A buffer pretty-printing the JSONRPC events")
   (-events-buffer-config
    :initform '(:size nil :format full)
    :initarg :events-buffer-config
    :documentation "Plist configuring the events buffer functions.")
   (-deferred-actions
    :initform (make-hash-table :test #'equal)
    :accessor jsonrpc--deferred-actions
    :documentation "Map (DEFERRED BUF) to (FN TIMER ID).  FN is\
a saved DEFERRED `async-request' from BUF, to be sent not later\
than TIMER as ID.")
   (-sync-request-alist ; bug#67945
    :initform nil
    :accessor jsonrpc--sync-request-alist
    :documentation "List of ((ID [ANXIOUS...])) where ID refers  \
to a sync `jsonrpc-request' and each ANXIOUS to another completed\
request that is higher up in the stack but couldn't run.")
   (-next-request-id
    :initform 0
    :accessor jsonrpc--next-request-id
    :documentation "Next number used for a request"))
  :documentation "Base class representing a JSONRPC connection.
The following keyword argument initargs are accepted:

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
:REQUEST-DISPATCHER.

:EVENTS-BUFFER-CONFIG is a plist.  Its `:size' stipulates the
size of the log buffer (0 disables, nil means infinite).  The
`:format' property is a symbol for choosing the log entry format.")

(cl-defmethod initialize-instance :after
  ((c jsonrpc-connection) ((&key (events-buffer-scrollback-size
                                  nil
                                  e-b-s-s-supplied-p)
                                 &allow-other-keys)
                           t))
  (when e-b-s-s-supplied-p
    (warn
     "`:events-buffer-scrollback-size' deprecated.  Use `events-buffer-config'.")
    (with-slots ((plist -events-buffer-config)) c
      (setf plist (copy-sequence plist)
            plist (plist-put plist :size events-buffer-scrollback-size)))))

(cl-defmethod slot-missing ((_c jsonrpc-connection)
                            (_n (eql :events-buffer-scrollback-size))
                            (_op (eql oset))
                            _)
  ;; Yuck!  But this just coerces EIEIO to backward-compatibly accept
  ;; the :e-b-s-s initarg that is no longer associated with a slot
  ;; #pineForCLOS..
  )

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

;;; API optional
(cl-defgeneric jsonrpc-convert-to-endpoint (connection message subtype)
  "Convert MESSAGE to JSONRPCesque message accepted by endpoint.
MESSAGE is a plist, jsonrpc.el's internal representation of a
JSONRPC message.  SUBTYPE is one of `request', `reply' or
`notification'.

Return a plist to be serialized to JSON with `json-serialize' and
transmitted to endpoint."
  ;; TODO: describe representations and serialization in manual and
  ;; link here.
  (:method (_s message subtype)
           `(:jsonrpc "2.0"
                      ,@(if (eq subtype 'reply)
                            ;; true JSONRPC doesn't have `method'
                            ;; fields in responses.
                            (cl-loop for (k v) on message by #'cddr
                                     unless (eq k :method)
                                     collect k and collect v)
                          message))))

;;; API optional
(cl-defgeneric jsonrpc-convert-from-endpoint (connection remote-message)
  "Convert JSONRPC-esque REMOTE-MESSAGE to a plist.
REMOTE-MESSAGE is a plist read with `json-parse'.

Return a plist of jsonrpc.el's internal representation of a
JSONRPC message."
  ;; TODO: describe representations and serialization in manual and
  ;; link here.
  (:method (_s remote-message)
           (cl-loop for (k v) on remote-message by #'cddr
                    unless (eq k :jsonrpc-json)
                    collect k and collect v)))


;;; Convenience
;;;
(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (funcall (if (fboundp 'gensym) 'gensym 'cl-gensym)
                    "jsonrpc-lambda-elem")))
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
  (setf (jsonrpc--continuations connection) nil))

(defvar jsonrpc-inhibit-debug-on-error nil
  "Inhibit `debug-on-error' when answering requests.
Some extensions, notably ert.el, set `debug-on-error' to non-nil,
which makes it hard to test the behavior of catching the Elisp
error and replying to the endpoint with an JSONRPC-error.  This
variable can be set around calls like `jsonrpc-request' to
circumvent that.")

(defun jsonrpc-connection-receive (conn foreign-message)
  "Process FOREIGN-MESSAGE just received from CONN.
This function will destructure MESSAGE and call the appropriate
dispatcher in CONN."
  (cl-destructuring-bind (&rest whole &key method id error params result _jsonrpc)
      (jsonrpc-convert-from-endpoint conn foreign-message)
    (unwind-protect
        (let* ((log-plist (list :json (plist-get foreign-message :jsonrpc-json)
                                :kind (cond ((and method id) 'request)
                                            (method          'notification)
                                            (id              'reply))
                                :message whole
                                :foreign-message foreign-message))
               (response-p (and (null method) id))
               (cont (and response-p (jsonrpc--remove conn id))))
          (cl-remf foreign-message :jsonrpc-json)
          ;; Do this pre-processing of the response so we can always
          ;; log richer information _before_ any non-local calls
          ;; further ahead. Putting the `jsonrpc--event' as
          ;; an unwind-form would make us log after the fact.
          (when cont
            (pcase-let ((`(,_ ,method ,_ ,_ ,_) cont))
              (if (keywordp method)
                  (setq method (substring (symbol-name method) 1)))
              ;; TODO: also set the depth
              (setq whole (plist-put whole :method method))))

          ;; Do the logging
          (apply #'jsonrpc--event conn 'server log-plist)
          (with-slots (last-error
                       (rdispatcher -request-dispatcher)
                       (ndispatcher -notification-dispatcher)
                       (sr-alist -sync-request-alist))
              conn
            (setf last-error error)
            (cond
             (;; A remote response whose request has been canceled
              ;; (i.e. timeout or C-g)
              ;;
              (and response-p (null cont))
              (jsonrpc--event
               conn 'internal
               :log-text
               (format "Response to request %s which has been canceled"
                       id)
               :id id)
              ;; TODO: food for thought: this seems to be also where
              ;; notifying the server of the cancellation would come
              ;; in.
              )
             (;; A remote response that can't run yet (bug#67945)
              (and response-p
                   (and sr-alist (not (eq id (caar sr-alist)))))
              (jsonrpc--event
               conn 'internal
               :log-text
               (format "anxious continuation to %s can't run, held up by %s"
                       id
                       (mapcar #'car sr-alist)))
              (push (cons cont (list result error))
                    (cdr (car sr-alist))))
             (;; A remote response that can continue now
              response-p
              (jsonrpc--continue conn id cont result error))
             (;; A remote request
              (and method id)
              (let* ((debug-on-error (and debug-on-error
                                          (not jsonrpc-inhibit-debug-on-error)))
                     (reply
                      (condition-case-unless-debug _ignore
                          (condition-case oops
                              `(:result ,(funcall rdispatcher conn (intern method)
                                                  params))
                            (jsonrpc-error
                             `(:error
                               (:code
                                ,(or (alist-get 'jsonrpc-error-code (cdr oops))
                                     -32603)
                                :message ,(or (alist-get 'jsonrpc-error-message
                                                         (cdr oops))
                                              "Internal error")))))
                        (error
                         '(:error (:code -32603 :message "Internal error"))))))
                (apply #'jsonrpc--reply conn id method reply)))
             (;; A remote notification
              method
              (funcall ndispatcher conn (intern method) params))
             (t
              (jsonrpc--event conn 'internal
                              :log-text "Malformed message" )))))
      (jsonrpc--call-deferred conn))))


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
                  (jsonrpc-error-code . -32603)
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
The JSONRPC request is formed by METHOD, a symbol; and PARAMS, a JSON
object value as described in `json-serialize' (which see).

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

PARAMS can also be the keyword `:jsonrpc-omit', in which case the
JSONRPC request object is formed witout a `params' entry.

Returns a list whose first element is an integer identifying the request
as specified in the JSONRPC 2.0 spec."
  (apply #'jsonrpc--async-request-1 connection method params args))

(cl-defun jsonrpc-request (connection
                           method params &key
                           deferred timeout
                           cancel-on-input
                           cancel-on-input-retval)
  "Make a request to CONNECTION, synchronously wait for a reply.
CONNECTION, METHOD and PARAMS as in `jsonrpc-async-request' (which see).

Except in the case of a non-nil CANCEL-ON-INPUT (explained
below), this function doesn't exit until anything interesting
happens (success reply, error reply, or timeout).  Furthermore,
it only exits locally (returning the JSONRPC result object) if
the request is successful, otherwise it exits non-locally with an
error of type `jsonrpc-error'.

DEFERRED and TIMEOUT as in `jsonrpc-async-request', which see.

If CANCEL-ON-INPUT is non-nil and the user inputs something while the
function is waiting, the function locally exits immediately returning
CANCEL-ON-INPUT-RETVAL.  Any future replies to the request coming from
the remote endpoint (normal or error) are ignored.  If CANCEL-ON-INPUT
is a function, it is invoked with one argument, an integer identifying
the canceled request as specified in the JSONRPC 2.0 spec.  Callers may
use this function to issue a cancel notification to the endpoint, thus
preventing it from continuing to work on the now-cancelled request."
  (let* ((tag (funcall (if (fboundp 'gensym) 'gensym 'cl-gensym)
                       "jsonrpc-request-catch-tag"))
         id-and-timer
         canceled
         (throw-on-input nil)
         (retval
          (unwind-protect
              (catch tag
                (setq
                 id-and-timer
                 (apply
                  #'jsonrpc--async-request-1
                  connection method params
                  :sync-request t
                  :success-fn (lambda (result)
                                (unless canceled
                                  (throw tag `(done ,result))))
                  :error-fn
                  (jsonrpc-lambda
                      (&key code message data)
                    (unless canceled
                      (throw tag `(error (jsonrpc-error-code . ,code)
                                         (jsonrpc-error-message . ,message)
                                         (jsonrpc-error-data . ,data)))))
                  :timeout-fn
                  (lambda ()
                    (unless canceled
                      (throw tag '(error (jsonrpc-error-message . "Timed out")))))
                  `(,@(when deferred `(:deferred ,deferred))
                    ,@(when timeout  `(:timeout  ,timeout)))))
                (cond (cancel-on-input
                       (unwind-protect
                           (let ((inhibit-quit t)) (while (sit-for 30)))
                         (setq canceled t))
                       (when (functionp cancel-on-input)
                         (funcall cancel-on-input (car id-and-timer)))
                       `(canceled ,cancel-on-input-retval))
                      (t (while t (accept-process-output nil 30)))))
            ;; In normal operation, continuations for error/success is
            ;; handled by `jsonrpc--continue'.  Timeouts also remove
            ;; the continuation...
            (pcase-let* ((`(,id ,_) id-and-timer))
              ;; ...but we still have to guard against exist explicit
              ;; user-quit (C-g) or the `cancel-on-input' case, so
              ;; discard the continuation.
              (jsonrpc--remove connection id (list deferred (current-buffer)))
              ;; ...finally, whatever may have happened to this sync
              ;; request, it might have been holding up any outer
              ;; "anxious" continuations.  The following ensures we
              ;; call them.
              (jsonrpc--continue connection id)))))
    (when (eq 'error (car retval))
      (signal 'jsonrpc-error
              (cons
               (format "request id=%s failed:" (car id-and-timer))
               (cdr retval))))
    (cadr retval)))

(cl-defun jsonrpc-notify (connection method params)
  "Notify CONNECTION of something, don't expect a reply.
CONNECTION, METHOD and PARAMS as in `jsonrpc-async-request' (which see)."
  (apply #'jsonrpc-connection-send connection
         :method method
         (unless (eq params :jsonrpc-omit) `(:params ,params))))

(define-obsolete-variable-alias 'jrpc-default-request-timeout
  'jsonrpc-default-request-timeout "28.1")

(defgroup jsonrpc nil
  "JSON-RPC customization."
  :prefix "jsonrpc-"
  :group 'comm)

(defcustom jsonrpc-default-request-timeout 10
  "Time in seconds before timing out a JSON-RPC request without response."
  :version "30.1"
  :type 'number
  :safe 'numberp
  :group 'jsonrpc)


;;; Specific to `jsonrpc-process-connection'
;;;

(defclass jsonrpc-process-connection (jsonrpc-connection)
  ((-process
    :initarg :process :accessor jsonrpc--process
    :documentation "Process object wrapped by the this connection.")
   (-expected-bytes
    :initform nil
    :accessor jsonrpc--expected-bytes
    :documentation "How many bytes declared by server.")
   (-on-shutdown
    :accessor jsonrpc--on-shutdown
    :initform #'ignore
    :initarg :on-shutdown
    :documentation "Function run when the process dies.")
   (-autoport-inferior
    :initform nil
    :documentation "Used by `jsonrpc-autoport-bootstrap'."))
  :documentation "A JSONRPC connection over an Emacs process.
The following initargs are accepted:

:PROCESS (mandatory), a live running Emacs process object or a
function producing one such object.  If a function, it is passed
the `jsonrpc-process-connection' object.  The process represents
either a pipe connection to locally running process or a stream
connection to a network host.  The remote endpoint is expected to
understand JSONRPC messages with basic HTTP-style enveloping
headers such as \"Content-Length:\".

:ON-SHUTDOWN (optional), a function of one argument, the
connection object, called when the process dies.")

(cl-defmethod initialize-instance :after ((conn jsonrpc-process-connection) slots)
  (cl-destructuring-bind (&key ((:process proc)) name &allow-other-keys) slots
    ;; FIXME: notice the undocumented bad coupling in the stderr
    ;; buffer name, it must be named exactly like this we expect when
    ;; calling `make-process'.  If there were a `set-process-stderr'
    ;; like there is `set-process-buffer' we wouldn't need this and
    ;; could use a pipe with a process filter instead of
    ;; `after-change-functions'.  Alternatively, we need a new initarg
    ;; (but maybe not a slot).
    (let* ((stderr-buffer-name (format "*%s stderr*" name))
           (stderr-buffer (jsonrpc--forwarding-buffer stderr-buffer-name "[stderr] " conn))
           (hidden-name (concat " " stderr-buffer-name)))
      ;; If we are correctly coupled to the client, the process now
      ;; created should pick up the `stderr-buffer' just created, which
      ;; we immediately rename
      (setq proc (if (functionp proc)
                     (if (zerop (cdr (func-arity proc)))
                         (funcall proc)
                       (funcall proc conn))
                   proc))
      (with-current-buffer stderr-buffer
        (ignore-errors (kill-buffer hidden-name))
        (rename-buffer hidden-name)
        (setq buffer-read-only t))
      (process-put proc 'jsonrpc-stderr stderr-buffer))
    (setf (jsonrpc--process conn) proc)
    (set-process-buffer proc (get-buffer-create (format " *%s output*" name)))
    (set-process-filter proc #'jsonrpc--process-filter)
    (set-process-sentinel proc #'jsonrpc--process-sentinel)
    (set-process-coding-system proc 'binary 'binary)
    (with-current-buffer (process-buffer proc)
      (set-buffer-multibyte nil)
      (buffer-disable-undo)
      (set-marker (process-mark proc) (point-min))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only t))
    (process-put proc 'jsonrpc-connection conn)))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-process-connection)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error
                                       _partial)
  "Send MESSAGE, a JSON object, to CONNECTION."
  (when method
    ;; sanitize method into a string
    (setq args
          (plist-put args :method
                     (cond ((keywordp method) (substring (symbol-name method) 1))
                           ((symbolp method) (symbol-name method))
                           ((stringp method) method)
                           (t (error "[jsonrpc] invalid method %s" method))))))
  (let* ((kind (cond ((or result-supplied-p error) 'reply)
                     (id 'request)
                     (method 'notification)))
         (converted (jsonrpc-convert-to-endpoint connection args kind))
         (json (jsonrpc--json-encode converted)))
    (process-send-string
     (jsonrpc--process connection)
     (concat "Content-Length: " (number-to-string (string-bytes json)) "\r\n"
              "\r\n" json))
    (jsonrpc--event
     connection
     'client
     :json json
     :kind  kind
     :message args
     :foreign-message converted)))

(defun jsonrpc-process-type (conn)
  "Return the `process-type' of JSONRPC connection CONN."
  (process-type (jsonrpc--process conn)))

(cl-defmethod jsonrpc-running-p ((conn jsonrpc-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (process-live-p (jsonrpc--process conn)))

(cl-defmethod jsonrpc-shutdown ((conn jsonrpc-process-connection)
                                &optional cleanup)
  "Wait for JSONRPC connection CONN to shutdown.
With optional CLEANUP, kill any associated buffers.
If CONN is not shutdown within a reasonable amount of time, warn
and delete the network process."
  (unwind-protect
      (cl-loop
       with proc = (jsonrpc--process conn) for i from 0
       while (not (process-get proc 'jsonrpc-sentinel-cleanup-started))
       unless (zerop i) do
       (jsonrpc--warn "Sentinel for %s still hasn't run, deleting it!" proc)
       (delete-process proc)
       do
       ;; Let sentinel have a chance to run
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
    (declare-function json-read-from-string "json" (string))
    (lambda ()
      (let ((json-object-type 'plist))
        ;; `json-read' can't be used because the old json API requires
        ;; decoded input.
        (prog1
            (json-read-from-string
             (decode-coding-string
              (buffer-substring-no-properties (point) (point-max))
              'utf-8-unix t))
          (goto-char (point-max))))))
  "Read JSON object in (binary unibyte) buffer from point.
Move point to end of buffer.")

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
    (connection id method &key (result nil result-supplied-p) (error nil error-supplied-p))
  "Reply to CONNECTION's request ID with RESULT or ERROR."
  (apply #'jsonrpc-connection-send connection
         `(:id ,id
               ,@(and result-supplied-p `(:result ,result))
               ,@(and error-supplied-p `(:error ,error))
               :method ,method)))

(defun jsonrpc--call-deferred (connection)
  "Call CONNECTION's deferred actions, who may again defer themselves."
  (when-let* ((actions (hash-table-values (jsonrpc--deferred-actions connection))))
    (jsonrpc--event
     connection 'internal
     :log-text (format "re-attempting deferred requests %s"
                       (mapcar (apply-partially #'nth 2) actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defun jsonrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((connection (process-get proc 'jsonrpc-connection)))
    (jsonrpc--debug connection "Connection state change: `%s'" change)
    (when (not (process-live-p proc))
      (with-current-buffer (jsonrpc-events-buffer connection)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers
      (mapc (jsonrpc-lambda (_id _method _success-fn _error-fn timer)
              (when timer (cancel-timer timer)))
            (jsonrpc--continuations connection))
      (maphash (lambda (_ triplet)
                 (pcase-let ((`(,_ ,timer ,_) triplet))
                   (when timer (cancel-timer timer))))
               (jsonrpc--deferred-actions connection))
      (process-put proc 'jsonrpc-sentinel-cleanup-started t)
      (unwind-protect
          ;; Call all outstanding error handlers
          (mapc (jsonrpc-lambda (_id _method _success-fn error-fn _timer)
                  (funcall error-fn '(:code -1 :message "Server died")))
                (jsonrpc--continuations connection))
        (jsonrpc--message "Server exited with status %s" (process-exit-status proc))
        (delete-process proc)
        (when-let* ((p (slot-value connection '-autoport-inferior))) (delete-process p))
        (funcall (jsonrpc--on-shutdown connection) connection)))))

(defvar jsonrpc--in-process-filter nil
  "Non-nil if inside `jsonrpc--process-filter'.")

(cl-defun jsonrpc--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when jsonrpc--in-process-filter
    ;; Problematic recursive process filters may happen if
    ;; `jsonrpc-connection-receive', called by us, eventually calls
    ;; client code which calls `process-send-string' (which see) to,
    ;; say send a follow-up message.  If that happens to writes enough
    ;; bytes for pending output to be received, we will lose JSONRPC
    ;; messages.  In that case, remove recursiveness by re-scheduling
    ;; ourselves to run from within a timer as soon as possible
    ;; (bug#60088)
    (run-at-time 0 nil #'jsonrpc--process-filter proc string)
    (cl-return-from jsonrpc--process-filter))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((conn (process-get proc 'jsonrpc-connection))
             (expected-bytes (jsonrpc--expected-bytes conn)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (let ((inhibit-read-only t)) (insert string))
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
                              (rx bol "Content-Length: " (group (+ digit))
                                  "\r\n"
                                  (* (* (not (in ":\n"))) ": "
                                     (* (not (in "\r\n"))) "\r\n")
                                  "\r\n")
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
                                              expected-bytes)))
                             message
                             )
                        (unwind-protect
                            (save-restriction
                              (narrow-to-region (point) message-end)
                              (setq message
                                    (condition-case-unless-debug oops
                                        (jsonrpc--json-read)
                                      (error
                                       (jsonrpc--warn "Invalid JSON: %s %s"
                                                      (cdr oops) (buffer-string))
                                       nil)))
                              (when message
                                (setq message
                                      (plist-put message :jsonrpc-json
                                                 (buffer-string)))
                                ;; Put new messages at the front of the queue,
                                ;; this is correct as the order is reversed
                                ;; before putting the timers on `timer-list'.
                                (push message
                                      (process-get proc 'jsonrpc-mqueue))))
                          (goto-char message-end)
                          (let ((inhibit-read-only t))
                            (delete-region (point-min) (point)))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter, which
          ;; may well be a recursive one stemming from the tail call
          ;; to `jsonrpc-connection-receive' below (bug#60088).
          ;;
          (setf (jsonrpc--expected-bytes conn) expected-bytes)
          ;; Now, time to notify user code of one or more messages in
          ;; order.  Very often `jsonrpc-connection-receive' will exit
          ;; non-locally (typically the reply to a request), so do
          ;; this all this processing in top-level loops timer.
          (cl-loop
           ;; `timer-activate' orders timers by time, which is an
           ;; very expensive operation when jsonrpc-mqueue is large,
           ;; therefore the time object is reused for each timer
           ;; created.
           with time = (current-time)
           for msg = (pop (process-get proc 'jsonrpc-mqueue)) while msg
           do (let ((timer (timer-create)))
                (timer-set-time timer time)
                (timer-set-function timer
                                    (lambda (conn msg)
                                      (with-temp-buffer
                                        (jsonrpc-connection-receive conn msg)))
                                    (list conn msg))
                (timer-activate timer))))))))

(defun jsonrpc--remove (conn id &optional deferred-spec)
  "Cancel CONN's continuations for ID, including its timer, if it exists.
Also cancel \"deferred actions\" if DEFERRED-SPEC.
Return the full continuation (ID SUCCESS-FN ERROR-FN TIMER)"
  (with-slots ((conts -continuations) (defs -deferred-actions)) conn
    (if deferred-spec (remhash deferred-spec defs))
    (when-let* ((ass (assq id conts)))
      (cl-destructuring-bind (_ _ _ _ timer) ass
        (when timer (cancel-timer timer)))
      (setf conts (delete ass conts))
      ass)))

(defun jsonrpc--schedule (conn id method success-fn error-fn timer)
  (push (list id method success-fn error-fn timer)
        (jsonrpc--continuations conn)))

(defun jsonrpc--continue (conn id &optional cont result error)
  (pcase-let* ((`(,cont-id ,_method ,success-fn ,error-fn ,_timer)
                cont)
               (head (pop (jsonrpc--sync-request-alist conn)))
               (anxious (cdr head)))
    (cond
     (anxious
      (when (not (= (car head) id)) ; sanity check
        (error "Internal error: please report this bug"))
      ;; If there are "anxious" `jsonrpc-request' continuations
      ;; that should already have been run, they should run now.
      ;; The main continuation -- if it exists -- should run
      ;; before them.  This order is important to preserve the
      ;; throw to the catch tags in `jsonrpc-request' in
      ;; order (bug#67945).
      (cl-flet ((later (f arg) (run-at-time 0 nil f arg)))
        (when cont-id
          (if error (later error-fn error)
            (later success-fn result)))
        (cl-loop
         for (acont ares aerr) in anxious
         for (anx-id _method success-fn error-fn) = acont
         do (jsonrpc--event
             conn 'internal
             :log-text (format "anxious continuation to %s running now" anx-id))
         if aerr do (later error-fn aerr)
         else do (later success-fn ares))))
     (cont-id
      ;; Else, just run the normal one, with plain funcall.
      (if error (funcall error-fn error)
        (funcall success-fn result)))
     (t
      ;; For clarity.  This happens if the `jsonrpc-request' was
      ;; canceled
      ))))

(cl-defun jsonrpc--async-request-1 (connection
                                    method
                                    params
                                    &rest args
                                    &key success-fn error-fn timeout-fn
                                    (timeout jsonrpc-default-request-timeout)
                                    (deferred nil)
                                    (sync-request nil))
  "Helper for `jsonrpc-request' and `jsonrpc-async-request'.

Return a list (ID TIMER).  ID is the new request's ID, or nil if
the request was deferred.  TIMER is a timer object set (or nil, if
TIMEOUT is nil)."
  (pcase-let* ((buf (current-buffer)) (point (point))
               (`(,_ ,timer ,old-id)
                (and deferred (gethash (list deferred buf)
                                       (jsonrpc--deferred-actions connection))))
               (id (or old-id (cl-incf (jsonrpc--next-request-id connection))))
               (maybe-timer
                (lambda ()
                  (when timeout
                    (or timer
                        (setq
                         timer
                         (run-with-timer
                          timeout nil
                          (lambda ()
                            (jsonrpc--remove connection id (list deferred buf))
                            (jsonrpc--event
                             connection 'internal
                             :log-text (format "timed-out request '%s'" method)
                             :id id)
                            (when timeout-fn (funcall timeout-fn))))))))))
    (when deferred
      (if (jsonrpc-connection-ready-p connection deferred)
          ;; Server is ready, we jump below and send it immediately.
          (remhash (list deferred buf) (jsonrpc--deferred-actions connection))
        ;; Otherwise, save in `jsonrpc--deferred-actions' and exit non-locally
        (unless old-id
          (jsonrpc--event
           connection 'internal
           :log-text (format "deferring request '%s'" method)
           :id id))
        (puthash (list deferred buf)
                 (list (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (save-excursion (goto-char point)
                                             (apply #'jsonrpc--async-request-1
                                                    connection
                                                    method params args)))))
                       (funcall maybe-timer) id)
                 (jsonrpc--deferred-actions connection))
        (cl-return-from jsonrpc--async-request-1 (list id timer))))
    ;; Really send it thru the wire
    ;;
    (apply #'jsonrpc-connection-send connection
           :id id
           :method method
           (unless (eq params :jsonrpc-omit) `(:params ,params)))
    ;; Setup some control structures
    ;;
    (when sync-request
      (push (list id) (jsonrpc--sync-request-alist connection)))

    (jsonrpc--schedule
     connection id method
     (or success-fn
         (lambda (&rest _ignored)
           (jsonrpc--event
            connection 'internal
            :log-text (format "success ignored")
            :id id)))
     (or error-fn
         (jsonrpc-lambda (&key code message &allow-other-keys)
           (jsonrpc--event
            connection 'internal
            :log-text (format "error %s ignored: %s ignored"
                              code message)
            :id id)))
     (funcall maybe-timer))
    (list id timer)))

(defun jsonrpc--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[jsonrpc] %s" (apply #'format format args)))

(defun jsonrpc--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (with-current-buffer (jsonrpc-events-buffer server)
    (jsonrpc--log-event
     server 'internal
     :log-text (apply #'format format args)
     :type 'debug)))

(defun jsonrpc--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jsonrpc--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jsonrpc
                     (apply #'format format args)
                     :warning)))

(cl-defun jsonrpc--event (connection
                          origin
                          &rest plist
                          &key _kind _json _message _foreign-message _log-text
                          &allow-other-keys)
  (with-current-buffer (jsonrpc-events-buffer connection)
    (run-hook-wrapped 'jsonrpc-event-hook
                      (lambda (fn)
                        (condition-case oops
                            (apply fn connection origin plist)
                          (error
                           (jsonrpc--message "event hook '%s' errored (%s).  Removing it"
                                             fn oops)
                           (remove-hook 'jsonrpc-event-hook fn)))))))

(defun jsonrpc--limit-buffer-size (max-size)
  "Limit the current buffer to MAX-SIZE by eating lines at the beginning.
Do nothing if MAX-SIZE is nil."
  (when max-size
    (while (> (buffer-size) max-size)
      (delete-region
       (point-min)
       (save-excursion
         ;; Remove 1/4, so that the cost is O(1) amortised, since each
         ;; call to `delete-region' will move the buffer contents twice.
         (goto-char (+ (point-min) (/ (buffer-size) 4)))
         (forward-line)
         (point))))))

(defvar jsonrpc-event-hook (list #'jsonrpc--log-event)
  "Hook run when JSON-RPC events are emitted.
This hooks runs in the events buffer of every  `jsonrpc-connection'
when an event is originated by either endpoint.  Each hook function
is passed the arguments described by the lambda list:

  (CONNECTION ORIGIN &key JSON KIND MESSAGE FOREIGN-MESSAGE LOG-TEXT
                     &allow-other-keys)

  CONNECTION       the `jsonrpc-connection' instance.
  ORIGIN           one of the symbols `client' ,`server'.
  JSON             the raw JSON string content.
  KIND             one of the symbols `request' ,`notification',
                   `reply'.
  MESSAGE          a plist representing the exchanged message in
                   jsonrpc.el's internal format
  FOREIGN-MESSAGE  a plist representing the exchanged message in
                   the remote endpoint's format.
  LOG-TEXT         text used for events of `internal' origin.
  ID               id of a message that this event refers to.
  TYPE             `error', `debug' or the default `info'.

Except for CONNECTION and ORIGIN all other keys are optional.
Unlisted keys may appear in the plist.

Do not use this hook to write JSON-RPC protocols, use other parts
of the API instead.")

(cl-defun jsonrpc--log-event (connection origin
                                         &key _kind message
                                         foreign-message log-text json
                                         type ((:id ref-id))
                                         &allow-other-keys)
  "Log a JSONRPC-related event.  Installed in `jsonrpc-event-hook'."
  (let* ((props (slot-value connection '-events-buffer-config))
         (max (plist-get props :size))
         (format (plist-get props :format)))
    (when (or (null max) (cl-plusp max))
      (cl-destructuring-bind (&key method id error &allow-other-keys) message
        (let* ((inhibit-read-only t)
               (depth (length
                       (jsonrpc--sync-request-alist connection)))
               (preamble (format "[jsonrpc] %s[%s]%s "
                                 (pcase type ('error "E") ('debug "D")
                                        (_ (pcase origin
                                             ('internal "i")
                                             (_ "e"))))
                                 (format-time-string "%H:%M:%S.%3N")
                                 (if (eq origin 'internal)
                                     (if ref-id (format " [%s]" ref-id) "")
                                   (format " %s%s %s%s"
                                           (make-string (* 2 depth) ? )
                                           (pcase origin
                                             ('client "-->")
                                             ('server "<--")
                                             (_ ""))
                                           (or method "")
                                           (if id (format "[%s]" id) "")))))
               (msg
                (pcase format
                  ('full  (format "%s%s\n" preamble (or json log-text)))
                  ('short (format "%s%s\n" preamble (or log-text "")))
                  (_
                   (format "%s%s" preamble
                           (or (and foreign-message
                                    (let ((lisp-indent-function ;bug#68072
                                           #'lisp-indent-function))
                                      (concat "\n" (pp-to-string
                                                    foreign-message))))
                               (concat log-text "\n")))))))
          (goto-char (point-max))
          ;; XXX: could use `run-at-time' to delay server logs
          ;; slightly to play nice with verbose servers' stderr.
          (when error
            (setq msg (propertize msg 'face 'error)))
          (insert-before-markers msg)
          (jsonrpc--limit-buffer-size max))))))

(defun jsonrpc--forwarding-buffer (name prefix conn)
  "Helper for `jsonrpc-process-connection' helpers.
Make a stderr buffer named NAME, forwarding lines prefixed by
PREFIX to CONN's events buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (fundamental-mode)
      (erase-buffer)
      (buffer-disable-undo)
      (add-hook
       'after-change-functions
       (lambda (beg _end _pre-change-len)
         (let* ((props (slot-value conn '-events-buffer-config))
                (max (plist-get props :size)))
           (unless (eql max 0)
             (cl-loop initially (goto-char beg)
                      do (forward-line)
                      while (bolp)
                      for line = (buffer-substring
                                  (line-beginning-position 0)
                                  (line-end-position 0))
                      do (with-current-buffer (jsonrpc-events-buffer conn)
                           (goto-char (point-max))
                           (let ((inhibit-read-only t))
                             (insert
                              (propertize (format "%s %s\n" prefix line)
                                          'face 'shadow))
                             (jsonrpc--limit-buffer-size max)))
                      until (eobp)))))
       nil t))
    (current-buffer)))


;;;; More convenience utils
(cl-defun jsonrpc-autoport-bootstrap (name contact
                                           &key connect-args)
  "Use CONTACT to start network server, then connect to it.

Return function suitable for the :PROCESS initarg of
`jsonrpc-process-connection' (which see).

CONTACT is a list where all the elements are strings except for
one, which is usuallky the keyword `:autoport'.

When the returned function is called it will start a program
using a command based on CONTACT, where `:autoport' is
substituted by a locally free network port.  Thereafter, a
network is made to this port.

Instead of the keyword `:autoport', a cons cell (:autoport
FORMAT-FN) is also accepted.  In that case FORMAT-FN is passed
the port number and should return a string used for the
substitution.

The internal processes and control buffers are named after NAME.

CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (lambda (conn)
    (let* ((port-probe (make-network-process :name "jsonrpc-port-probe-dummy"
                                             :server t
                                             :host "localhost"
                                             :service 0))
           (port-number (unwind-protect
                            (process-contact port-probe :service)
                          (delete-process port-probe)))
           (inferior-buffer (jsonrpc--forwarding-buffer
                             (format " *%s inferior output*" name)
                             "[inferior]"
                             conn))
           (cmd (cl-loop for e in contact
                         if (eq e :autoport) collect (format "%s" port-number)
                         else if (eq (car-safe e) :autoport)
                         collect (funcall (cdr e) port-number)
                         else collect e))
           inferior np)
      (unwind-protect
          (progn
            (message "[jsonrpc] Attempting to start `%s'"
                     (string-join cmd " "))
            (setq inferior
                  (make-process
                   :name (format "inferior (%s)" name)
                   :buffer inferior-buffer
                   :noquery t
                   :command cmd))
            (setq np
                  (cl-loop
                   repeat 10 for i from 0
                   do (accept-process-output nil 0.5)
                   while (process-live-p inferior)
                   do (message
                       "[jsonrpc] %sTrying to connect to localhost:%s (attempt %s)"
                       (if (zerop i) "Started.  " "")
                       port-number (1+ i))
                   thereis (ignore-errors
                             (apply #'open-network-stream
                                    (format "autostart (%s)" name)
                                    nil
                                    "localhost" port-number connect-args))))
            (setf (slot-value conn '-autoport-inferior) inferior)
            np)
        (cond ((and (process-live-p np)
                    (process-live-p inferior))
               (message "[jsonrpc] Done, connected to %s!" port-number))
              (t
               (when inferior (delete-process inferior))
               (when np (delete-process np))
               (error "[jsonrpc] Could not start and/or connect")))))))

(defun jsonrpc-continuation-count (conn)
  "Number of outstanding continuations for CONN."
  (length (jsonrpc--continuations conn)))

(provide 'jsonrpc)
;;; jsonrpc.el ends here
