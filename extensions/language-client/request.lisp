(defpackage :lem-language-client/request
  (:use :cl
        :lem-lsp-base/type)
  (:import-from :lem-language-client/client
                :client-connection)
  (:import-from :lem-lsp-base/converter
                :convert-from-json
                :convert-to-json)
  (:export :request
           :request-async
           :execute-command))
(in-package :lem-language-client/request)

(cl-package-locks:lock-package :lem-lsp-mode/request)

(defvar *log-enable* t)
(defvar *log-mutex* (bt2:make-lock))

(defun do-log (string &rest args)
  (when *log-enable*
    (bt2:with-lock-held (*log-mutex*)
      (let ((log-pathname (merge-pathnames "language-client.log" (lem:lem-logdir-pathname))))
        (ensure-directories-exist log-pathname)
        (with-open-file (out log-pathname
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (fresh-line out)
          (apply #'format out string args)
          (terpri out))))))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun do-request-log (method params &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[<-~;->~] request: ~A ~A" (eq from :client) method (pretty-json params)))

(defun do-response-log (response &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[->~;<-~] response: ~A" (eq from :client) (pretty-json response)))

(defun do-error-response-log (condition &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[->~;<-~] response: ~A" (eq from :client) condition))

(defun jsonrpc-call (jsonrpc method params)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (let ((response (jsonrpc:call jsonrpc method params)))
      (do-response-log response)
      response)))

(defun jsonrpc-call-async (jsonrpc method params callback &optional error-callback)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (jsonrpc:call-async jsonrpc
                        method
                        params
                        (lambda (response)
                          (do-response-log response)
                          (funcall callback response))
                        error-callback)))

(defun jsonrpc-notify (jsonrpc method params)
  (do-log "notify: ~A ~A" method (pretty-json params))
  (jsonrpc:notify jsonrpc method params))

(defun coerce-response (request response)
  (convert-from-json response
                     (request-message-result request)))

(defmethod request (client (message request-message) params)
  (coerce-response message
                   (jsonrpc-call (client-connection client)
                                 (request-message-method message)
                                 (convert-to-json params))))

(defmethod request-async (client (message request-message) params callback &optional error-callback)
  (jsonrpc-call-async (client-connection client)
                      (request-message-method message)
                      (convert-to-json params)
                      (lambda (response)
                        (handler-bind ((error (lambda (e)
                                                (log:error "~A"
                                                          (with-output-to-string (stream)
                                                            (uiop:println e stream)
                                                            (uiop:print-backtrace :stream stream
                                                                                  :condition e))))))
                          (let ((value (coerce-response message response)))
                            (funcall callback value))))
                      error-callback))

(defmethod request (client (message notification-message) params)
  (jsonrpc-notify (client-connection client)
                  (notification-message-method message)
                  (convert-to-json params)))

(defun execute-command (client command &rest arguments)
  (request client
           (make-instance 'lsp:workspace/execute-command)
           (make-instance 'lsp:execute-command-params
                          :command command
                          :arguments (coerce arguments 'vector))))
