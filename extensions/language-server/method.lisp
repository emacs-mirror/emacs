(in-package :lem-language-server)

(defvar *debug-on-error* nil)
(defvar *method-classes* '())

(define-condition uninitialized-error (jsonrpc:jsonrpc-error)
  ((code :initform -32002)
   (message :initform "\"initialize\" request has not been called.")))

(defgeneric call-lsp-method (lsp-method json))
(defgeneric call-lsp-method-aux (lsp-method params))

(defun call-with-error-handler (function)
  (if *debug-on-error*
      (funcall function)
      (handler-bind ((error (lambda (condition)
                              (let ((error-message
                                      (with-output-to-string (stream)
                                        (format stream "~&~A~%" condition)
                                        (uiop:print-backtrace :stream stream
                                                              :condition condition))))
                                (log:debug error-message)
                                (return-from call-with-error-handler)))))
        (funcall function))))

(defmacro with-error-handler (() &body body)
  `(call-with-error-handler (lambda () ,@body)))

(defclass lsp-method ()
  ((name :initarg :name
         :reader lsp-method-name)
   (params-type :initarg :params-type
                :reader lsp-method-params-type)))

(defun check-initialized (lsp-method)
  (unless (typep lsp-method 'initialize-request)
    (unless (server-client-capabilities (current-server))
      (error 'uninitialized-error))))

(defmethod call-lsp-method (lsp-method json)
  (log-request (lsp-method-name lsp-method) json)
  (check-initialized lsp-method)
  (with-error-handler ()
    (let* ((params (if-let ((params-type (lsp-method-params-type lsp-method)))
                     (convert-from-json json params-type)
                     json))
           (response (call-lsp-method-aux lsp-method params)))
      (log-response (lsp-method-name lsp-method) response)
      response)))

(defmacro define-request ((class-name method-name)
                          (&optional (params (gensym "params")) params-type)
                          &body body)
  (with-unique-names (instance)
    `(progn
       (pushnew ',class-name *method-classes*)
       (defclass ,class-name (lsp-method)
         ()
         (:default-initargs
          :name ,method-name
          :params-type ',params-type))
       (defmethod call-lsp-method-aux ((,instance ,class-name) ,params)
         ,@body))))

(defun log-request (method-name json)
  (let ((json-string (with-output-to-string (stream)
                       (yason:encode json stream))))
    (log:debug "~A: ~A" method-name json-string)))

(defun log-response (method-name response)
  (let ((json
          (with-output-to-string (output)
            (with-open-stream (stream (yason:make-json-output-stream output))
              (yason:encode response stream)))))
    (log:debug "~A: ~A" method-name json)))
