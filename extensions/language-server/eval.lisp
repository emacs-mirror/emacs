(in-package :lem-language-server)

(lem-lsp-base/type:define-class show-eval-result-params ()
  ((message :type lem-lsp-base/type:lsp-string
            :initarg :message
            :accessor show-eval-result-params-message)
   (type :type lsp:message-type
         :initarg :type
         :accessor show-eval-result-params-type)
   (id :initarg :id
       :accessor show-eval-result-params-id)
   (text-document :initarg :text-document
                  :type lsp:text-document-identifier
                  :accessor show-eval-result-params-text-document)))

(lem-lsp-base/type:define-notification-message lisp/show-eval-result ()
  :message-direction "serverToClient"
  :method "lisp/showEvalResult"
  :params 'show-eval-result-params)

(lem-lsp-base/type:define-class start-eval-params ()
  ((id :initarg :id
       :type lem-lsp-base/type:lsp-uinteger
       :accessor start-eval-params-id)
   (range :initarg :range
          :type lsp:range
          :accessor start-eval-params-range)
   (text-document :initarg :text-document
                  :type lsp:text-document-identifier
                  :accessor start-eval-params-text-document)))

(lem-lsp-base/type:define-notification-message lisp/start-eval ()
  :message-direction "serverToClient"
  :method "lisp/startEval"
  :params 'start-eval-params)

(defun convert-eval-result (value)
  (alexandria:destructuring-ecase value
    ((:ok result)
     (let ((value (or (micros/lsp-api:eval-result-error result)
                      (micros/lsp-api:eval-result-value result)))
           (errorp (not (null (micros/lsp-api:eval-result-error
                               result)))))
       (values value
               (if errorp
                   lsp:message-type-error
                   lsp:message-type-info))))
    ((:abort condition)
     (values condition
             lsp:message-type-error))))

(defun notify-eval-result (value &key (id 0) text-document)
  (multiple-value-bind (message type)
      (convert-eval-result value)
    (notify-log-message type message)
    (notify-to-client (make-instance 'lisp/show-eval-result)
                      (make-instance 'show-eval-result-params
                                     :id id
                                     :type type
                                     :message message
                                     :text-document text-document))))

(defun remote-eval (string package-name &key callback request-id)
  (micros/client:remote-eval
   (server-backend-connection *server*)
   `(micros/lsp-api:eval-for-language-server ,string)
   :package-name package-name
   :callback (lambda (value)
               (with-error-handler ()
                 (funcall callback value)))
   :thread t
   :request-id request-id))

(defun interrupt-eval (request-id)
  (micros/client:interrupt (server-backend-connection *server*) request-id))

(defun eval-last-expression (params)
  (let ((point (text-document-position-params-to-point params))
        (text-document-identifier
          (lsp:text-document-position-params-text-document params)))
    (lem:with-point ((start point :left-inserting)
                     (end point :left-inserting))
      (when (lem:form-offset start -1) ; TODO: nilの場合を考慮する
        (lem:form-offset (lem:move-point end start) 1)
        (let ((string (lem:points-to-string start end))
              (range (points-to-lsp-range start end))
              (request-id (micros/client::new-request-id (server-backend-connection *server*))))
          (notify-to-client (make-instance 'lisp/start-eval)
                            (make-instance 'start-eval-params
                                           :range range
                                           :id request-id
                                           :text-document text-document-identifier))
          (remote-eval string
                       (scan-current-package point)
                       :callback (lambda (value)
                                   (notify-eval-result value
                                                       :id request-id
                                                       :text-document text-document-identifier))
                       :request-id request-id))))))

(defun micros-write-string (string target info)
  (declare (ignore target))
  (with-error-handler ()
    (let ((info (ecase info
                  (:log lsp:message-type-log)
                  (:error lsp:message-type-error))))
      (let ((jsonrpc/connection:*connection* (server-jsonrpc-connection *server*)))
        (notify-log-message info string)))))
