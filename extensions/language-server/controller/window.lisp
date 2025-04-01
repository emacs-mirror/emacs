(in-package :lem-language-server)

(defun notify-to-client (notification params)
  (assert (typep params (notification-message-params notification)))
  (let ((jsonrpc/connection:*connection* (server-jsonrpc-connection *server*)))
    (let ((method (notification-message-method notification))
          (params (convert-to-json params)))
      (jsonrpc:notify (server-jsonrpc-server *server*) method params))))

(defun notify-show-message (type message)
  (notify-to-client (make-instance 'lsp:window/show-message)
                    (make-instance 'lsp:show-message-params
                                   :type type
                                   :message message)))

(defun notify-log-message (type message)
  (notify-to-client (make-instance 'lsp:window/log-message)
                    (make-instance 'lsp:log-message-params
                                   :type type
                                   :message message)))
