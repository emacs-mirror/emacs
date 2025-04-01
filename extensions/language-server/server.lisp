(in-package :lem-language-server)

(defvar *server*)

(defun current-server () *server*)

(defgeneric start-server (server))
(defgeneric run-backend (server))
(defgeneric swank-port (server))
(defgeneric micros-port (server))
(defgeneric exit-server (server status-code))
(defgeneric remote-eval-sync (server expression package-name))

(defmethod start-server :before (server)
  (setf *server* server)
  (setf micros/client:*write-string-function* 'micros-write-string)
  (expose-all-methods server)
  (unless (server-backend-connection server)
    (setf (server-backend-connection server)
          (run-backend server))))

(defclass server ()
  ((jsonrpc-server :initform (jsonrpc:make-server)
                   :reader server-jsonrpc-server)
   (client-capabilities :initform nil :accessor server-client-capabilities)
   (shutdown-request-received :initform nil
                              :accessor server-shutdown-request-received-p)
   (backend-connection :initform nil
                       :accessor server-backend-connection)))

(defclass mock-server (server)
  ((exit-status :initform nil
                :accessor mock-server-exit-status)))

(defclass tcp-server (server)
  ((port :initarg :port
         :reader tcp-server-port)))

(defclass stdio-server (server)
  ())

(defmethod start-server ((server mock-server))
  nil)

(defmethod start-server ((server tcp-server))
  (with-yason-bindings ()
    (jsonrpc:server-listen (server-jsonrpc-server server)
                           :mode :tcp
                           :port (tcp-server-port server))))

(defmethod start-server ((server stdio-server))
  (with-yason-bindings ()
    (jsonrpc:server-listen (server-jsonrpc-server server)
                           :mode :stdio)))

(defmethod expose-all-methods ((server server))
  (loop :for class :in *method-classes*
        :for instance := (make-instance class)
        :do (jsonrpc:expose (server-jsonrpc-server server)
                            (lsp-method-name instance)
                            (curry #'call-lsp-method instance))))

(defun start-mock-server ()
  (start-server (make-instance 'mock-server)))

(defun start-tcp-server (port)
  (start-server (make-instance 'tcp-server :port port)))

(defun start-stdio-server ()
  (start-server (make-instance 'stdio-server)))

(defun run-backend-internal (deny-port)
  (let ((hostname (config :backend-hostname))
        (port (config :backend-port)))
    (if (and hostname port)
        (micros/client:connect hostname port)
        (micros/client:start-server-and-connect deny-port))))

(defmethod run-backend ((server tcp-server))
  (run-backend-internal (tcp-server-port server)))

(defmethod run-backend ((server stdio-server))
  (run-backend-internal nil))

(defmethod run-backend ((server mock-server))
  nil)

(defun swank-port-internal (server)
  (micros/client:connection-swank-port
   (server-backend-connection server)))

(defmethod swank-port ((server tcp-server))
  (swank-port-internal server))

(defmethod swank-port ((server stdio-server))
  (swank-port-internal server))

(defmethod swank-port ((server mock-server))
  nil)

(defun micros-port-internal (server)
  (micros/client::connection-port
   (server-backend-connection server)))

(defmethod micros-port ((server tcp-server))
  (micros-port-internal server))

(defmethod micros-port ((server stdio-server))
  (micros-port-internal server))

(defmethod micros-port ((server mock-server))
  nil)

(defmethod exit-server ((server tcp-server) status-code)
  (uiop:quit status-code))

(defmethod exit-server ((server stdio-server) status-code)
  (uiop:quit status-code))

(defmethod exit-server ((server mock-server) status-code)
  (setf (mock-server-exit-status server) status-code))

(defun remote-eval-sync-internal (server expression package-name)
  (micros/client:remote-eval-sync (server-backend-connection server)
                                  expression
                                  :package-name package-name))

(defmethod remote-eval-sync ((server tcp-server) expression package-name)
  (remote-eval-sync-internal server expression package-name))

(defmethod remote-eval-sync ((server stdio-server) expression package-name)
  (remote-eval-sync-internal server expression package-name))

(defmethod remote-eval-sync ((server mock-server) expression package-name)
  )

(defmethod server-jsonrpc-connection ((server server))
  (jsonrpc::transport-connection (jsonrpc::jsonrpc-transport (server-jsonrpc-server server))))

(defun call-with-mock-server (function)
  (let ((*debug-on-error* t)
        (*server* nil))
    (start-mock-server)
    (funcall function)))

(defmacro with-mock-server (() &body body)
  `(call-with-mock-server (lambda () ,@body)))
