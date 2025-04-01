(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:import-from :lem-lsp-mode/lem-stdio-transport
                :lem-stdio-transport)
  (:export :dispose
           :tcp-client
           :stdio-client)
  #+sbcl
  (:lock t))
(in-package :lem-lsp-mode/client)

(defgeneric dispose (client))

(defclass tcp-client (lem-language-client/client:client)
  ((port
    :initarg :port
    :reader tcp-client-port)
   (process
    :initform nil
    :initarg :process
    :reader tcp-client-process)))

(defmethod lem-language-client/client:jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (lem-language-client/client:client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))

(defmethod dispose ((client tcp-client))
  (when (tcp-client-process client)
    (lem-process:delete-process (tcp-client-process client))))

(defclass stdio-client (lem-language-client/client:client)
  ((process :initarg :process
            :reader stdio-client-process)))

(defmethod lem-language-client/client:jsonrpc-connect ((client stdio-client))
  (jsonrpc/client:client-connect-using-class (lem-language-client/client:client-connection client)
                                             'lem-stdio-transport
                                             :process (stdio-client-process client)))

(defmethod dispose ((client stdio-client))
  (async-process:delete-process (stdio-client-process client)))
