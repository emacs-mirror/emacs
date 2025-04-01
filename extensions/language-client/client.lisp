(defpackage :lem-language-client/client
  (:use :cl)
  (:export :jsonrpc-connect
           :client
           :client-connection)
  #+sbcl
  (:lock t))
(in-package :lem-language-client/client)

(defgeneric jsonrpc-connect (client))

(defclass client ()
  ((connection
    :initform (jsonrpc:make-client)
    :reader client-connection)))
