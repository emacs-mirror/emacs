(defpackage :lem-lisp-mode/connections
  (:use :cl)
  (:export :connection-list
           :add-connection
           :remove-connection))
(in-package :lem-lisp-mode/connections)

(defvar *connection-list* '())

(defun connection-list ()
  (copy-list *connection-list*))

(defun add-connection (connection)
  (push connection *connection-list*))

(defun remove-connection (connection)
  (setf *connection-list* (delete connection *connection-list*))
  (values))
