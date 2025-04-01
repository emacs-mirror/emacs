(defpackage :lem-elisp-mode/rpc
  (:use :cl :lem)
  (:export
   :*elisp-rpc-client*
   :*elisp-rpc-url*
   :*elisp-rpc-auth*
   :connected-p
   :connect-to-server
   :client-disconnect
   :get-completions
   :get-symbol-location
   :get-symbol-documentation))

(in-package :lem-elisp-mode/rpc)

(defvar *elisp-rpc-url* "http://localhost:55486")

(defvar *elisp-rpc-auth* '("lem" . "lem"))

(defvar *elisp-rpc-client*
  (jsonrpc:make-client))

(defun connected-p ()
  (handler-case (null (jsonrpc/base:ensure-connected  *elisp-rpc-client*))
    (error (c)
      (declare (ignore c))
      nil)))

(defun connect-to-server (&key
                          (client *elisp-rpc-client*)
                          (server-url *elisp-rpc-url*))
  (jsonrpc:client-connect client
                          :mode :http
                          :url server-url))

(defun client-disconnect (&key
                          (client *elisp-rpc-client*))
  (jsonrpc:client-disconnect client))

;;TODO Add auth as a parameter
(defun get-completions (prefix
                        &key
                        (client *elisp-rpc-client*))
  "Returns a list of all the Emacs Lisp symbols defined."
  (mapcar (lambda (i) (format nil "~a" i))
          (jsonrpc:call client "lemmington-get-completion" (list prefix)
                        :basic-auth '("lem" . "lem"))))


(defun get-symbol-location (symbol
                            &key
                            (client *elisp-rpc-client*))
  "Returns the symbol location and absolute file position (location position)."
  (jsonrpc:call client "lemmington-symbol-location" (list symbol)
                :basic-auth '("lem" . "lem")))

(defun get-symbol-documentation (symbol
                                 &key
                                 (client *elisp-rpc-client*))
  "Returns the symbol documentation."
  (jsonrpc:call client "lemmington-symbol-documentation" (list symbol)
                :basic-auth '("lem" . "lem")))

(defun get-exported-functions (&key
                               (client *elisp-rpc-client*))
  "Returns a list of all the exported functions."
  (jsonrpc:call client "lemmington-exported-functions" nil
                :basic-auth '("lem" . "lem")))
