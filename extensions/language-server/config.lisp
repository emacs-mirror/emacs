(in-package :lem-language-server)

(defparameter *config*
  '(:name "cl-lsp"
    :version "0.1.0"
    :backend-port nil
    :backend-hostname "localhost"))

(defun config (name)
  (getf *config* name))

(defun (setf config) (value name)
  (setf (getf *config* name) value))

(defun language-server-name ()
  (config :name))

(defun language-server-version ()
  (config :version))
