(defpackage :lem-lsp-mode/spec
  (:use :cl)
  (:export :spec-language-id
           :spec-root-uri-patterns
           :spec-command
           :spec-install-command
           :spec-readme-url
           :spec-connection-mode
           :spec-port
           :get-language-spec
           :register-language-spec
           :get-spec-command
           :spec-mode))
(in-package :lem-lsp-mode/spec)

(defclass spec ()
  ((language-id
    :initarg :language-id
    :initform (alexandria:required-argument :language-id)
    :reader spec-language-id)
   (root-uri-patterns
    :initarg :root-uri-patterns
    :initform nil
    :reader spec-root-uri-patterns)
   (command
    :initarg :command
    :initform nil
    :reader spec-command)
   (install-command
    :initarg :install-command
    :initform nil
    :reader spec-install-command)
   (readme-url
    :initarg :readme-url
    :initform nil
    :reader spec-readme-url)
   (connection-mode
    :initarg :connection-mode
    :initform (alexandria:required-argument :connection-mode)
    :reader spec-connection-mode)
   (port
    :initarg :port
    :initform nil
    :reader spec-port)
   (mode
    :initarg :mode
    :reader spec-mode)))

(defun get-language-spec (major-mode)
  (let ((spec (get major-mode 'spec)))
    (assert (typep spec 'spec))
    spec))

(defun register-language-spec (major-mode spec)
  (check-type spec spec)
  (setf (get major-mode 'spec) spec))

(defmethod (setf spec-command) ((command list)
				(spec spec))
  (setf (slot-value spec 'command) command))

(defun get-spec-command (spec &rest args)
  (let ((command (spec-command spec)))
    (if (functionp command)
        (apply command args)
        command)))
