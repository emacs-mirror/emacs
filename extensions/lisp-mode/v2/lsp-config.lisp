(defpackage :lem-lisp-mode/v2/lsp-config
  (:use :cl
        :lem
        :lem-lsp-mode
        :lem-lsp-base/converter))
(in-package :lem-lisp-mode/v2/lsp-config)

(defvar *self-connection* t)

(define-language-spec (lisp-spec lem-lisp-mode:lisp-mode)
  :language-id "lisp"
  :root-uri-patterns '(".asd")
  :command (lambda (port)
             (assert (not *self-connection*))
             `("lem-language-server"
               "--tcp"
               "--port" ,(princ-to-string port)
               "--log-file" ,(namestring
                              (merge-pathnames "language-server.log"
                                               (lem:lem-logdir-pathname)))))
  :connection-mode :tcp)

(defun connection-workspace (connection)
  (lem-lisp-mode:connection-value connection 'workspace))

(defun (setf connection-workspace) (workspace connection)
  (setf (lem-lisp-mode:connection-value connection 'workspace) workspace))

(defun self-connection-p (workspace)
  (lem-lsp-mode::workspace-value workspace 'self-connection *self-connection*))

(defun (setf self-connection-p) (value workspace)
  (setf (lem-lsp-mode::workspace-value workspace 'self-connection) value))

(defmethod lem-lsp-mode::initialized-workspace ((mode lem-lisp-mode:lisp-mode) workspace)
  (if (self-connection-p workspace)
      (let ((connection (lem-lisp-mode:self-connection)))
        (setf (connection-workspace connection) workspace))
      (let* ((swank-port (gethash "swankPort" (lem-lsp-mode::workspace-server-info workspace)))
             (connection (lem-lisp-mode:connect-to-micros "127.0.0.1" swank-port)))
        (setf (connection-workspace connection) workspace))))

(defun start-micros-server (port)
  (setf (lem-language-server::config :backend-port) port)
  (let* ((output (make-broadcast-stream))
         (*standard-output* output)
         (*error-output* output))
    (micros:create-server :port port)))

(defun start-language-server (port)
  (bt2:make-thread (lambda ()
                    (lem-language-server:start-tcp-server port))))

(defmethod lem-lsp-mode::run-server ((spec lisp-spec))
  (if (not *self-connection*)
      (call-next-method)
      (let* ((lsp-port (lem/common/socket:random-available-port))
             (micros-port (lem/common/socket:random-available-port lsp-port)))
        (start-language-server lsp-port)
        (start-micros-server micros-port)
        (make-instance 'lem-lsp-mode/client:tcp-client :port lsp-port))))

(defmethod lem-lisp-mode:switch-connection :after ((connection lem-lisp-mode:connection))
  (let ((workspace (connection-workspace connection)))
    (lem-lsp-mode::change-workspace workspace)))

;; override lisp-mode autodoc
(defmethod lem:execute :after ((mode lem-lisp-mode:lisp-mode) (command lem:self-insert) argument)
  )

(defun reinitialize-all-lisp-buffers ()
  (dolist (buffer (buffer-list))
    (when (mode-active-p buffer 'lem-lisp-mode:lisp-mode)
      (lem-lsp-mode::reopen-buffer buffer))))

(define-command lisp/new-workspace () ()
  (let ((*self-connection* nil))
    (let* ((spec (lem-lsp-mode/spec:get-language-spec 'lem-lisp-mode:lisp-mode))
           (client (lem-lsp-mode::run-server spec))
           (workspace (lem-lsp-mode::make-workspace :spec spec
                                                    :client client
                                                    :buffer (current-buffer))))
      (setf (self-connection-p workspace) nil)
      (lem-lsp-mode::connect-and-initialize workspace
                                            (current-buffer)
                                            (lambda ()
                                              (reinitialize-all-lisp-buffers))))))
