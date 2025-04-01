(defpackage :lem-language-server/cli
  (:use :cl)
  (:export :main))
(in-package :lem-language-server/cli)

(defparameter *command-line-spec*
  '((("stdio")
     :type boolean
     :optional t
     :documentation "Start the server with stdio (default)")
    (("tcp")
     :type boolean
     :optional t
     :documentation "Start the server with tcp")
    (("port")
     :type integer
     :optional t
     :documentation "Specifies the port number for tcp (10007 by default)")
    (("log-file")
     :type string
     :optional t
     :documentation "Specify the file to output the log")
    (("help" #\h)
     :type boolean
     :optional t
     :documentation "display help")))

(defun main (&optional (args (uiop:command-line-arguments)))
  (command-line-arguments:handle-command-line
   *command-line-spec*
   'run-server
   :command-line args))

(defun command-line-error (control-string &rest format-arguments)
  (apply #'format t control-string format-arguments)
  (command-line-arguments:show-option-help *command-line-spec*))

(defun run-server (&key tcp stdio (port 10007) log-file help)
  (when help
    (command-line-arguments:show-option-help *command-line-spec*)
    (return-from run-server))
  (when (and tcp stdio)
    (command-line-error "These arguments cannot be used together: (tcp, stdio)")
    (return-from run-server))
  (when log-file
    (log:config :sane :daily log-file :debug))
  (cond (tcp
         (lem-language-server:start-tcp-server port))
        (t
         (unless log-file
           ;; Disable logging to avoid conflicts between log output and standard output
           (log:config :off))
         (lem-language-server:start-stdio-server))))
