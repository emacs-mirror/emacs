(uiop:define-package :lem-copilot/install
  (:use :cl :lem)
  (:local-nicknames (:client :lem-copilot/client)
                    (:logger :lem-copilot/logger)
                    (:copilot :lem-copilot))
  (:export :copilot-install-server))
(in-package :lem-copilot/install)

(define-command copilot-install-server () ()
  (let* ((buffer (make-buffer "*copilot-install-server*"))
         (command (list "npm"
                        "-g"
                        "--prefix"
                        (namestring (copilot::copilot-root))
                        "install"
                        "copilot-node-server@1.40.0")))
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-point ((point (buffer-point buffer) :left-inserting))
      (with-open-stream (output (make-editor-output-stream point))
        (format output "~{~A ~}~%" command)
        (redraw-display)
        (uiop:run-program command
                          :output output
                          :error-output output)))))
