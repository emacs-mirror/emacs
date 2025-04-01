(defpackage :lem-copilot/test-commands
  (:use :cl :lem :lem-copilot/utils)
  (:local-nicknames (:client :lem-copilot/client)
                    (:logger :lem-copilot/logger)
                    (:copilot :lem-copilot)))
(in-package :lem-copilot/test-commands)

(define-command test/copilot-document () ()
  (let ((response (client:request (copilot::client)
                                  "testing/getDocument"
                                  (hash "uri" (copilot::buffer-uri (current-buffer))))))
    (show-message (pretty-json response))
    (assert (equal (gethash "text" response)
                   (buffer-text (current-buffer))))))

(define-command test/copilot-log () ()
  (let* ((buffer (make-buffer "*copilot-log*" :enable-undo-p nil)))
    (erase-buffer buffer)
    (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer)))
      (logger:write-log stream))
    (pop-to-buffer buffer)))
