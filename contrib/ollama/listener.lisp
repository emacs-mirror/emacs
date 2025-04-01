(defpackage :lem-ollama/listener
  (:use :cl :lem :lem-ollama :alexandria-2))
(in-package :lem-ollama/listener)

(define-major-mode ollama-listener-mode lem-ollama::ollama-mode
    (:name "ollama-listener"
     :keymap *ollama-listener-mode-keymap*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(defun reset-listener-variables (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        #'execute-input))

(defun print-prompt (&key first?)
  (message "ready")
  (insert-string 
   (buffer-end-point (get-repl-buffer))
   (format nil (if first? "ollama> " "~%~%ollama> "))
   :read-only t
   :attribute (make-attribute :bold t))
  (redraw-display))

(defun execute-input (point string)
  (bt2:make-thread
   (lambda ()
     (with-open-stream (out (make-buffer-output-stream point))
       (handler-case
           (ollama-request-and-handle
            (str:trim (car (last (ppcre:split "ollama>" string))))
            out 
            :close-hook #'print-prompt)
         (error (c)
           (format out "Error: ~a~%" c)
           (print-prompt)))))))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*ollama*")))
    (unless (eq (buffer-major-mode buffer) 'ollama-listener-mode)
      (change-buffer-mode buffer 'ollama-listener-mode)
      (print-prompt :first? t))
    buffer))

(define-command run-ollama () ()
  (pop-to-buffer (get-repl-buffer)))