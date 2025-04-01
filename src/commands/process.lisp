(defpackage :lem-core/commands/process
  (:use :cl :lem-core)
  (:export :filter-buffer
           :pipe-command)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/process)

(define-key *global-keymap* "C-x #" 'filter-buffer)
(define-key *global-keymap* "C-x @" 'pipe-command)

(define-command filter-buffer (cmd) ((:string "Filter buffer: "))
  "Replaces the contents of the buffer with the result of executing the command entered."
  (let ((buffer (current-buffer))
        (line-number (line-number-at-point (current-point)))
        (charpos (point-charpos (current-point))))
    (multiple-value-bind (start end)
        (cond ((buffer-mark-p buffer)
               (values (region-beginning buffer)
                       (region-end buffer)))
              (t
               (values (buffer-start-point buffer)
                       (buffer-end-point buffer))))
      (let ((string (points-to-string start end))
            output-value
            error-output-value
            status)
        (let ((output-string
                (with-output-to-string (output)
                  (with-input-from-string (input string)
                    (multiple-value-setq
                        (output-value error-output-value status)
                      (uiop:run-program cmd
                                        :directory (buffer-directory buffer)
                                        :input input
                                        :output output
                                        :error-output output
                                        :ignore-error-status t))))))
          (when (zerop status)
            (delete-between-points start end)
            (insert-string start output-string)
            (move-to-line (current-point) line-number)
            (line-offset (current-point) 0 charpos)))))))

(define-command pipe-command (str) ((:string "Pipe command: "))
  "Run a command and displays the output."
  (let ((directory (buffer-directory)))
    (let ((output-string
            (with-output-to-string (out)
              (uiop:run-program str
                                :directory directory
                                :output out
                                :error-output out
                                :ignore-error-status t))))
      (unless (string= output-string "")
        (with-pop-up-typeout-window (out (make-buffer "*Command*") :erase t :read-only nil)
          (write-string output-string out))))))
