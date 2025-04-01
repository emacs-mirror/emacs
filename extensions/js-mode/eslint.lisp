(defpackage :lem-js-mode.eslint
  (:use :cl :lem)
  (:export :eslint))
(in-package :lem-js-mode.eslint)

(defun run-eslint (buffer)
  (with-output-to-string (output)
    (uiop:run-program (format nil "eslint '~A'" (buffer-filename buffer))
                      :ignore-error-status t
                      :directory (buffer-directory buffer)
                      :output output
                      :error-output output)))

(defun parse-eslint-output (text)
  (with-input-from-string (in text)
    (loop :for line := (read-line in nil)
          :while line
          :for result := (ppcre:register-groups-bind (line-number column description)
                             ("^\\s*(\\d+):(\\d+)\\s*(.*)" line)
                           (setq line-number (parse-integer line-number)
                                 column (parse-integer column))
                           (list line-number column description))
          :when result
          :collect it)))

(define-command eslint () ()
  (let* ((buffer (current-buffer))
         (notes (parse-eslint-output (run-eslint buffer))))
    (lem/peek-source:with-collecting-sources (collector)
      (dolist (note notes)
        (destructuring-bind (line-number column description) note
          (lem/peek-source:with-appending-source
              (point :move-function (lambda ()
                                      (let ((point (buffer-point buffer)))
                                        (move-to-line point line-number)
                                        (move-to-column point (1- column)))))
            (insert-string point (princ-to-string line-number)
                           :attribute 'lem/peek-source:position-attribute)
            (insert-character point #\:)
            (insert-string point (princ-to-string column)
                           :attribute 'lem/peek-source:position-attribute)
            (insert-character point #\:)
            (insert-string point description)))))))
