(in-package :lem-core)

(defparameter *default-editor-abort-message* "Abort")

(define-condition editor-abort (editor-condition)
  ((message
    :initarg :message
    :initform *default-editor-abort-message*
    :reader editor-abort-message))
  (:report
   (lambda (condition stream)
     (when (editor-abort-message condition)
       (princ (editor-abort-message condition) stream)))))

(define-condition exit-editor ()
  ((report :initarg :report
           :reader exit-editor-report)))

(define-condition undefined-key-error (editor-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s
                     "Key not found: ~A"
                     (keyseq-to-string (last-read-key-sequence))))))

(define-condition move-cursor-error (editor-error)
  ((point :initarg :point
          :reader move-cursor-error-point)))

(define-condition end-of-buffer (move-cursor-error)
  ()
  (:default-initargs :message nil))

(define-condition beginning-of-buffer (move-cursor-error)
  ()
  (:default-initargs :message nil))
