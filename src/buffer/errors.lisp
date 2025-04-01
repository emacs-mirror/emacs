(defpackage :lem/buffer/errors
  (:use :cl)
  (:export :editor-condition
           :directory-does-not-exist
           :directory-does-not-exist-directory
           :read-only-error
           :editor-error
           :scan-error
           :editor-interrupt))
(in-package :lem/buffer/errors)

(define-condition editor-condition (simple-error)
  ())

(define-condition directory-does-not-exist (editor-condition)
  ((directory :initarg :directory
              :reader directory-does-not-exist-directory))
  (:report (lambda (condition stream)
             (format stream "directory does not exist: ~A"
                     (directory-does-not-exist-directory condition)))))

(define-condition read-only-error (editor-condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Read Only" stream))))

(define-condition editor-error (editor-condition)
  ((message
    :initform nil
    :initarg :message))
  (:report
   (lambda (condition stream)
     (with-slots (message) condition
       (when message
         (princ message stream))))))

(defun editor-error (message &rest args)
  (error 'editor-error :message (apply #'format nil message args)))

(defun scan-error ()
  (editor-error "Scan Error"))

(define-condition editor-interrupt (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (princ "Interrupt" stream))))
