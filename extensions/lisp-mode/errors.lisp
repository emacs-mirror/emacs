(defpackage :lem-lisp-mode/errors
  (:use :cl)
  (:export :disconnected
           :change-connection))
(in-package :lem-lisp-mode/errors)

(define-condition disconnected (simple-condition)
  ())

(define-condition change-connection (simple-condition)
  ())
