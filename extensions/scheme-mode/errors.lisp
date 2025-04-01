(defpackage :lem-scheme-mode.errors
  (:use :cl)
  (:export :disconnected
           :change-connection))
(in-package :lem-scheme-mode.errors)

(define-condition disconnected (simple-condition)
  ())

(define-condition change-connection (simple-condition)
  ())
