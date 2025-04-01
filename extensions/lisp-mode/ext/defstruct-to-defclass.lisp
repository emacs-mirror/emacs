(defpackage :lem-lisp-mode/defstruct-to-defclass
  (:use :cl :lem))
(in-package :lem-lisp-mode/defstruct-to-defclass)

(define-command lisp-defstruct-to-defclass () ()
  (lem-lisp-syntax:defstruct-to-defclass (current-point)))
