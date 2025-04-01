(defpackage :lem-lisp-mode/test-api
  (:use :cl)
  (:export :*disable-self-connect*))
(in-package :lem-lisp-mode/test-api)

(defvar *disable-self-connect* nil)
