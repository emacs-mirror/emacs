(defpackage :lem-tests/language-server/utils
  (:use :cl
        :lem-language-server)
  (:export :lines))
(in-package :lem-tests/language-server/utils)

(defun lines (&rest strings)
  (format nil "~{~A~%~}" strings))
