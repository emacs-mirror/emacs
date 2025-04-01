(defpackage :lem-copilot/utils
  (:use :cl)
  (:export :hash
           :pretty-json))
(in-package :lem-copilot/utils)

(defun hash (&rest args)
  (alexandria:plist-hash-table args :test 'equal))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))
