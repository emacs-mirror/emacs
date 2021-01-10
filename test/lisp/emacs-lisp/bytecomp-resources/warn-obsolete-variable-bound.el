;;; -*- lexical-binding: t -*-

(make-obsolete-variable 'bytecomp--tests-obsolete-var-2 nil "99.99")

(defun foo ()
  (let ((bytecomp--tests-obsolete-var-2 2))
    bytecomp--tests-obsolete-var-2))
