;;; -*- lexical-binding: t -*-

(defun foo-obsolete ()
  (declare (obsolete nil "99.99"))
  nil)

(defun foo ()
  (foo-obsolete))
