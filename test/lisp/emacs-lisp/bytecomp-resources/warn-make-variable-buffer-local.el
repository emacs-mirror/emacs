;;; -*- lexical-binding: t -*-
(defvar foobar)
(defun foo ()
  (make-variable-buffer-local 'foobar))
