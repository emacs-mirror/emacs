;;; -*- lexical-binding: t -*-
(defun foo ()
  (add-hook 'bytecomp--tests-obsolete-var #'next-line))
