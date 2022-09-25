;;; -*- lexical-binding: t -*-
(defsubst warn-callargs-defsubst-f1 (_x)
  nil)
(defun warn-callargs-defsubst-f2 ()
  (warn-callargs-defsubst-f1 1 2))
