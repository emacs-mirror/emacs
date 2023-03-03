;;; -*- lexical-binding: t -*-
(defun foo ()
  (autoload 'bar "baz" nil nil 'macro))
