;;; -*- lexical-binding: t -*-
(defun foo ()
  (make-process :name "ls" :command "ls" :name "ls"))
