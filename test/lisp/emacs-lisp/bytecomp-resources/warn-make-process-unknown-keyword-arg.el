;;; -*- lexical-binding: t -*-
(defun foo ()
  (make-process :name "ls" :command "ls"
                :coding-system 'binary))
