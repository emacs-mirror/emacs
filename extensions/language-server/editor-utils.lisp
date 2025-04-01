(in-package :lem-language-server)

(defun backward-up-list (point)
  (lem:backward-up-list point t))

(defun forward-up-list (point)
  (lem:forward-up-list point t))

(defun forward-down-list (point)
  (lem:forward-down-list point t))
