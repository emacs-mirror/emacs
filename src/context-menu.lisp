(in-package :lem-core)

(defun buffer-context-menu (buffer)
  (buffer-value buffer 'context-menu))

(defun (setf buffer-context-menu) (context-menu buffer)
  (setf (buffer-value buffer 'context-menu) context-menu))
