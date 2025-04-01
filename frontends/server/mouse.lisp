(uiop:define-package :lem-server/mouse
  (:use :cl)
  (:export :update-position
           :get-position))
(in-package :lem-server/mouse)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defun update-position (x y)
  (setf *mouse-x* x
        *mouse-y* y))

(defun get-position ()
  (values *mouse-x* *mouse-y*))
