(defpackage :lem-sdl2/mouse
  (:use :cl)
  (:export :cursor-shown-p
           :show-cursor
           :hide-cursor))
(in-package :lem-sdl2/mouse)

(defvar *cursor-shown* t)

(defun cursor-shown-p ()
  *cursor-shown*)

(defun show-cursor ()
  (setf *cursor-shown* t)
  (sdl2:show-cursor))

(defun hide-cursor ()
  (setf *cursor-shown* nil)
  (sdl2:hide-cursor))
