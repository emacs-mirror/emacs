(defpackage :lem-sdl2/resource
  (:use :cl)
  (:export :get-resource-pathname))
(in-package :lem-sdl2/resource)

(defvar *resource-directory* nil)

(defun get-resource-pathname (pathname)
  (if *resource-directory*
      (merge-pathnames pathname *resource-directory*)
      (or (lem:lem-relative-pathname pathname)
          (asdf:system-relative-pathname :lem-sdl2 pathname))))
