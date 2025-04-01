(defpackage :lem-sdl2/icon-font
  (:use :cl)
  (:export :clear-icon-font-cache
           :icon-font))
(in-package :lem-sdl2/icon-font)

(defvar *icon-font-cache* (make-hash-table :test 'eql))

(defun clear-icon-font-cache ()
  (clrhash *icon-font-cache*))

(defun icon-font (character font-size)
  ;; TODO: Fix problem with opening several of the same font file
  (or (gethash character *icon-font-cache*)
      (alexandria:when-let (font-name (lem:icon-value (char-code character) :font))
        (let ((pathname (lem-sdl2/resource:get-resource-pathname
                         (merge-pathnames font-name "resources/fonts/"))))
          (setf (gethash character *icon-font-cache*)
                (sdl2-ttf:open-font pathname font-size))))))
