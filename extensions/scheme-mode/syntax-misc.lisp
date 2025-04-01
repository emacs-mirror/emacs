(defpackage :lem-scheme-syntax.misc
  (:use :cl :lem)
  (:export
   :beginning-of-defun
   :top-of-defun))
(in-package :lem-scheme-syntax.misc)

(defun beginning-of-defun (point n)
  (with-point ((start point))
    (if (minusp n)
        (dotimes (_ (- n) point)
          (if (start-line-p point)
              (line-offset point -1)
              (line-start point))
          (loop
            (when (char= #\( (character-at point 0))
              (return))
            (unless (line-offset point -1)
              (move-point point start)
              (return-from beginning-of-defun nil))))
        (dotimes (_ n point)
          (loop
            (unless (line-offset point 1)
              (move-point point start)
              (return-from beginning-of-defun nil))
            (when (char= #\( (character-at point 0))
              (return)))))))

(defun top-of-defun (point)
  (beginning-of-defun (line-end point) -1))
