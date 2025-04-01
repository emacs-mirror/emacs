(defpackage :lem-nix-mode/indent
  (:use :cl
        :lem
        :lem/language-mode)
  (:export :beginning-of-defun
           :end-of-defun
           :calc-indent))
(in-package :lem-nix-mode/indent)

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\{")))

(defun end-of-defun (point n)
  (if (minusp n)
      (beginning-of-defun point (- n))
      (search-forward-regexp point "^\\}")))

(defun calc-indent (point)
  (with-point ((point point))
    (let ((tab-width 2)
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))
