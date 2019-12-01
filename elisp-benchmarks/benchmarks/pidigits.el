;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapted to elisp from CL version from:
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;; Compute pi

(require 'cl-lib)

(defvar elb-acc)
(defvar elb-den)
(defvar elb-num)

(defun elb-extract-digit (nth)
  (truncate (+ (* elb-num nth) elb-acc) elb-den))

(defun elb-eliminate-digit (d)
  (cl-decf elb-acc (* elb-den d))
  (setf elb-acc (* elb-acc 10)
	elb-num (* elb-num 10)))

(defun elb-next-term (k)
  (let ((k2 (1+ (* k 2))))
    (cl-incf elb-acc (* elb-num 2))
    (setf elb-acc (* elb-acc k2)
	  elb-den (* elb-den k2)
	  elb-num (* elb-num k))))

(defun elb-pidigits (x)
  (let ((elb-acc 0)
	(elb-den 1)
	(elb-num 1)
	(res ()))
    (cl-do ((d 0) (k 0) (i 0) (n 10000))
	((>= i n))
      (setf n x)
      (elb-next-term (cl-incf k))
      (unless (> elb-num elb-acc)
	(setf d (elb-extract-digit 3))
	(unless (/= d (elb-extract-digit 4))
	  (push d res)
	  (cl-incf i)
 	  ;; (when (= (mod (cl-incf i) 10) 0)
	  ;;   (message "%d" i))
	  (elb-eliminate-digit d))))
    (reverse res)))

(defun elb-pidigits-entry ()
  (cl-loop repeat 1000
	   do (elb-pidigits 500)))

(provide 'elb-pidigits)
