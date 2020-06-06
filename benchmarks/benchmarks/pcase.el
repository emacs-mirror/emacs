;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;; Apply a simple pattern match defined with pcase on the element of a list.

(require 'cl-lib)

(defvar elb-pcase-len 5000)
(defvar elb-pcase-list (cl-loop repeat elb-pcase-len
				collect (cl-case (random 3)
					  (0 '(a b))
					  (1 '(a))
					  (2 (random 10)))))

(defsubst foo (x)
  (1+ x))

(defsubst bar (x)
  (* x x))

(defun elb-pcase (l)
  (cl-loop for x in l
	   counting (pcase x
		      (`(a b) 1)
		      (`(a) 2)
		      (_ (foo (bar x))))))

(defun elb-pcase-entry ()
  (cl-loop repeat 20000
	   do (elb-pcase elb-pcase-list)))

(provide 'elb-pcase)
