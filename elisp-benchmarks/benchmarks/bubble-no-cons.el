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

;; Like bubble but in place.

(require 'cl-lib)

(defvar elb-bubble-len 1000)
(defvar elb-bubble-list (mapcar #'random (make-list elb-bubble-len
						    most-positive-fixnum)))
(defun elb-bubble-no-cons (list)
  (cl-loop repeat (length list)
	   do
	   (cl-loop for x on list
		    for a = (car x)
		    for b = (cadr x)
		    when (and b (> a b))
		      do (setcar x b)
		         (setcar (cdr x) a)
		    finally return list)))

(defun elb-bubble-no-cons-entry ()
  (cl-loop repeat 200
	   for l = (copy-sequence elb-bubble-list)
	   do (elb-bubble-no-cons l)))

(provide 'elb-bubble)
