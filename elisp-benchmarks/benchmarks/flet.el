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

;; Apply a local function cl-flet defined on each element of list.

(require 'cl-lib)

(defvar elb-flet-len 50000)
(defvar elb-flet-list
  (mapcar #'random (make-list elb-flet-len 1000)))

(defun elb-flet (l)
  (cl-flet ((f (x)
	       (or (> x 750)
		   (< x 250))))
    (cl-loop for x in l
	     count (f x))))

(defun elb-flet-entry ()
  (cl-loop repeat 2500
	   do (elb-flet elb-flet-list)))

(provide 'elb-flet)

;; Local Variables:
;; comp-speed: 3
;; End:
