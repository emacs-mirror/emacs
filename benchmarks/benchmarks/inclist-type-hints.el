;;; bench/inclist-type-hints.el --- Exercise type hints -*- lexical-binding: t; -*-

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

;; Iteratively increment the elements of a list. Same as inclist.el
;; but make use of compiler type hints.

;;; Code:

(require 'cl-lib)

(defvar elb-inclist-th-no-type-hints-len 50000)
(defvar elb-inclist-th-no-type-hints-list
  (mapcar #'random (make-list elb-inclist-th-no-type-hints-len 100)))

(defun elb-inclist-th (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (prog1 l
    (while l
      (let ((c (cl-the cons l)))
	(cl-incf (cl-the fixnum (car c)))
	(setq l (cdr c))))))

(defun elb-inclist-type-hints-entry ()
  (let ((l (copy-sequence elb-inclist-th-no-type-hints-list)))
    (cl-loop repeat 10000
	     do (elb-inclist-th l))))
