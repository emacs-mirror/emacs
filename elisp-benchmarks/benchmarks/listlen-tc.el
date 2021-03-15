;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

;; Compute the length of a list tail recursively.

(require 'cl-lib)

(defvar elb-listlen-tc-len 10000)
(defvar elb-listlen-tc-list
  (mapcar #'random (make-list elb-listlen-tc-len 100)))

(defun elb-listlen-tc (l n)
  (if (null l)
      n
    (cl-incf n)
    (elb-listlen-tc (cdr l) n)))

(defun elb-listlen-tc-entry ()
  (let* ((n (+ 1000 elb-listlen-tc-len))
	 (max-lisp-eval-depth n)
	 (max-specpdl-size n))
    (cl-loop repeat 10000
	     do (elb-listlen-tc elb-listlen-tc-list 0))))

(provide 'listlen-tc)

;; Local Variables:
;; comp-speed: 3
;; End:
