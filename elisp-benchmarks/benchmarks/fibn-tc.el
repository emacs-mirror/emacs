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

;; Fibonacci sequence tail recursive algo.

(require 'cl-lib)

(defun elb-fibn-tc (a b count)
  (if (= count 0)
      b
    (elb-fibn-tc (+ a b) a (- count 1))))

(defun elb-fibn-tc-entry ()
  (cl-loop repeat 1000000
	   do (elb-fibn-tc 1 0 80)))

(provide 'fibn-tc)
