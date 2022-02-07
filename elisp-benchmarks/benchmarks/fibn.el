;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2022 Free Software Foundation, Inc.

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

;;; Code:

;; Adapted to elisp from CL version from:
;; https://drmeister.wordpress.com/2015/07/30/timing-data-comparing-cclasp-to-c-sbcl-and-python/

(defun elb-fibn (reps num)
  (let ((z 0))
    (dotimes (_ reps)
      (let ((p1 1)
            (p2 1))
        (dotimes (_ (- num 2))
          (setf z (+ p1 p2)
                p2 p1
                p1 z))))
    z))

(defun elb-fibn-entry ()
  ;; Use 80 to stay in the fixnum range (on 64bit systems).
  (elb-fibn 1000000 80))

;; Fibonacci sequence tail recursive algo.

(defun elb-fibn-tc (a b count)
  (if (= count 0)
      b
    (elb-fibn-tc (+ a b) a (- count 1))))

(defun elb-fibn-tc-entry ()
  (dotimes (_ 1000000)
    (elb-fibn-tc 1 0 80)))

;; Fibonacci sequence with named-let.

(defun elb-fibn-named-let (count)
  (named-let loop ((a 1)
                   (b 0)
                   (count count))
    (if (= count 0)
        b
      (loop (+ a b) a (- count 1)))))

(defun elb-fibn-named-let-entry ()
  (dotimes (_ 1000000)
    (elb-fibn-named-let 80)))

;; Fibonacci sequence with the naive recursive algo.

(defun elb-fibn-rec (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (elb-fibn-rec (- n 1))
	      (elb-fibn-rec (- n 2))))))

(defun elb-fibn-rec-entry ()
  (elb-fibn-rec 37))


(provide 'elb-fibn)
;;; elb-fibn ends here.
