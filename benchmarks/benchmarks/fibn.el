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
  ;; Use 80 to stay in the fixnum range.
  (elb-fibn 3000000 80))

(provide 'elb-fibn)

;; Local Variables:
;; comp-speed: 3
;; End:
