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

;; From:
;; https://www.emacswiki.org/emacs/EmacsLispBenchmark

(require 'cl-lib)

(defvar elb-bubble-len 1000)
(defvar elb-bubble-list (mapcar #'random (make-list elb-bubble-len
						    most-positive-fixnum)))

(defun elb-bubble (list)
  (let ((i (length list)))
    (while (> i 1)
      (let ((b list))
        (while (cdr b)
          (when (< (cadr b) (car b))
            (setcar b (prog1 (cadr b)
                        (setcdr b (cons (car b) (cddr b))))))
          (setq b (cdr b))))
      (setq i (1- i)))
    list))

(defun elb-bubble-entry ()
  (cl-loop repeat 100
	   for l = (copy-sequence elb-bubble-list)
	   do (elb-bubble l)))

(provide 'elb-bubble)

;; Local Variables:
;; comp-speed: 3
;; End:
