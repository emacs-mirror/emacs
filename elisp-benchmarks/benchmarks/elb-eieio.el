;;; elb-eieio.el --- Benchmarking EIEIO operations   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)

(defclass elb--eieio-cons () ((car :initarg :car) (cdr :initarg :cdr)))

(cl-defmethod shared-initialize ((_obj elb--eieio-cons) _slots)
  (cl-call-next-method))

(cl-defgeneric elb--eieio-length (_x) 0)
(cl-defmethod  elb--eieio-length ((x elb--eieio-cons))
  (1+ (elb--eieio-length (slot-value x 'cdr))))

(defun elb-eieio-entry ()
  (let ((total 0))
    (dotimes (_ 5000)
      (let ((l nil))
        ;; The elb--eieio-length recursion can't deal with more than about
        ;; 150 elements.
        (dotimes (i 100)
          (setq l (make-instance 'elb--eieio-cons :car i :cdr l)))
        (setq total (+ total (elb--eieio-length l)))))
    total))
 
(provide 'elb-eieio)
;;; elb-eieio.el ends here
