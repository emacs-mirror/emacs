;;; finalizer-tests.el --- Finalizer tests -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords:

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest finalizer-basic ()
  "Test that finalizers run at all."
  (skip-unless gc-precise)
  (let* ((finalized nil)
         (finalizer (make-finalizer (lambda () (setf finalized t)))))
    (garbage-collect)
    (should (equal finalized nil))
    (setf finalizer nil)
    (garbage-collect)
    (should (equal finalized t))))

(ert-deftest finalizer-circular-reference ()
  "Test references from a callback to a finalizer."
  (skip-unless gc-precise)
  (let ((finalized nil))
    (let* ((value nil)
           (finalizer (make-finalizer (lambda () (setf finalized value)))))
      (setf value finalizer)
      (setf finalizer nil))
    (garbage-collect)
    (should finalized)))

(ert-deftest finalizer-cross-reference ()
  "Test that between-finalizer references do not prevent collection."
  (skip-unless gc-precise)
  (let ((d nil) (fc 0))
    (let* ((f1-data (cons nil nil))
           (f2-data (cons nil nil))
           (f1 (make-finalizer
                (lambda () (cl-incf fc) (setf d f1-data))))
           (f2 (make-finalizer
                (lambda () (cl-incf fc) (setf d f2-data)))))
      (setcar f1-data f2)
      (setcar f2-data f1))
    (garbage-collect)
    (should (equal fc 2))))

(ert-deftest finalizer-error ()
  "Test that finalizer errors are suppressed"
  (skip-unless gc-precise)
  (make-finalizer (lambda () (error "ABCDEF")))
  (garbage-collect)
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (should (equal
               (buffer-substring (point) (point-at-eol))
               "finalizer failed: (error \"ABCDEF\")")))))

(ert-deftest finalizer-object-type ()
  (should (equal (type-of (make-finalizer nil)) 'finalizer)))
