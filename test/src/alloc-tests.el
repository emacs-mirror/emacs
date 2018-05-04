;;; alloc-tests.el --- alloc tests -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest finalizer-object-type ()
  (should (equal (type-of (make-finalizer nil)) 'finalizer)))

(ert-deftest record-1 ()
  (let ((x (record 'foo 1 2 3)))
    (should (recordp x))
    (should (eq (type-of x) 'foo))
    (should (eq (aref x 0) 'foo))
    (should (eql (aref x 3) 3))
    (should (eql (length x) 4))))

(ert-deftest record-2 ()
  (let ((x (make-record 'bar 1 0)))
    (should (eql (length x) 2))
    (should (eql (aref x 1) 0))))

(ert-deftest record-3 ()
  (let* ((x (record 'foo 1 2 3))
         (y (copy-sequence x)))
    (should-not (eq x y))
    (dotimes (i 4)
      (should (eql (aref x i) (aref y i))))))
