;;; exif-tests.el --- tests for exif.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'exif)

(defun test-image-file (name)
  (expand-file-name
   name (expand-file-name "data/image"
                          (or (getenv "EMACS_TEST_DIRECTORY")
                              "../../"))))

(ert-deftest test-exif-parse ()
  (let ((exif (exif-parse-file (test-image-file "black.jpg"))))
    (should (equal (exif-field 'make exif) "Panasonic"))
    (should (equal (exif-field 'orientation exif) 1))
    (should (equal (exif-field 'x-resolution exif) '(180 . 1)))
    (should (equal (exif-field 'date-time exif) "2019:09:21 16:22:13"))))

(ert-deftest test-exif-parse-short ()
  (let ((exif (exif-parse-file (test-image-file "black-short.jpg"))))
    (should (equal (exif-field 'make exif) "thr"))
    (should (equal (exif-field 'model exif) "four"))
    (should (equal (exif-field 'software exif) "em"))
    (should (equal (exif-field 'artist exif) "z"))))

(ert-deftest test-exit-direct-ascii-value ()
  (should (equal (exif--direct-ascii-value 28005 2 t) (string ?e ?m 0)))
  (should (equal (exif--direct-ascii-value 28005 2 nil) (string ?m ?e 0))))

;;; exif-tests.el ends here
