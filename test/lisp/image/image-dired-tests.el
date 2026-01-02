;;; image-dired-tests.el --- Tests for image-dired.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
(require 'image-dired)

(defun image-dired-test-image-file (name)
  (expand-file-name
   name (expand-file-name "data/image"
                          (or (getenv "EMACS_TEST_DIRECTORY")
                              "../"))))

(ert-deftest image-dired-tests-get-exif-file-name ()
  (skip-unless (image-type-available-p 'jpeg))
  (let ((img (image-dired-test-image-file "black.jpg")))
    (should (equal (image-dired-get-exif-file-name img)
                   "2019_09_21_16_22_13_black.jpg"))))

;;; image-dired-tests.el ends here
