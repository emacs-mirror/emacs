;;; image-dired-util-tests.el --- Tests for image-dired.el  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'image-dired-util)

(ert-deftest image-dired-thumb-name ()
  (let ((image-dired-thumbnail-storage 'standard))
    (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
    (should (equal (file-name-nondirectory (image-dired-thumb-name "foo.jpg"))
                   "4abfc97f9a5d3c4c519bfb23e4da8b90.png")))
  (let ((image-dired-thumbnail-storage 'image-dired))
    (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
    (should (equal (file-name-nondirectory (image-dired-thumb-name "foo.jpg"))
                   "foo_5baffb8d7984b3088db58efd7d8909c5.thumb.jpg")))
  (let ((image-dired-thumbnail-storage 'per-directory))
    (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
    (should (equal (file-name-nondirectory (image-dired-thumb-name "foo.jpg"))
                   "foo.thumb.jpg"))))

;;; image-dired-util-tests.el ends here
