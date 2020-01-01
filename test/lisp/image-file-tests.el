;;; image-file-tests.el --- Test suite for image-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(require 'ert)
(require 'image-file)

(defconst image-file-tests-data-directory
  (expand-file-name "data/image" (getenv "EMACS_TEST_DIRECTORY"))
  "Directory containing image test data.")

(ert-deftest insert-image-file ()
  (skip-unless (image-type-available-p 'png))
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert-image-file (expand-file-name "blank-100x200.png"
                                         image-file-tests-data-directory))
    (should (image--get-image)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-image-file (expand-file-name "blank-100x200.png"
                                         image-file-tests-data-directory))
    (should (image--get-image))))

;;; image-file-tests.el ends here
