;;; align-tests.el --- Test suite for aligns  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'align)

(defun test-align-compare (file function)
  (should (equal
           (with-temp-buffer
             (insert-file-contents (ert-resource-file (format file "pre")))
             (funcall function)
             (align (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max)))
           (with-temp-buffer
             (insert-file-contents (ert-resource-file (format file "post")))
             (buffer-string)))))

(ert-deftest align-java ()
  (test-align-compare "align-%s.java" #'java-mode))

(ert-deftest align-c ()
  (test-align-compare "align-%s.c" #'c-mode))

(provide 'align-tests)

;;; align-tests.el ends here
