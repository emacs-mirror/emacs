;;; coding-tests.el --- tests for text encoding and decoding

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>

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

;;; Code:

(require 'ert)

;; Directory to hold test data files.
(defvar coding-tests-workdir
  (expand-file-name "coding-tests" temporary-file-directory))

;; Remove all generated test files.
(defun coding-tests-remove-files ()
  (delete-directory coding-tests-workdir t))

(ert-deftest ert-test-coding-bogus-coding-systems ()
  (unwind-protect
      (let (test-file)
        (or (file-directory-p coding-tests-workdir)
            (mkdir coding-tests-workdir t))
        (setq test-file (expand-file-name "nonexistent" coding-tests-workdir))
        (if (file-exists-p test-file)
            (delete-file test-file))
        (should-error
         (let ((coding-system-for-read 'bogus))
           (insert-file-contents test-file)))
        ;; See bug #21602.
        (setq test-file (expand-file-name "writing" coding-tests-workdir))
        (should-error
         (let ((coding-system-for-write (intern "\"us-ascii\"")))
           (write-region "some text" nil test-file))))
    (coding-tests-remove-files)))
