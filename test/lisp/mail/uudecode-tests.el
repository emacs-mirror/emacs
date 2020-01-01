;;; uudecode-tests.el --- Tests for uudecode.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'uudecode)

(defvar uudecode-tests-data-dir
  (file-truename
   (expand-file-name "uudecode-resources/"
                     (file-name-directory (or load-file-name
                                              buffer-file-name))))
  "Base directory of uudecode-tests.el test data files.")

(defun uudecode-tests-read-file (file)
  "Read contents of FILE and return as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar uudecode-tests-encoded-str
  (uudecode-tests-read-file
   (expand-file-name "uuencoded.txt" uudecode-tests-data-dir))
  "Uuencoded data for bookmark-tests.el
Same as `bookmark-tests-decoded-str' but uuencoded.")
(defvar uudecode-tests-decoded-str
  (uudecode-tests-read-file
   (expand-file-name "uudecoded.txt" uudecode-tests-data-dir))
  "Plain text data for bookmark-tests.el
Same as `bookmark-tests-encoded-str' but plain text.")

(ert-deftest uudecode-tests-decode-region-internal ()
  ;; Write to buffer
  (with-temp-buffer
    (insert uudecode-tests-encoded-str)
    (uudecode-decode-region-internal (point-min) (point-max))
    (should (equal (buffer-string) uudecode-tests-decoded-str)))
  ;; Write to file
  (with-temp-buffer
    (let ((tmpfile (make-temp-file "uudecode-tests-")))
      (unwind-protect
          (progn
            (insert uudecode-tests-encoded-str)
            (uudecode-decode-region-internal (point-min) (point-max) tmpfile)
            (should (equal (uudecode-tests-read-file tmpfile)
                           uudecode-tests-decoded-str)))
        (delete-file tmpfile)))))

(ert-deftest uudecode-tests-decode-region-external ()
  ;; Write to buffer
  (when uudecode-use-external
    (with-temp-buffer
      (insert uudecode-tests-encoded-str)
      (uudecode-decode-region-external (point-min) (point-max))
      (should (equal (buffer-string) uudecode-tests-decoded-str)))
    ;; Write to file
    (with-temp-buffer
      (let ((tmpfile (make-temp-file "uudecode-tests-")))
        (unwind-protect
            (progn
              (insert uudecode-tests-encoded-str)
              (uudecode-decode-region-external (point-min) (point-max) tmpfile)
              (should (equal (uudecode-tests-read-file tmpfile)
                             uudecode-tests-decoded-str)))
          (delete-file tmpfile))))))

(provide 'uudecode-tests)
;;; uudecode-tests.el ends here
