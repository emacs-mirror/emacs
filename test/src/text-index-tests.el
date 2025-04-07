;;; text-index-tests.el --- tests for src/text-index.c  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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
(require 'cl-lib)

;; Note that the tests can be quite expensive on large files because of
;; the calls to 'text-index--charpos-to-bytepos-brute' which always
;; scans the text from the beginning.

(defvar text-index-small-test-files
  (list (expand-file-name "HELLO" data-directory)
        (expand-file-name "src/buffer.c" source-directory)))

(defvar text-index-big-test-files
  (list (expand-file-name "src/buffer.c" source-directory)))

(defvar text-index-all-test-files
  (append text-index-small-test-files
          text-index-big-test-files))

(cl-defun test-check-charpos (charpos)
  (let* ((real-bytepos (text-index--charpos-to-bytepos-brute charpos))
         (index-bytepos (text-index--charpos-to-bytepos charpos))
         (index-charpos (and index-bytepos
                             (text-index--bytepos-to-charpos real-bytepos))))
    (cond ((not (eq real-bytepos index-bytepos))
           (message "Different bytepos at charpos %d (real %S index %S)"
                    charpos real-bytepos index-bytepos)
           nil)
          ((and index-charpos
                (not (eq index-charpos charpos)))
           (message "Different charpos at bytepos %S (charpos %S index %S)"
                    real-bytepos charpos index-charpos)
           nil)
          (t t))))

(cl-defmacro text-index-with-buffer ((file) &rest body)
  (declare (indent 1))
  `(ert-with-test-buffer ()
     (insert-file-contents ,file)
     (progn ,@body)))

(ert-deftest text-index-test-forward ()
  (cl-loop for file in text-index-small-test-files do
           (text-index-with-buffer (file)
             (cl-loop for charpos from (point-min) below (point-max) do
                      (should (test-check-charpos charpos))))))

(ert-deftest text-index-test-backward ()
  (cl-loop for file in text-index-small-test-files do
           (text-index-with-buffer (file)
             (cl-loop for charpos from (point-max) downto (point-min) do
                      (should (test-check-charpos charpos))))))

(ert-deftest text-index-test-random-charpos ()
  (cl-loop for file in text-index-all-test-files do
           (text-index-with-buffer (file)
             (cl-loop repeat 10000 do
                      (should (test-check-charpos (random (point-max))))))))

(defvar text-index-test-strings
  ["1" "💡" "💡3" "💡💡" "💡💡5"])

(ert-deftest text-index-test-random-insert ()
  (cl-loop for file in text-index-all-test-files do
           (text-index-with-buffer (file)
             (cl-loop repeat 10000
	              for charpos = (1+ (random (buffer-size)))
	              for string = (aref text-index-test-strings
                                         (random (length text-index-test-strings)))
	              do (progn (goto-char charpos)
		                (insert string)
		                (test-check-charpos (random (point-max))))))))
