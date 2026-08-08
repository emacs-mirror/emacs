;;; outline-tests.el --- ERT tests for outline.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;; Tests for the `outline' feature.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'outline)

(ert-deftest outline-tests--outline-xref-search-function ()
  "Test the `outline-xref' function with `outline-search-function'."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((outline-regexp "\\*")
            (outline-search-function
             (lambda (&optional bound move _backward _looking-at)
               (re-search-forward "^-" bound move))))
        (outline-xref)))
    (with-current-buffer (get-buffer "*xref*")
      (goto-char (point-min))
      (should-error (search-forward "* Star heading"))
      (goto-char (point-min))
      (should (search-forward "- Dash heading")))))

(ert-deftest outline-tests--outline-xref-regexp ()
  "Test the `outline-xref' function with `outline-regexp'."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((outline-regexp "\\*")
            (outline-search-function nil))
        (outline-xref)))
    (with-current-buffer (get-buffer "*xref*")
      (goto-char (point-min))
      (should (search-forward "* Star heading"))
      (goto-char (point-min))
      (should-error (search-forward "- Dash heading")))))

(ert-deftest outline-tests--outline-xref-undefined ()
  "Test the `outline-xref' function with undefined search strategy."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((outline-regexp nil)
            (outline-search-function nil))
        (should-error (outline-xref))))))

(provide 'outline-tests)

;;; outline-tests.el ends here
