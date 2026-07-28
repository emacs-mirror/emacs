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

(ert-deftest outline-tests--outline-occur ()
  "Test the `outline-occur' function with `outline-regexp'."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (setq-local outline-regexp "\\*"
		  outline-occur-regexp nil)
      (outline-occur))
    (with-current-buffer (get-buffer "*Occur*")
      (goto-char (point-min))
      (should (search-forward "* Star heading"))
      (goto-char (point-min))
      (should-error (search-forward "- Dash heading")))))

(ert-deftest outline-tests--outline-occur-override ()
  "Test the `outline-occur' function with `outline-occur-regexp'."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (setq-local outline-regexp "\\*"
		  outline-occur-regexp "-")
      (outline-occur))
    (with-current-buffer (get-buffer "*Occur*")
      (goto-char (point-min))
      (should-error (search-forward "* Star heading"))
      (goto-char (point-min))
      (should (search-forward "- Dash heading")))))

(ert-deftest outline-tests--outline-occur-undefined-regexp ()
  "Test the `outline-occur' function with undefined regexp."
  (let ((test-file (ert-resource-file "outline.txt")))
    (with-temp-buffer
      (insert-file-contents test-file)
      (setq-local outline-regexp nil
		  outline-occur-regexp nil)
      (should-error (outline-occur)))))

(provide 'outline-tests)

;;; outline-tests.el ends here
