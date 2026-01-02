;;; fw-tests.el --- Tests for semantic/fw.el  -*- lexical-binding:t -*-

;; Copyright (C) 2003-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

;; Moved here from test/manual/cedet/semantic-tests.el

;;; Code:

(require 'ert)
(require 'semantic/fw)

;;; From semantic-fw:

(ert-deftest semantic-test-data-cache ()
  "Test the data cache."
  (let ((data '(a b c)))
    (with-current-buffer (get-buffer-create " *semantic-test-data-cache*")
      (erase-buffer)
      (insert "The Moose is Loose")
      (goto-char (point-min))
      (semantic-cache-data-to-buffer (current-buffer) (point) (+ (point) 5)
                                     data 'moose 'exit-cache-zone)
      ;; retrieve cached data
      (should (equal (semantic-get-cache-data 'moose) data)))))

;;; fw-tests.el ends here
