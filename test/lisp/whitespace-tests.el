;;; whitespace-tests.el --- Test suite for whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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
(require 'whitespace)

(defun whitespace-tests--cleanup-string (string)
  (with-temp-buffer
    (insert string)
    (whitespace-cleanup)
    (buffer-string)))

(ert-deftest whitespace-cleanup-eob ()
  (let ((whitespace-style '(empty)))
    (should (equal (whitespace-tests--cleanup-string "a\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t \n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t \n\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "\n\t\n")
                   ""))
    ;; Whitespace at end of non-empty line is not covered by the
    ;; `empty' style.
    (should (equal (whitespace-tests--cleanup-string "a  \n\t \n\n")
                   "a  \n"))))

(provide 'whitespace-tests)

;;; whitespace-tests.el ends here
