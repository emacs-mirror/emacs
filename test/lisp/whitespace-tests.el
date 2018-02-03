;;; whitespace-tests.el --- Test suite for whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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


;; We cannot call whitespace-mode because it will do nothing in batch
;; mode.  So we call its innards instead.
(defun whitespace-tests-whitespace-mode-on ()
  "Turn whitespace-mode on even in batch mode."
  (whitespace-turn-on)
  (whitespace-action-when-on)
  (setq whitespace-mode t))

(ert-deftest whitespace-tests-display-tables ()
  "Test whitespace stores and restores the buffer display table - bug26892."
  (with-temp-buffer
    (whitespace-mode -1) ; turn off in case global ws mode is active
    (let ((whitespace-style '(space-mark tab-mark newline-mark))
          (whitespace-display-mappings '((space-mark   32 [183] [46])
                                         (space-mark  160 [164] [95])
                                         (newline-mark 10 [36 10])
                                         (tab-mark      9 [187 9] [92 9])))
          (buffer-display-table nil))
      ;test the display table actually changes
      (should-not (equal nil
                         (progn (whitespace-tests-whitespace-mode-on)
                                buffer-display-table)))
      ;test the display table restores correctly
      (should (equal nil
                     (progn (whitespace-turn-off)
                            buffer-display-table)))
      ;test the stored display table is preserved
      (should (equal nil
                     (progn (whitespace-tests-whitespace-mode-on)
                            (whitespace-tests-whitespace-mode-on)
                            (whitespace-turn-off)
                            buffer-display-table))))))

(provide 'whitespace-tests)

;;; whitespace-tests.el ends here
