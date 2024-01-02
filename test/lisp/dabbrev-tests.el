;;; dabbrev-tests.el --- Test suite for dabbrev.  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Alan Third <alan@idiocy.org>
;; Keywords: dabbrev

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
(require 'dabbrev)

(ert-deftest dabbrev-expand-test ()
  "Test for bug#1948.
When `dabbrev-eliminate-newlines' is non-nil (the default),
repeated calls to `dabbrev-expand' can result in the source of
first expansion being replaced rather than the destination."
  (with-temp-buffer
   (insert "ab  x\na\nab  y")
   (goto-char 8)
   (save-window-excursion
     (set-window-buffer nil (current-buffer))
     (execute-kbd-macro (kbd "M-/ SPC M-/ M-/")))
   (should (string= (buffer-string) "ab  x\nab y\nab  y"))))

(ert-deftest dabbrev-completion-test ()
  "Test for bug#17899.
dabbrev-completion should not look for expansions in other
buffers unless a prefix argument is used."
  (with-temp-buffer
    (insert "axy")
    (with-temp-buffer
      (insert "abc\na")
      (goto-char 6)
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (execute-kbd-macro (kbd "C-M-/")))
      (should (string= (buffer-string) "abc\nabc")))))

(ert-deftest dabbrev-completion-test-with-argument ()
  "Test for bug#17899.
dabbrev-completion should not complete because it has found
multiple expansions."
  (with-temp-buffer
    (insert "axy")
    (with-temp-buffer
      (insert "abc\na")
      (goto-char 6)
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (execute-kbd-macro (kbd "C-u C-u C-M-/")))
      (should (string= (buffer-string) "abc\na")))))

;;; dabbrev-tests.el ends here
