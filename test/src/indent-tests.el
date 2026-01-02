;;; indent-tests.el --- tests for src/indent.c  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

(ert-deftest indent-tests-move-to-column-invis-1tab ()
  "Test `move-to-column' when a TAB is followed by invisible text."
  (should
   (string=
    (with-temp-buffer
      (insert "\tLine starting with INVISIBLE text after TAB\n")
      (add-text-properties 2 21 '(invisible t))
      (goto-char (point-min))
      (move-to-column 7 t)
      (buffer-substring-no-properties 1 8))
    "       ")))

(ert-deftest indent-tests-move-to-column-invis-2tabs ()
  "Test `move-to-column' when 2 TABs are followed by invisible text."
  (should
   (string=
    (with-temp-buffer
      (insert "\t\tLine starting with INVISIBLE text after TAB\n")
      (add-text-properties 3 22 '(invisible t))
      (goto-char (point-min))
      (move-to-column 12 t)
      (buffer-substring-no-properties 1 11))
    "\t    \tLine")))

(ert-deftest indent-tests-move-to-column-invis-between-tabs ()
  "Test `move-to-column' when 2 TABs are mixed with invisible text."
  (should
   (string=
    (with-temp-buffer
      (insert "\txxx\tLine starting with INVISIBLE text after TAB\n")
      (add-text-properties 6 25 '(invisible t))
      (add-text-properties 2 5 '(invisible t))
      (goto-char (point-min))
      (move-to-column 12 t)
      (buffer-substring-no-properties 1 14))
    "\txxx    \tLine")))

;;; indent-tests.el ends here
