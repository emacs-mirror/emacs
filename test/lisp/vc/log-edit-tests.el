;;; log-edit-tests.el --- Unit tests for log-edit.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

;; Unit tests for lisp/vc/log-edit.el.

;;; Code:

(require 'log-edit)
(require 'ert)

(ert-deftest log-edit-fill-entry ()
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1):
\(fun2):
\(fun3):
* file2.txt (fun4):
\(fun5):
\(fun6):
\(fun7): Some prose.
\(fun8): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.
\(fun9): Etc.")
    (goto-char (point-min))
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7): Some prose.
\(fun8): A longer description of a complicated change.  Spread over a
couple of sentences.  Long enough to be filled for several lines.
\(fun9): Etc."))
    (let ((fill-column 20)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1)
\(fun2, fun3):
* file2.txt (fun4)
\(fun5, fun6, fun7):
Some prose.
\(fun8): A longer
description of a
complicated change.
Spread over a couple
of sentences.  Long
enough to be filled
for several lines.
\(fun9): Etc."))
    (let ((fill-column 40)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7):
Some prose.
\(fun8): A longer description of a
complicated change.  Spread over a
couple of sentences.  Long enough to be
filled for several lines.
\(fun9): Etc."))))

(ert-deftest log-edit-fill-entry-indented-func-entries ()
  ;; Indenting function entries is a typical mistake caused by using a
  ;; misconfigured or non-ChangeLog specific fill function.
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1):
  (fun2):
  (fun3):
* file2.txt (fun4):
  (fun5):
  (fun6):
  (fun7): Some prose.
  (fun8): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.
  (fun9): Etc.")
    (goto-char (point-min))
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7): Some prose.
\(fun8): A longer description of a complicated change.  Spread over a
couple of sentences.  Long enough to be filled for several lines.
\(fun9): Etc."))))

(ert-deftest log-edit-fill-entry-trailing-prose ()
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1): A longer description of a complicated change.
Spread over a couple of sentences.  Long enough to be filled for several
lines."))))

(ert-deftest log-edit-fill-entry-joining ()
  ;; Join short enough function names on the same line.
  (with-temp-buffer
    (insert "* dir/file.ext (fun1):\n(fun2):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "* dir/file.ext (fun1, fun2):")))
  ;; Don't combine them if they're too long.
  (with-temp-buffer
    (insert "* dir/long-file-name.ext (a-really-long-function-name):
\(another-very-long-function-name):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "* dir/long-file-name.ext (a-really-long-function-name)
\(another-very-long-function-name):")))
  ;; Put function name on next line, if the file name is too long.
  (with-temp-buffer
    (insert "\
* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext\
 (a-really-long-function-name):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext
\(a-really-long-function-name):"))))

;;; log-edit-tests.el ends here
