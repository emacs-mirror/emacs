;;; sh-script-tests.el --- Tests for sh-script.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

(require 'sh-script)
(require 'ert)
(require 'ert-x)

(ert-deftest test-sh-script-indentation ()
  (with-temp-buffer
    (insert "relative-path/to/configure --prefix=$prefix\\
             --with-x")
    (shell-script-mode)
    (goto-char (point-min))
    (forward-line 1)
    (indent-for-tab-command)
    (should (equal
             (buffer-substring-no-properties (point-min) (point-max))
             "relative-path/to/configure --prefix=$prefix\\
			   --with-x"))))

(ert-deftest test-basic-sh-indentation ()
  (with-temp-buffer
    (insert "myecho () {\necho foo\n}\n")
    (shell-script-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
  "myecho () {
    echo foo
}
"))))

(ert-deftest test-indentation ()
  (ert-test-erts-file (ert-resource-file "sh-indents.erts")))

(defun test-sh-back (string &optional pos)
  (with-temp-buffer
    (shell-script-mode)
    (insert string)
    (sh-smie--default-backward-token)
    (= (point) (or pos 1))))

(ert-deftest test-backward-token ()
  (should (test-sh-back "foo"))
  (should (test-sh-back "foo.bar"))
  (should (test-sh-back "foo\\1bar"))
  (should (test-sh-back "foo\\\nbar"))
  (should (test-sh-back "foo\\\n\\\n\\\nbar"))
  (should (test-sh-back "foo"))
  (should-not (test-sh-back "foo;bar"))
  (should (test-sh-back "foo#zot")))

;;; sh-script-tests.el ends here
