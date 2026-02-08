;;; sh-script-tests.el --- Tests for sh-script.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
    (skip-unless sh-indent-supported-here)
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
    (skip-unless sh-indent-supported-here)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
  "myecho () {
    echo foo
}
"))))

(ert-deftest test-indentation ()
  (ert-test-erts-file (ert-resource-file "sh-indents.erts")))

(ert-deftest test-indent-after-continuation ()
  (with-temp-buffer
    (insert "for f \\\nin a; do \\\ntoto; \\\ndone\n")
    (shell-script-mode)
    (skip-unless sh-indent-supported-here)
    (let ((sh-indent-for-continuation '++))
      (let ((sh-indent-after-continuation t))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string)
                       "for f \\\n\tin a; do \\\n    toto; \\\n    done\n")))
      (let ((sh-indent-after-continuation 'always))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string)
                       "for f \\\n\tin a; do \\\n\ttoto; \\\n\tdone\n")))
      (let ((sh-indent-after-continuation nil))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string)
                       "for f \\\nin a; do \\\n    toto; \\\ndone\n"))))))

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

(ert-deftest sh-script-test-do-fontification ()
  "Test that \"do\" gets fontified correctly, even with no \";\"."
  (with-temp-buffer
    (shell-script-mode)
    (insert "for i do echo 1; done")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "do")
    (forward-char -1)
    (should (equal (get-text-property (point) 'face) 'font-lock-keyword-face))))

;;; sh-script-tests.el ends here
