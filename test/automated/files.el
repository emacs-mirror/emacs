;;; files.el --- tests for file handling.

;; Copyright (C) 2012 Free Software Foundation, Inc.

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

(defvar files-test-var1 nil)

(defun files-test-fun1 ()
  (setq files-test-var1 t))

(ert-deftest files-test-bug12155 ()
  "Test for http://debbugs.gnu.org/12155 ."
  (with-temp-buffer
    (insert "text\n"
            ";; Local Variables:\n"
            ";; eval: (files-test-fun1)\n"
            ";; End:\n")
    (let ((enable-local-variables :safe)
          (enable-local-eval 'maybe))
      (hack-local-variables)
      (should (eq files-test-var1 nil)))))

(ert-deftest files-test-disable-local-variables ()
  "Test that setting enable-local-variables to nil works."
  (with-temp-buffer
    (insert "text\n"
            ";; Local Variables:\n"
            ";; files-test-var1: t\n"
            ";; End:\n")
    (let ((enable-local-variables nil))
      (hack-local-variables)
      (should (eq files-test-var1 nil)))))

;;; files.el ends here
