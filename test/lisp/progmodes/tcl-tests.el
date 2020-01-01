;;; tcl-tests.el --- Test suite for tcl-mode

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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
(require 'tcl)

;; From bug#23565
(ert-deftest tcl-mode-beginning-of-defun-1 ()
  (with-temp-buffer
    (tcl-mode)
    (insert "proc bad {{value \"\"}} {\n    # do something\n}")
    (should (beginning-of-defun))
    (should (= (point) (point-min)))
    (end-of-defun)
    (should (= (point) (point-max)))))

;; From bug#23565
(ert-deftest tcl-mode-beginning-of-defun-2 ()
  (with-temp-buffer
    (tcl-mode)
    (insert "proc good {{value}} {\n    # do something\n}")
    (should (beginning-of-defun))
    (should (= (point) (point-min)))
    (end-of-defun)
    (should (= (point) (point-max)))))

(ert-deftest tcl-mode-function-name ()
  (with-temp-buffer
    (tcl-mode)
    (insert "proc notinthis {} {\n  # nothing\n}\n\n")
    (should-not (add-log-current-defun))))

(ert-deftest tcl-mode-function-name ()
  (with-temp-buffer
    (tcl-mode)
    (insert "proc simple {} {\n  # nothing\n}")
    (backward-char 3)
    (should (equal "simple" (add-log-current-defun)))))

(ert-deftest tcl-mode-function-name ()
  (with-temp-buffer
    (tcl-mode)
    (insert "proc inthis {} {\n  # nothing\n")
    (should (equal "inthis" (add-log-current-defun)))))

;; From bug#32035
(ert-deftest tcl-mode-namespace-indent ()
  (with-temp-buffer
    (tcl-mode)
    (let ((text "namespace eval Foo {\n    variable foo\n}\n"))
      (insert text)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) text)))))

(provide 'tcl-tests)

;;; tcl-tests.el ends here
