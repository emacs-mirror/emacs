;;; pascal-tests.el --- tests for pascal.el    -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'pascal)

(ert-deftest pascal-completion ()
  ;; Bug#41740: completion functions must preserve point.
  (let ((pascal-completion-cache nil))
    (with-temp-buffer
      (pascal-mode)
      (insert "program test; var")
      (let* ((point-before (point))
             (completions (pascal-completion "var" nil 'metadata))
             (point-after (point)))
        (should (equal completions nil))
        (should (equal point-before point-after)))))

  (let ((pascal-completion-cache nil))
    (with-temp-buffer
      (pascal-mode)
      (insert "program test; function f(x : i")
      (let* ((point-before (point))
             (completions (pascal-completion "i" nil 'metadata))
             (point-after (point)))
        (should (equal completions nil))
        (should (equal point-before point-after)))))

  (let ((pascal-completion-cache nil))
    (with-temp-buffer
      (pascal-mode)
      (insert "program test; function f(x : integer) : real")
      (let* ((point-before (point))
             (completions (pascal-completion "real" nil 'metadata))
             (point-after (point)))
        (should (equal completions nil))
        (should (equal point-before point-after))))))

(ert-deftest pascal-beg-of-defun ()
  (with-temp-buffer
    (pascal-mode)
    (insert "program test; procedure p(")
    (forward-char -1)
    (pascal-beg-of-defun)
    (should (equal (point) 15))))

(provide 'pascal-tests)

;;; pascal-tests.el ends here
