;;; derived-tests.el --- tests for derived.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

(define-derived-mode derived-tests--parent-mode prog-mode "P"
  :after-hook
  (let ((f (let ((x "S")) (lambda () x))))
    (insert (format "AFP=%s " (let ((x "D")) (funcall f)))))
  (insert "PB "))

(define-derived-mode derived-tests--child-mode derived-tests--parent-mode "C"
  :after-hook
  (let ((f (let ((x "S")) (lambda () x))))
    (insert (format "AFC=%s " (let ((x "D")) (funcall f)))))
  (insert "CB "))

(ert-deftest derived-tests-after-hook-lexical ()
  (with-temp-buffer
    (let ((derived-tests--child-mode-hook
           (lambda () (insert "MH "))))
      (derived-tests--child-mode)
      (should (equal (buffer-string) "PB CB MH AFP=S AFC=S ")))))

;;; derived-tests.el ends here
