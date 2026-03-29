;;; m4-mode-tests.el --- tests for m4-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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
(require 'ert-font-lock)
(require 'm4-mode)

(ert-deftest m4-current-defun-name ()
  "Test `m4-current-defun-name' behavior."
  (with-temp-buffer
    (m4-mode)
    (insert "define(`a', `')")
    (should (equal (m4-current-defun-name) "a"))
    ;; Not at toplevel.
    (insert "define(`b', `')")
    (should (equal (m4-current-defun-name) "a"))
    (insert "\nm4_define(`c', `')")
    (should (equal (m4-current-defun-name) "c"))
    (insert "\nAC_DEFUN([d], [])")
    (should (equal (m4-current-defun-name) "d"))
    (insert "\nAU_DEFUN([e], [])")
    (should (equal (m4-current-defun-name) "e"))))

(ert-deftest m4-mode-comment-syntax ()
  "Test `m4-mode' comment syntax."
  (with-temp-buffer
    (m4-mode)
    (insert "# comment")
    (should (eq (syntax-ppss-context (syntax-ppss)) 'comment))
    (insert ?\n)
    (should-not (syntax-ppss-context (syntax-ppss)))))

(ert-font-lock-deftest-file m4-mode-font-lock
  "Test `m4-mode' font lock."
  m4-mode "font-lock.m4")

;;; m4-mode-tests.el ends here
