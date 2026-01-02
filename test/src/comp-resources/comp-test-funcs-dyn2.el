;;; comp-test-funcs-dyn2.el -*- lexical-binding: nil; no-byte-compile: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Alan Mackenzie <acm@muc.de>

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
;; Test the compilation of a function under dynamic binding.

;;; Code:

(defun comp-tests-result-lambda ()
  (lambda (bar) (car bar)))

(provide 'comp-test-funcs-dyn2)
;;; comp-test-funcs-dyn2.el ends here.
