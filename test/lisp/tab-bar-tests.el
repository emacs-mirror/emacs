;;; tab-bar-tests.el --- Tests for tab-bar.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>

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

(defun tab-bar-tests-close-other-tabs (arg)
  (tab-bar-tabs-set nil)
  (tab-rename "1")
  (tab-new) (tab-rename "2") ;; (tab-switch "2")
  (tab-new) (tab-rename "3") ;; (tab-switch "3")
  (should (eq (length (tab-bar-tabs)) 3))
  (should (equal (alist-get 'name (tab-bar--current-tab-find)) "3"))
  (tab-bar-close-other-tabs arg)
  (should (equal (alist-get 'name (tab-bar--current-tab-find))
                 (if arg (number-to-string (max 1 (min arg 3))) "3")))
  (should (eq (length (tab-bar-tabs)) 1))
  (should (eq (length tab-bar-closed-tabs) 2))
  (tab-undo)
  (tab-undo)
  (should (equal (tab-undo) "No more closed tabs to undo"))
  (should (eq (length (tab-bar-tabs)) 3))
  (should (eq (length tab-bar-closed-tabs) 0)))

(ert-deftest tab-bar-tests-close-other-tabs-default ()
  (tab-bar-tests-close-other-tabs nil))

(ert-deftest tab-bar-tests-close-other-tabs-with-arg ()
  (dotimes (i 5) (tab-bar-tests-close-other-tabs i)))

(provide 'tab-bar-tests)
;;; tab-bar-tests.el ends here
