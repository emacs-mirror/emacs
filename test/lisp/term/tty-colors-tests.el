;;; tty-colors-tests.el --- tests for tty-colors.el  -*- lexical-binding: t -*-

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


;;; Code:
(require 'ert)
(require 'term/tty-colors)

(ert-deftest tty-colors-test-standard-colors ()
  (should (equal (tty-color-standard-values "white") '(65535 65535 65535)))
  (should (equal (tty-color-standard-values "#F00") '(65535 0 0)))
  (should (equal (tty-color-standard-values "#00FF00") '(0 65535 0)))
  (should (equal (tty-color-standard-values "#00000000FFFF") '(0 0 65535)))
  (should (equal (tty-color-standard-values "rgb:0/0/7") '(0 0 30583)))
  (should (equal (tty-color-standard-values "rgb:0/ff/0") '(0 65535 0)))
  (should (equal (tty-color-standard-values "rgb:ffFF/0000/0000") '(65535 0 0))))

(provide 'term-tests)

;;; term-tests.el ends here
