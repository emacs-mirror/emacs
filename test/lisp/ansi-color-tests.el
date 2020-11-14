;;; ansi-color-tests.el --- Test suite for ansi-color  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Pablo Barbáchano <pablob@amazon.com>
;; Keywords: ansi

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

(require 'ansi-color)

(defvar test-strings '(("\e[33mHello World\e[0m" . "Hello World")
                       ("\e[1m\e[3m\e[5mbold italics blink\e[0m" . "bold italics blink")))

(ert-deftest ansi-color-apply-on-region-test ()
    (dolist (pair test-strings)
      (with-temp-buffer
        (insert (car pair))
        (ansi-color-apply-on-region (point-min) (point-max))
        (should (equal (buffer-string) (cdr pair)))
        (should (not (equal (overlays-at (point-min)) nil))))))

(ert-deftest ansi-color-apply-on-region-preserving-test ()
    (dolist (pair test-strings)
      (with-temp-buffer
        (insert (car pair))
        (ansi-color-apply-on-region (point-min) (point-max) t)
        (should (equal (buffer-string) (car pair))))))

(provide 'ansi-color-tests)

;;; ansi-color-tests.el ends here
