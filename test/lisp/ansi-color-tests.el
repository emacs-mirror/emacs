;;; ansi-color-tests.el --- Test suite for ansi-color  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl-lib))

(defvar yellow (aref ansi-color-names-vector 3))
(defvar bright-yellow (aref ansi-bright-color-names-vector 3))

(defvar test-strings
  `(("\e[33mHello World\e[0m" "Hello World"
     (foreground-color . ,yellow))
    ("\e[43mHello World\e[0m" "Hello World"
     (background-color . ,yellow))
    ("\e[93mHello World\e[0m" "Hello World"
     (foreground-color . ,bright-yellow))
    ("\e[103mHello World\e[0m" "Hello World"
     (background-color . ,bright-yellow))
    ("\e[1;33mHello World\e[0m" "Hello World"
     (bold (foreground-color . ,yellow))
     (bold (foreground-color . ,bright-yellow)))
    ("\e[33;1mHello World\e[0m" "Hello World"
     (bold (foreground-color . ,yellow))
     (bold (foreground-color . ,bright-yellow)))
    ("\e[1m\e[33mHello World\e[0m" "Hello World"
     (bold (foreground-color . ,yellow))
     (bold (foreground-color . ,bright-yellow)))
    ("\e[33m\e[1mHello World\e[0m" "Hello World"
     (bold (foreground-color . ,yellow))
     (bold (foreground-color . ,bright-yellow)))
    ("\e[1m\e[3m\e[5mbold italics blink\e[0m" "bold italics blink"
     (bold italic success))))

(ert-deftest ansi-color-apply-on-region-test ()
  (pcase-dolist (`(,input ,text ,face) test-strings)
    (with-temp-buffer
      (insert input)
      (ansi-color-apply-on-region (point-min) (point-max))
      (should (equal (buffer-string) text))
      (should (equal (get-char-property (point-min) 'face) face))
      (should (not (equal (overlays-at (point-min)) nil))))))

(ert-deftest ansi-color-apply-on-region-bold-is-bright-test ()
  (pcase-dolist (`(,input ,text ,face ,bright-face) test-strings)
    (with-temp-buffer
      (let ((ansi-color-bold-is-bright t))
        (insert input)
        (ansi-color-apply-on-region (point-min) (point-max))
        (should (equal (buffer-string) text))
        (should (equal (get-char-property (point-min) 'face)
                       (or bright-face face)))
        (should (not (equal (overlays-at (point-min)) nil)))))))

(ert-deftest ansi-color-apply-on-region-preserving-test ()
    (dolist (pair test-strings)
      (with-temp-buffer
        (insert (car pair))
        (ansi-color-apply-on-region (point-min) (point-max) t)
        (should (equal (buffer-string) (car pair))))))

(provide 'ansi-color-tests)

;;; ansi-color-tests.el ends here
