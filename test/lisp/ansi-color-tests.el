;;; ansi-color-tests.el --- Test suite for ansi-color  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Pablo Barbáchano <pablob@amazon.com>

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

(defvar ansi-color-tests--strings
  (let ((bright-yellow (face-foreground 'ansi-color-bright-yellow nil 'default))
        (yellow (face-foreground 'ansi-color-yellow nil 'default)))
    `(("Hello World" "Hello World")
      ("\e[33mHello World\e[0m" "Hello World"
       (:foreground ,yellow))
      ("\e[43mHello World\e[0m" "Hello World"
       (:background ,yellow))
      ("\e[93mHello World\e[0m" "Hello World"
       (:foreground ,bright-yellow))
      ("\e[103mHello World\e[0m" "Hello World"
       (:background ,bright-yellow))
      ("\e[1;33mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[33;1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[1m\e[33mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[33m\e[1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[1m\e[3m\e[5mbold italics blink\e[0m" "bold italics blink"
       (ansi-color-bold ansi-color-italic ansi-color-slow-blink))
      ("\e[10munrecognized\e[0m" "unrecognized"))))

(ert-deftest ansi-color-apply-on-region-test ()
  (pcase-dolist (`(,input ,text ,face) ansi-color-tests--strings)
    (with-temp-buffer
      (insert input)
      (ansi-color-apply-on-region (point-min) (point-max))
      (should (equal (buffer-string) text))
      (should (equal (get-char-property (point-min) 'face) face))
      (when face
        (should (overlays-at (point-min)))))))

(ert-deftest ansi-color-apply-on-region-bold-is-bright-test ()
  (pcase-dolist (`(,input ,text ,normal-face ,bright-face)
                 ansi-color-tests--strings)
    (with-temp-buffer
      (let ((ansi-color-bold-is-bright t)
            (face (or bright-face normal-face)))
        (insert input)
        (ansi-color-apply-on-region (point-min) (point-max))
        (should (equal (buffer-string) text))
        (should (equal (get-char-property (point-min) 'face) face))
        (when face
          (should (overlays-at (point-min))))))))

(ert-deftest ansi-color-apply-on-region-preserving-test ()
  (dolist (pair ansi-color-tests--strings)
    (with-temp-buffer
      (insert (car pair))
      (ansi-color-apply-on-region (point-min) (point-max) t)
      (should (equal (buffer-string) (car pair))))))

(provide 'ansi-color-tests)

;;; ansi-color-tests.el ends here
