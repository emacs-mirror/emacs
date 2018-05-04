;;; faces-tests.el --- Tests for faces.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'faces)

(defgroup faces--test nil ""
  :group 'faces--test)

(defface faces--test1
  '((t :background "black" :foreground "black"))
  ""
  :group 'faces--test)

(defface faces--test2
  '((t :box 1))
  ""
  :group 'faces--test)

(ert-deftest faces--test-color-at-point ()
  (with-temp-buffer
    (insert (propertize "STRING" 'face '(faces--test2 faces--test1)))
    (goto-char (point-min))
    (should (equal (background-color-at-point) "black"))
    (should (equal (foreground-color-at-point) "black")))
  (with-temp-buffer
    (insert (propertize "STRING" 'face '(:foreground "black" :background "black")))
    (goto-char (point-min))
    (should (equal (background-color-at-point) "black"))
    (should (equal (foreground-color-at-point) "black")))
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local font-lock-comment-face 'faces--test1)
    (setq-local font-lock-constant-face 'faces--test2)
    (insert ";; `symbol'")
    (font-lock-fontify-region (point-min) (point-max))
    (goto-char (point-min))
    (should (equal (background-color-at-point) "black"))
    (should (equal (foreground-color-at-point) "black"))
    (goto-char 6)
    (should (equal (background-color-at-point) "black"))
    (should (equal (foreground-color-at-point) "black"))))

(provide 'faces-tests)
;;; faces-tests.el ends here
