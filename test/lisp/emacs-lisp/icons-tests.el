;;; icons-tests.el --- Tests for icons.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'icons)
(require 'ert)
(require 'ert-x)
(require 'cus-edit)

(define-icon icon-test1 nil
  '((symbol ">")
    (text "great"))
  "Test icon"
  :version "29.1")

(define-icon icon-test2 icon-test1
  '((text "child"))
  "Test icon"
  :version "29.1")

(deftheme test-icons-theme "")

(ert-deftest test-icon-theme ()
  (let ((icon-preference '(image emoji symbol text)))
    (should (equal (icon-string 'icon-test1) ">")))
  (let ((icon-preference '(text)))
    (should (equal (icon-string 'icon-test1) "great")))
  (custom-theme-set-icons
   'test-icons-theme
   '(icon-test1 ((symbol "<") (text "less"))))
  (let ((icon-preference '(image emoji symbol text)))
    (should (equal (icon-string 'icon-test1) ">"))
    (enable-theme 'test-icons-theme)
    (should (equal (icon-string 'icon-test1) "<"))))

(ert-deftest test-icon-inheretance ()
  (let ((icon-preference '(image emoji symbol text)))
    (should (equal (icon-string 'icon-test2) ">")))
  (let ((icon-preference '(text)))
    (should (equal (icon-string 'icon-test2) "child"))))

;;; icons-tests.el ends here
