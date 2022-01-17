;;; textsec-tests.el --- Tests for textsec.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(require 'textsec)
(require 'ert)
(require 'ert-x)

(ert-deftest test-scripts ()
  (should (equal (textsec-scripts "Circle")
                 '((latin) (latin) (latin) (latin) (latin) (latin))))
  (should (textsec-single-script-p "Circle"))

  (should (equal (textsec-scripts "СігсӀе")
                 '((cyrillic) (cyrillic) (cyrillic)
                   (cyrillic) (cyrillic) (cyrillic))))
  (should (textsec-single-script-p "СігсӀе"))

  (should (equal (textsec-scripts "Сirсlе")
                 '((cyrillic) (latin) (latin) (cyrillic) (latin) (cyrillic))))
  (should-not (textsec-single-script-p "Сirсlе"))

  (should (equal (textsec-scripts "Circ1e")
                 '((latin) (latin) (latin) (latin) (common) (latin))))
  (should (textsec-single-script-p "Circ1e"))

  (should (equal (textsec-scripts "C𝗂𝗋𝖼𝗅𝖾")
                 '((latin) (common) (common) (common) (common) (common))))
  (should (textsec-single-script-p "C𝗂𝗋𝖼𝗅𝖾"))

  (should (equal (textsec-scripts "𝖢𝗂𝗋𝖼𝗅𝖾")
                 '((common) (common) (common) (common) (common) (common))))
  (should (textsec-single-script-p "𝖢𝗂𝗋𝖼𝗅𝖾"))

  (should (equal (textsec-scripts "〆切")
                 '((common han) (han))))
  (should (textsec-single-script-p "〆切"))

  (should (equal (textsec-scripts "ねガ")
                 '((hiragana) (katakana))))
  (should (textsec-single-script-p "ねガ")))

(ert-deftest test-minimal-scripts ()
  (should (equal (textsec-covering-scripts "Circle")
                 '(latin)))
  (should (equal (textsec-covering-scripts "Сirсlе")
                 '(cyrillic latin)))
  (should (equal (textsec-covering-scripts "〆切")
                 '(han))))

(ert-deftest test-restriction-level ()
  (should (eq (textsec-restriction-level "foo")
              'ascii-only))
  (should (eq (textsec-restriction-level "C𝗂𝗋𝖼𝗅𝖾")
              'single-script))
  (should (eq (textsec-restriction-level "切foo")
              'highly-restrictive))
  (should (eq (textsec-restriction-level "հfoo")
              'moderately-retrictive))
  (should (eq (textsec-restriction-level "Сirсlе")
              'unrestricted)))

;;; textsec-tests.el ends here
