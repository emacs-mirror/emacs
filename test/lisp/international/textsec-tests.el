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

(ert-deftest test-mixed-numbers ()
  (should-not (textsec-mixed-numbers-p "foo"))
  (should-not (textsec-mixed-numbers-p "8foo8"))
  (should (textsec-mixed-numbers-p "8foo৪")))

(ert-deftest test-resolved ()
  (should (equal (textsec-resolved-script-set "ǉeto")
                 '(latin)))
  (should-not (textsec-resolved-script-set "Сirсlе")))

(ert-deftest test-confusable ()
  (should (equal (textsec-unconfuse-string "ǉeto") "ljeto"))
  (should (textsec-ascii-confusable-p "ǉeto"))
  (should-not (textsec-ascii-confusable-p "ljeto"))
  (should (equal (textsec-unconfuse-string "～") "〜"))
  (should-not (textsec-ascii-confusable-p "～"))

  (should (textsec-single-script-confusable-p "ǉeto" "ljeto"))
  (should-not (textsec-single-script-confusable-p "paypal" "pаypаl"))
  (should-not (textsec-single-script-confusable-p "scope""ѕсоре"))

  (should-not (textsec-mixed-script-confusable-p "ǉeto" "ljeto"))
  (should (textsec-mixed-script-confusable-p "paypal" "pаypаl"))
  (should (textsec-mixed-script-confusable-p "scope""ѕсоре"))

  (should-not (textsec-whole-script-confusable-p "ǉeto" "ljeto"))
  (should-not (textsec-whole-script-confusable-p "paypal" "pаypаl"))
  (should (textsec-whole-script-confusable-p "scope""ѕсоре")))

(ert-deftest test-suspiction-domain ()
  (should (textsec-domain-suspicious-p "foo/bar.org"))
  (should-not (textsec-domain-suspicious-p "foo.org"))
  (should (textsec-domain-suspicious-p "f\N{LEFT-TO-RIGHT ISOLATE}oo.org")))

(ert-deftest test-suspicious-local ()
  (should-not (textsec-local-address-suspicious-p "larsi"))
  (should (textsec-local-address-suspicious-p ".larsi"))
  (should (textsec-local-address-suspicious-p "larsi."))
  (should-not (textsec-local-address-suspicious-p "la.rsi"))
  (should (textsec-local-address-suspicious-p "lar..si"))

  (should-not (textsec-local-address-suspicious-p "LÅRSI"))
  (should (textsec-local-address-suspicious-p "LÅRSI"))

  (should (textsec-local-address-suspicious-p "larsi8৪")))

(ert-deftest test-suspicious-name ()
  (should-not (textsec-name-suspicious-p "Lars Ingebrigtsen"))
  (should (textsec-name-suspicious-p "LÅRS INGEBRIGTSEN"))
  (should-not (textsec-name-suspicious-p "LÅRS INGEBRIGTSEN"))

  (should (textsec-name-suspicious-p
           "Lars Ingebrigtsen\N{LEFT-TO-RIGHT ISOLATE}"))
  (should-not (textsec-name-suspicious-p
               "Lars Ingebrigtsen\N{LEFT-TO-RIGHT MARK}"))

  (should (textsec-name-suspicious-p
           "\N{LEFT-TO-RIGHT MARK}\N{LEFT-TO-RIGHT MARK}Lars Ingebrigtsen"))
  (should-not (textsec-name-suspicious-p
               "\N{LEFT-TO-RIGHT MARK}\N{RIGHT-TO-LEFT MARK}Lars Ingebrigtsen"))
  (should (textsec-name-suspicious-p
               "\N{LEFT-TO-RIGHT MARK}\N{RIGHT-TO-LEFT MARK}\N{LEFT-TO-RIGHT MARK}\N{RIGHT-TO-LEFT MARK}\N{LEFT-TO-RIGHT MARK}Lars Ingebrigtsen")))

(ert-deftest test-suspicious-email ()
  (should-not
   (textsec-email-suspicious-p "Lars Ingebrigtsen <larsi@gnus.org>"))
  (should
   (textsec-email-suspicious-p "LÅrs Ingebrigtsen <larsi@gnus.org>"))
  (should
   (textsec-email-suspicious-p "Lars Ingebrigtsen <.larsi@gnus.org>"))
  (should
   (textsec-email-suspicious-p "Lars Ingebrigtsen <larsi@gn\N{LEFT-TO-RIGHT ISOLATE}us.org>")))

;;; textsec-tests.el ends here
