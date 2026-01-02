;;; puny-tests.el --- tests for net/puny.el  -*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
(require 'puny)

(ert-deftest puny-test-encode ()
  "Test puny encoding."
  (should (string= (puny-encode-string "bücher") "xn--bcher-kva")))

(ert-deftest puny-test-decode ()
  "Test puny decoding."
  (should (string= (puny-decode-string "xn--bcher-kva") "bücher")))

(ert-deftest puny-test-encode2 ()
  "Test puny encoding."
  (should (string= (puny-encode-string "חנוך") "xn--9dbdkw")))

(ert-deftest puny-test-decode2 ()
  "Test puny decoding."
  (should (string= (puny-decode-string "xn--9dbdkw") "חנוך")))

(ert-deftest puny-test-encode-domain ()
  (should (string= (puny-encode-domain "åäö.se") "xn--4cab6c.se"))
  (should (string= (puny-encode-domain "яндекс.рф") "xn--d1acpjx3f.xn--p1ai")))

(ert-deftest puny-test-decode-domain ()
  (should (string= (puny-decode-domain "xn--4cab6c.se") "åäö.se"))
  (should (string= (puny-decode-domain "xn--d1acpjx3f.xn--p1ai") "яндекс.рф")))

(ert-deftest puny-highly-restrictive-domain-p ()
  (should (puny-highly-restrictive-domain-p "foo.bar.org"))
  (should (puny-highly-restrictive-domain-p "foo.abcåäö.org"))
  (should (puny-highly-restrictive-domain-p "foo.ர.org"))
  ;; Disallow unicode character 2044, visually similar to "/".
  (should-not (puny-highly-restrictive-domain-p "www.yourbank.com⁄login⁄checkUser.jsp?inxs.ch"))
  ;; Disallow mixing scripts.
  (should-not (puny-highly-restrictive-domain-p "åர.org"))
  ;; Only allowed in moderately restrictive.
  (should-not (puny-highly-restrictive-domain-p "Teχ.org"))
  (should-not (puny-highly-restrictive-domain-p "HλLF-LIFE.org"))
  (should-not (puny-highly-restrictive-domain-p "Ωmega.org"))
  ;; Only allowed in unrestricted.
  (should-not (puny-highly-restrictive-domain-p "I♥NY.org")))

(ert-deftest puny-normalize ()
  (should (equal (puny-encode-string (string-glyph-compose "Bä.com"))
                 "xn--b.com-gra"))
  (should (equal (puny-encode-string "Bä.com")
                 "xn--b.com-gra"))
  (should (equal (puny-encode-string "Bä.com") "xn--b.com-gra")))

;;; TODO!
;; puny-resources/IdnaTestV2.txt has a bunch of tests, and they should
;; be implemented.  However, the puny encoding does not fully
;; implement https://www.unicode.org/reports/tr46/#Conformance yet, so
;; it'll fail.

;;; puny-tests.el ends here
