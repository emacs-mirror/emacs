;;; mail-parse-tests.el --- tests for mail-parse.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
(require 'mail-parse)
(require 'subr-x)

(ert-deftest test-mail-header-parse-address-lax ()
  (should (equal (mail-header-parse-address-lax
                  "Lars Ingebrigtsen <larsi@gnus.org>")
                 '("larsi@gnus.org" . "Lars Ingebrigtsen")))
  (should (equal (mail-header-parse-address-lax
                  "Lars Ingebrigtsen larsi@gnus.org>")
                 '("larsi@gnus.org" . "Lars Ingebrigtsen")))
  (should (equal (mail-header-parse-address-lax
                  "Lars Ingebrigtsen larsi@gnus.org")
                 '("larsi@gnus.org" . "Lars Ingebrigtsen")))
  (should (equal (mail-header-parse-address-lax
                  "larsi@gnus.org (Lars Ingebrigtsen)")
                 '("larsi@gnus.org " . "Lars Ingebrigtsen")))
  (should (equal (mail-header-parse-address-lax "larsi@gnus.org")
                 '("larsi@gnus.org")))
  (should (equal (mail-header-parse-address-lax "foo")
                 nil)))

(ert-deftest test-mail-header-parse-addresses-lax ()
  (should (equal (mail-header-parse-addresses-lax
                  "Bob Weiner <rsw@gnu.org>, Mats Lidell <matsl@gnu.org>")
                 '(("rsw@gnu.org" . "Bob Weiner")
                   ("matsl@gnu.org" . "Mats Lidell")))))

(provide 'mail-parse-tests)

;;; mail-parse-tests.el ends here
