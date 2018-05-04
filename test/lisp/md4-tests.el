;;; md4-tests.el --- tests for md4.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Maintainer: emacs-devel@gnu.org

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
(require 'md4)

(defun md4-tests-digest->hex (str)
  "Print digest STR in hexadecimal."
  (mapconcat (lambda (x) (format "%02x" x)) str ""))

(ert-deftest md4-test-rfc1320 ()
  "Verify the test suite results in RFC 1320.
See <https://tools.ietf.org/html/rfc1320>."
  (should
   (equal (md4-tests-digest->hex (md4 "" 0))
          "31d6cfe0d16ae931b73c59d7e0c089c0"))
  (should
   (equal (md4-tests-digest->hex (md4 "a" 1))
          "bde52cb31de33e46245e05fbdbd6fb24"))
  (should
   (equal (md4-tests-digest->hex (md4 "abc" 3))
          "a448017aaf21d8525fc10ae87aa6729d"))
  (should
   (equal (md4-tests-digest->hex (md4 "message digest" 14))
          "d9130a8164549fe818874806e1c7014b"))
  (should
   (equal (md4-tests-digest->hex (md4 "abcdefghijklmnopqrstuvwxyz" 26))
          "d79e1c308aa5bbcdeea8ed63df412da9"))
  (should
   (equal (md4-tests-digest->hex
           (md4 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" 62))
          "043f8582f241db351ce627e153e7f0e4"))
  (should
   (equal (md4-tests-digest->hex
           (md4 "12345678901234567890123456789012345678901234567890123456789012345678901234567890" 80))
          "e33b4ddc9c38f2199c3e7b164fcc0536")))

;;; md4-tests.el ends here
