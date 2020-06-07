;;; battery-tests.el --- tests for battery.el -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

(require 'battery)

(ert-deftest battery-linux-proc-apm-regexp ()
  "Test `battery-linux-proc-apm-regexp'."
  (let ((str "1.16 1.2 0x07 0x01 0xff 0x80 -1% -1 ?"))
    (should (string-match battery-linux-proc-apm-regexp str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "1.16"))
    (should (equal (match-string 2 str) "1.2"))
    (should (equal (match-string 3 str) "07"))
    (should (equal (match-string 4 str) "01"))
    (should (equal (match-string 5 str) "ff"))
    (should (equal (match-string 6 str) "80"))
    (should (equal (match-string 7 str) "-1"))
    (should (equal (match-string 8 str) "-1"))
    (should (equal (match-string 9 str) "?")))
  (let ((str "1.16 1.2 0x03 0x00 0x00 0x01 99% 1792 min"))
    (should (string-match battery-linux-proc-apm-regexp str))
    (should (equal (match-string 0 str) str))
    (should (equal (match-string 1 str) "1.16"))
    (should (equal (match-string 2 str) "1.2"))
    (should (equal (match-string 3 str) "03"))
    (should (equal (match-string 4 str) "00"))
    (should (equal (match-string 5 str) "00"))
    (should (equal (match-string 6 str) "01"))
    (should (equal (match-string 7 str) "99"))
    (should (equal (match-string 8 str) "1792"))
    (should (equal (match-string 9 str) "min"))))

(ert-deftest battery-format ()
  "Test `battery-format'."
  (should (equal (battery-format "" ()) ""))
  (should (equal (battery-format "" '((?b . "-"))) ""))
  (should (equal (battery-format "%a%b%p%%" '((?b . "-") (?p . "99")))
                 "-99%")))

;;; battery-tests.el ends here
