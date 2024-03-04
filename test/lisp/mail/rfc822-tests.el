;;; rfc822-tests.el --- Tests for rfc822.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'rfc822)

(defmacro rfc822-tests-deftest (email desc &optional valid)
  `(ert-deftest ,(intern (format "rfc822-email-%s-%s"
                                 (if valid "valid" "invalid")
                                 desc)) ()
     (if ,valid
         (should (equal (rfc822-addresses ,email) (list ,email)))
       (let ((addresses (rfc822-addresses ,email)))
         ;; `rfc822-addresses' returns a string if parsing fails.
         (while (and (consp addresses)
                     (not (eq (string-to-char (car addresses)) ?\()))
           (setq addresses (cdr addresses)))
         ;; Found saved error.
         (should (= (length addresses) 1))))))

;;;; Valid emails

(rfc822-tests-deftest "email@example.org" "email" t)
(rfc822-tests-deftest "firstname.lastname@example.org" "dot-in-address" t)
(rfc822-tests-deftest "email@subdomain.example.org" "dot-in-subdomain" t)
(rfc822-tests-deftest "firstname+lastname@example.org" "contains-plus-sign" t)
(rfc822-tests-deftest "email@123.123.123.123" "domain-valid-ip" t)
(rfc822-tests-deftest "email@[123.123.123.123]" "domain-valid-ip-square-bracket" t)
(rfc822-tests-deftest "\"email\"@example.org" "quotes-around-email" t)
(rfc822-tests-deftest "1234567890@example.org" "digits-in-address" t)
(rfc822-tests-deftest "email@example-one.com" "dash-in-domain-name" t)
(rfc822-tests-deftest "_______@example.org" "underscore-in-address" t)
(rfc822-tests-deftest "email@example.name" "dotname-tld" t)
(rfc822-tests-deftest "email@example.co.jp" "dot-in-tld" t)
(rfc822-tests-deftest "firstname-lastname@example.org" "dash-in-address" t)
(rfc822-tests-deftest "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm@example.org" "address-long" t)

;;;; Invalid emails

(rfc822-tests-deftest "#@%^%#$@#$@#.com" "garbage")
(rfc822-tests-deftest "@example.org" "missing-username")
(rfc822-tests-deftest "email@example@example.org" "two-at-signs")
(rfc822-tests-deftest ".email@example.org" "address-leading-dot")
(rfc822-tests-deftest "email.@example.org" "address-trailing-dot")
(rfc822-tests-deftest "email..email@example.org" "address-multiple-dots")
(rfc822-tests-deftest "email@example..org" "domain-multiple-dots")
(rfc822-tests-deftest "email@example.org." "domain-trailing-dot")
(rfc822-tests-deftest "email@.example.org" "domain-leading-dot")
(rfc822-tests-deftest "test\\@test@example.org" "address-escaped-at-sign")

;; FIXME: Should these fail?
;; (rfc822-tests-deftest "plainaddress" "missing-at-sign-and-domain")
;; (rfc822-tests-deftest "email@example.org (J. Random Hacker)" "text-following-email")
;; (rfc822-tests-deftest "email@-example.org" "leading-dash-in-domain-is-invalid")
;; (rfc822-tests-deftest "email@example-.org" "trailing-dash-in-domain-is-invalid")
;; (rfc822-tests-deftest "あいうえお@example.org" "address-unicode-chars")
;; (rfc822-tests-deftest "email.example.org" "missing-at")
;; (rfc822-tests-deftest "email@111.222.333.44444" "invalid-IP-format")
;; (rfc822-tests-deftest "email@domain" "missing-top-level-domain")
;; (rfc822-tests-deftest "email@domain.web" ".web-is-not-a-valid-top-level-domain")

(provide 'rfc822-tests)
;;; rfc822-tests.el ends here
