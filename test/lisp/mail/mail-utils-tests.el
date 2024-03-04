;;; mail-utils-tests.el --- tests for mail-utils.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'sasl)
(require 'mail-utils)

(ert-deftest mail-utils-tests-mail-quote-printable ()
  (should (equal (mail-quote-printable "abc") "abc"))
  (should (equal (mail-quote-printable "åäö") "=E5=E4=F6"))
  (should (equal (mail-quote-printable "åäö" t) "=?ISO-8859-1?Q?=E5=E4=F6?=")))

(ert-deftest mail-utils-tests-mail-quote-printable-region ()
  (with-temp-buffer
    (insert "?=\"\"")
    (mail-quote-printable-region (point-min) (point-max))
    (should (equal (buffer-string) "=3F=3D=22=22")))
  (with-temp-buffer
    (insert "x")
    (mail-quote-printable-region (point-min) (point-max) t)
    (should (equal (buffer-string) "=?=?ISO-8859-1?Q?x"))))

(ert-deftest mail-utils-tests-mail-unquote-printable ()
  (should (equal (mail-unquote-printable "=E5=E4=F6") "åäö"))
  (should (equal (mail-unquote-printable "=?ISO-8859-1?Q?=E5=E4=F6?=" t) "åäö")))

(ert-deftest mail-utils-tests-mail-unquote-printable-region ()
  (with-temp-buffer
    (insert "=E5=E4=F6")
    (mail-unquote-printable-region (point-min) (point-max))
    (should (equal (buffer-string) "åäö")))
  (with-temp-buffer
    (insert "=?ISO-8859-1?Q?=E5=E4=F6?=")
    (mail-unquote-printable-region (point-min) (point-max) t)
    (should (equal (buffer-string) "åäö"))))

(ert-deftest mail-utils-tests-mail-strip-quoted-names ()
  (should (equal (mail-strip-quoted-names
                  "\"foo\" <foo@example.org>, bar@example.org")
                 "foo@example.org, bar@example.org")))

(ert-deftest mail-utils-tests-mail-dont-reply-to ()
  (let ((mail-dont-reply-to-names "foo@example.org"))
    (should (equal (mail-dont-reply-to "foo@example.org, bar@example.org")
                   "bar@example.org"))))


(ert-deftest mail-utils-tests-mail-fetch-field ()
  (with-temp-buffer
    (insert "Foo: bar\nBaz: zut")
    (should (equal (mail-fetch-field "Foo") "bar"))))

(ert-deftest mail-utils-tests-mail-parse-comma-list ()
  (with-temp-buffer
    (insert "foo@example.org,bar@example.org,baz@example.org")
    (goto-char (point-min))
    (should (equal (mail-parse-comma-list)
                   '("baz@example.org" "bar@example.org" "foo@example.org")))))

(ert-deftest mail-utils-tests-mail-comma-list-regexp ()
  (should (equal (mail-comma-list-regexp
                  "foo@example.org,bar@example.org,baz@example.org")
                 "foo@example.org\\|bar@example.org\\|baz@example.org")))

(ert-deftest mail-utils-tests-mail-rfc822-time-zone ()
  (with-suppressed-warnings ((obsolete mail-rfc822-time-zone))
    (should (stringp (mail-rfc822-time-zone (current-time))))))

(ert-deftest mail-utils-test-mail-rfc822-date/contains-year ()
  (should (string-match (rx " 20" digit digit " ")
                        (mail-rfc822-date))))

(ert-deftest mail-utils-test-mail-mbox-from ()
  (with-temp-buffer
    (insert "Subject: Hello
From: jrh@example.org
To: emacs-devel@gnu.org
Date: Sun, 07 Feb 2021 22:46:37 -0500")
    (should (equal (mail-mbox-from)
                   "From jrh@example.org Sun Feb  7 22:46:37 2021\n"))))

(provide 'mail-utils-tests)
;;; mail-utils-tests.el ends here
