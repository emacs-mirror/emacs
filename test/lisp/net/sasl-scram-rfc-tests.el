;;; sasl-scram-rfc-tests.el --- tests for SCRAM       -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

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

;; Test cases from RFC 5802 and RFC 7677.

;;; Code:

(require 'sasl)
(require 'sasl-scram-rfc)

(ert-deftest sasl-scram-sha-1-test ()
  ;; The following strings are taken from section 5 of RFC 5802.
  (let ((client
	 (sasl-make-client (sasl-find-mechanism '("SCRAM-SHA-1"))
			   "user"
			   "imap"
			   "localhost"))
	(data "r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096")
	(c-nonce "fyko+d2lbbFgONRv9qkxdawL")
	(sasl-read-passphrase
	 (lambda (_prompt) (copy-sequence "pencil"))))
    (sasl-client-set-property client 'c-nonce c-nonce)
    (should
     (equal
      (sasl-scram-sha-1-client-final-message client (vector nil data))
      "c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="))

    ;; This should not throw an error:
    (sasl-scram-sha-1-authenticate-server client (vector nil "v=rmF9pqV8S7suAoZWja4dJRkFsKQ=
"))))

(require 'sasl-scram-sha256)

(ert-deftest sasl-scram-sha-256-test ()
  ;; The following strings are taken from section 3 of RFC 7677.
  (let ((client
         (sasl-make-client (sasl-find-mechanism '("SCRAM-SHA-256"))
                           "user"
                           "imap"
                           "localhost"))
        (data "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096")
        (c-nonce "rOprNGfwEbeRWgbNEkqO")
        (sasl-read-passphrase
         (lambda (_prompt) (copy-sequence "pencil"))))
    (sasl-client-set-property client 'c-nonce c-nonce)
    (should
     (equal
      (sasl-scram-sha-256-client-final-message client (vector nil data))
      "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ="))

    ;; This should not throw an error:
    (sasl-scram-sha-256-authenticate-server client (vector nil "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4="))))

;;; sasl-scram-rfc-tests.el ends here
