;;; dns-mode-tests.el --- Test suite for dns-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

;; Author: Peder O. Klingenberg <peder@klingenberg.no>
;; Keywords: dns zone

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
(require 'dns-mode)

(ert-deftest dns-mode-tests-dns-mode-soa-increment-serial ()
  (with-temp-buffer
    (insert "$TTL 86400
@   IN  SOA     ns.icann.org. noc.dns.icann.org. (
        2015080302  ;Serial
        7200        ;Refresh
        3600        ;Retry
        1209600     ;Expire
        3600        ;Negative response caching TTL\n)")
    (dns-mode-soa-increment-serial)
    ;; Number is updated from 2015080302 to the current date
    ;; (actually, just ensure the year part is later than 2020).
    (should (string-match "\\$TTL 86400
@   IN  SOA     ns.icann.org. noc.dns.icann.org. (
        20[2-9][0-9]+  ;Serial
        7200        ;Refresh
        3600        ;Retry
        1209600     ;Expire
        3600        ;Negative response caching TTL\n)"
                          (buffer-string)))))

;;; IPv6 reverse zones
(ert-deftest dns-mode-ipv6-conversion ()
  (let ((address "2001:db8::42"))
    (should (equal (dns-mode-reverse-and-expand-ipv6 address)
                   "2.4.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa. "))
    (should (equal (dns-mode-reverse-and-expand-ipv6 address 32)
                   "8.b.d.0.1.0.0.2.ip6.arpa. "))
    (should (equal (dns-mode-reverse-and-expand-ipv6 address -112)
                   "2.4.0.0 "))))

(ert-deftest dns-mode-ipv6-text-replacement ()
  (let ((address "2001:db8::42/32"))
    (with-temp-buffer
      ;; Conversion with point directly after address
      (insert address)
      (dns-mode-ipv6-to-nibbles nil)
      (should (equal (buffer-string) "8.b.d.0.1.0.0.2.ip6.arpa. "))
      ;; Kill ring contains the expected
      (erase-buffer)
      (yank)
      (should (equal (buffer-string) address))
      ;; Point at beginning of address (and prefix arg to command)
      (goto-char (point-min))
      (dns-mode-ipv6-to-nibbles t)
      (should (equal (buffer-string) "2.4.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0 "))
      ;; Point separated from address by whitespace
      (erase-buffer)
      (insert address)
      (insert " ")
      (dns-mode-ipv6-to-nibbles nil)
      (should (equal (buffer-string) "8.b.d.0.1.0.0.2.ip6.arpa.  ")))))

;;; dns-mode-tests.el ends here
