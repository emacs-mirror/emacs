;;; dig-tests.el --- Tests for dig.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
(require 'dig)

(defvar dig-test-result-data "
; <<>> DiG 9.11.16-2-Debian <<>> gnu.org
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 7777
;; flags: qr rd ra; QUERY: 1, ANSWER: 1, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 4096
;; QUESTION SECTION:
;gnu.org.			IN	A

;; ANSWER SECTION:
gnu.org.		300	IN	A	111.11.111.111

;; Query time: 127 msec
;; SERVER: 192.168.0.1#53(192.168.0.1)
;; WHEN: Sun Apr 26 00:47:55 CEST 2020
;; MSG SIZE  rcvd: 52

" "Data used to test dig.el.")

(ert-deftest dig-test-dig-extract-rr ()
  (with-temp-buffer
    (insert dig-test-result-data)
    (should (equal (dig-extract-rr "gnu.org")
                   "gnu.org.		300	IN	A	111.11.111.111"))))

(provide 'dig-tests)
;;; dig-tests.el ends here
