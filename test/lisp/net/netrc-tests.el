;;; netrc-tests.el --- Tests for netrc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'netrc)

(ert-deftest test-netrc-parse-services ()
  (let ((netrc-services-file (ert-resource-file "services")))
    (should (equal (netrc-parse-services)
                   '(("tcpmux" 1 tcp)
                     ("smtp" 25 tcp)
                     ("http" 80 tcp)
                     ("kerberos" 88 tcp)
                     ("kerberos" 88 udp)
                     ("rtmp" 1 ddp))))))

(ert-deftest test-netrc-find-service-name ()
  (let ((netrc-services-file (ert-resource-file "services")))
    (should (equal (netrc-find-service-name 25) "smtp"))
    (should (equal (netrc-find-service-name 88 'udp) "kerberos"))
    (should-not (netrc-find-service-name 12345))))

(ert-deftest test-netrc-credentials ()
  (let ((netrc-file (ert-resource-file "authinfo")))
    (should (equal (netrc-credentials "imap.example.org")
                   '("jrh@example.org" "*foobar*")))
    (should (equal (netrc-credentials "ftp.example.org")
                   '("jrh" "*baz*")))))

(ert-deftest test-netrc-credentials ()
  (let ((netrc-file (ert-resource-file "netrc-folding")))
    (should
     (equal (netrc-parse netrc-file)
            '((("machine" . "XM") ("login" . "XL") ("password" . "XP"))
              (("machine" . "YM")) (("login" . "YL")) (("password" . "YP")))))))

(provide 'netrc-tests)

;;; netrc-tests.el ends here
