;;; rfc2104-tests.el --- Tests of RFC2104 hashes

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'rfc2104)
(require 'sha1)
(require 'md5)

(ert-deftest dbus-test-sha1 ()
  (should
   (equal (rfc2104-hash 'sha1 64 20 "Jefe" "what do ya want for nothing?")
          "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")))

(ert-deftest dbus-test-md5 ()
  (should
   (equal (rfc2104-hash 'md5 64 16 "Jefe" "what do ya want for nothing?")
          "750c783e6ab0b503eaa86e310a5db738")))

(provide 'rfc2104-tests)

;;; rfc2104-tests.el ends here
