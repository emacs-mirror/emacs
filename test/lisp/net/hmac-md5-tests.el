;;; hmac-md5-tests.el --- Tests for hmac-md5.el  -*- lexical-binding:t -*-

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
(require 'hmac-md5)

;; Test cases from RFC 2202, "Test Cases for HMAC-MD5 and HMAC-SHA-1",
;; moved here from hmac-md5.el

(ert-deftest hmac-md5-test-encode-string ()
  ;; RFC 2202 -- test_case 1
  (should (equal (encode-hex-string
                  (hmac-md5 "Hi There" (make-string 16 ?\x0b)))
                 "9294727a3638bb1c13f48ef8158bfc9d"))

  ;; RFC 2202 -- test_case 2
  (should (equal (encode-hex-string
                  (hmac-md5 "what do ya want for nothing?" "Jefe"))
                 "750c783e6ab0b503eaa86e310a5db738"))

  ;; RFC 2202 -- test_case 3
  (should (equal (encode-hex-string
                  (hmac-md5 (decode-hex-string (make-string 100 ?d))
                            (decode-hex-string (make-string 32 ?a))))
                 "56be34521d144c88dbb8c733f0e8b3f6"))

  ;; RFC 2202 -- test_case 4
  (should (equal (encode-hex-string
                  (hmac-md5 (decode-hex-string
                             (mapconcat (lambda (c) (concat (list c) "d"))
                                        (make-string 50 ?c)))
                            (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
                 "697eaf0aca3a3aea3a75164746ffaa79"))

  ;; RFC 2202 -- test_case 5 (a)
  (should (equal (encode-hex-string
                  (hmac-md5 "Test With Truncation" (make-string 16 ?\x0c)))
                 "56461ef2342edc00f9bab995690efd4c"))

  ;; RFC 2202 -- test_case 5 (b)
  (should (equal (encode-hex-string
                  (hmac-md5-96 "Test With Truncation" (make-string 16 ?\x0c)))
                 "56461ef2342edc00f9bab995"))

  ;; RFC 2202 -- test_case 6
  (should (equal (encode-hex-string
                  (hmac-md5
                   "Test Using Larger Than Block-Size Key - Hash Key First"
                   (decode-hex-string (make-string 160 ?a))))
                 "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"))

  ;; RFC 2202 -- test_case 7
  (should (equal (encode-hex-string
                  (hmac-md5
                   "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
                   (decode-hex-string (make-string 160 ?a))))
                 "6f630fad67cda0ee1fb1f562db3aa53e")))

(provide 'hmac-md5-tests)
;;; hmac-md5-tests.el ends here
