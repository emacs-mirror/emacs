;;; url-tramp-tests.el --- Test suite for Tramp / URL conversion.

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

(require 'url-tramp)
(require 'ert)

(ert-deftest url-tramp-test-convert-url-to-tramp ()
  "Test that URLs are converted into proper Tramp file names."
  (should
   (string-equal
    (url-tramp-convert-url-to-tramp "ftp://ftp.is.co.za/rfc/rfc1808.txt")
    "/ftp:ftp.is.co.za:/rfc/rfc1808.txt"))

  (should
   (string-equal
    (url-tramp-convert-url-to-tramp "ssh://user@localhost")
    "/ssh:user@localhost:"))

  (should
   (string-equal
    (url-tramp-convert-url-to-tramp "telnet://remotehost:42")
    "/telnet:remotehost#42:"))

  ;; The password will be added to the cache.  The password cache key
  ;; is the remote file name identification of the Tramp file.
  (should
   (string-equal
    (url-tramp-convert-url-to-tramp "scp://user:geheim@somewhere/localfile")
    "/scp:user@somewhere:/localfile"))
  (let ((key
         (file-remote-p
          (url-tramp-convert-url-to-tramp "scp://user@somewhere/localfile"))))
    (should (password-in-cache-p key))
    (should (string-equal (password-read-from-cache key) "geheim"))
    (password-cache-remove key)
    (should-not (password-in-cache-p key)))

  ;; "http" does not belong to `url-tramp-protocols'.
  (should-not (url-tramp-convert-url-to-tramp "http://www.gnu.org")))

(ert-deftest url-tramp-test-convert-tramp-to-url ()
  "Test that Tramp file names are converted into proper URLs."
  (should
   (string-equal
    (url-tramp-convert-tramp-to-url "/ftp:ftp.is.co.za:/rfc/rfc1808.txt")
    "ftp://ftp.is.co.za/rfc/rfc1808.txt"))

  (should
   (string-equal
    (url-tramp-convert-tramp-to-url "/ssh:user@localhost:")
    "ssh://user@localhost"))

  (should
   (string-equal
    (url-tramp-convert-tramp-to-url "/telnet:user@remotehost#42:")
    "telnet://user@remotehost:42"))

  ;; "sftp" does not belong to `url-tramp-protocols'.
  (should-not (url-tramp-convert-tramp-to-url "/sftp:user@localhost:")))

(provide 'url-tramp-tests)

;;; url-tramp-tests.el ends here
