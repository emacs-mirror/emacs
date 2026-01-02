;;; sasl-cram-tests.el --- tests for sasl-cram.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

;; Test case from RFC 2195.

;;; Code:

(require 'ert)
(require 'sasl)
(require 'sasl-cram)

(ert-deftest sasl-cram-md5-response-test ()
  ;; The following strings are taken from section 2 of RFC 2195.
  (let ((client
         (sasl-make-client (sasl-find-mechanism '("CRAM-MD5"))
			   "user"
			   "imap"
                           "localhost"))
        (data (base64-decode-string "PDE4OTYuNjk3MTcwOTUyQHBvc3RvZmZpY2UucmVzdG9uLm1jaS5uZXQ+"))
        (sasl-read-passphrase
         (lambda (_prompt) (copy-sequence "tanstaaftanstaaf"))))
    (should (equal (sasl-cram-md5-response client (vector nil data))
                   "user b913a602c7eda7a495b4e6e7334d3890"))))

(provide 'sasl-cram-tests)
;;; sasl-cram-tests.el ends here
