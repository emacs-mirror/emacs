;;; hmac-md5.el --- Compute HMAC-MD5.  -*- lexical-binding:t -*-

;; Copyright (C) 1999, 2001, 2007-2022 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC2104, HMAC-MD5, MD5, KEYED-MD5, CRAM-MD5

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

;; Test cases from RFC 2202, "Test Cases for HMAC-MD5 and HMAC-SHA-1",
;; moved to lisp/test/net/hmac-md5-tests.el

;;; Code:

(eval-when-compile (require 'hmac-def))
(require 'hex-util)			; (decode-hex-string STRING)
(require 'md5)				; expects (md5 STRING)

(defun md5-binary (string)
  "Return the MD5 of STRING in binary form."
  (if (condition-case nil
	  ;; `md5' of v21 takes 4th arg CODING (and 5th arg NOERROR).
	  (md5 "" nil nil 'binary)	; => "d41d8cd98f00b204e9800998ecf8427e"
	(wrong-number-of-arguments nil))
      (decode-hex-string (md5 string nil nil 'binary))
    (decode-hex-string (md5 string))))

(define-hmac-function hmac-md5 md5-binary 64 16) ; => (hmac-md5 TEXT KEY)
(define-hmac-function hmac-md5-96 md5-binary 64 16 96)

(provide 'hmac-md5)

;;; hmac-md5.el ends here
