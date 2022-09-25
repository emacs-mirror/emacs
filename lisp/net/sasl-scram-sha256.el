;;; sasl-scram-sha256.el --- SCRAM-SHA-256 module for the SASL client framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Package: sasl

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

;; Implement the SCRAM-SHA-256 mechanism from RFC 7677.

;;; Code:

(require 'cl-lib)
(require 'sasl)
(require 'hex-util)
(require 'rfc2104)
(require 'sasl-scram-rfc)

;;; SCRAM-SHA-256

(defconst sasl-scram-sha-256-steps
  '(sasl-scram-client-first-message
    sasl-scram-sha-256-client-final-message
    sasl-scram-sha-256-authenticate-server))

(defun sasl-scram-sha256 (object &optional start end binary)
  (secure-hash 'sha256 object start end binary))

(defun sasl-scram-sha-256-client-final-message (client step)
  (sasl-scram--client-final-message
   ;; HMAC-SHA256 uses block length 64 and hash length 32; see RFC 4634.
   'sasl-scram-sha256 64 32 client step))

(defun sasl-scram-sha-256-authenticate-server (client step)
  (sasl-scram--authenticate-server
   'sasl-scram-sha256 64 32 client step))

(put 'sasl-scram-sha256 'sasl-mechanism
     (sasl-make-mechanism "SCRAM-SHA-256" sasl-scram-sha-256-steps))

(provide 'sasl-scram-sha256)

;;; sasl-scram-sha256.el ends here
