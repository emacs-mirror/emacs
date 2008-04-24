;;; sasl-ntlm.el --- NTLM (NT Lan Manager) module for the SASL client framework

;; Copyright (C) 2000, 2007, 2008  Free Software Foundation, Inc.

;; Author: Taro Kawagishi <tarok@transpulse.org>
;; Keywords: SASL, NTLM
;; Version: 1.00
;; Created: February 2001

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a SASL interface layer for NTLM authentication message
;; generation by ntlm.el

;;; Code:

(require 'sasl)
(require 'ntlm)

(defconst sasl-ntlm-steps
  '(ignore				;nothing to do before making
    sasl-ntlm-request			;authentication request
    sasl-ntlm-response)			;response to challenge
  "A list of functions to be called in sequnece for the NTLM
authentication steps.  Ther are called by 'sasl-next-step.")

(defun sasl-ntlm-request (client step)
  "SASL step function to generate a NTLM authentication request to the server.
Called from 'sasl-next-step.
CLIENT is a vector [mechanism user service server sasl-client-properties]
STEP is a vector [<previous step function> <result of previous step function>]"
  (let ((user (sasl-client-name client)))
    (ntlm-build-auth-request user)))

(defun sasl-ntlm-response (client step)
  "SASL step function to generate a NTLM response against the server
challenge stored in the 2nd element of STEP.  Called from 'sasl-next-step."
  (let* ((user (sasl-client-name client))
	 (passphrase
	  (sasl-read-passphrase (format "NTLM passphrase for %s: " user)))
	 (challenge (sasl-step-data step)))
    (ntlm-build-auth-response challenge user
			      (ntlm-get-password-hashes passphrase))))

(put 'sasl-ntlm 'sasl-mechanism
     (sasl-make-mechanism "NTLM" sasl-ntlm-steps))

(provide 'sasl-ntlm)

;; arch-tag: 1d9164c1-1df0-418f-b7ab-360157fd05dc
;;; sasl-ntlm.el ends here
