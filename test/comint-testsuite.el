;;; bytecomp-testsuite.el

;; Copyright (C) 2010  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for comint and related modes.

;;; Code:

(require 'comint)

(defun comint-testsuite-run ()
  (interactive)
  (with-output-to-temp-buffer "*comint test*"
    (comint-testsuite--test-comint-password-prompt-regexp)))

(defun comint-testsuite--test-comint-password-prompt-regexp ()
  (interactive)
  (let ((password-strings
	 '("foo@example.net's password: " ;ssh
	   "Password for foo@example.org: " ; kinit
	   "Please enter the password for foo@example.org: "   ; kinit
	   "Kerberos password for devnull/root <at> GNU.ORG: " ; ksu
	   "Enter passphrase: " ; ssh-add
	   "Enter passphrase (empty for no passphrase): " ; ssh-keygen
	   "Enter same passphrase again: "     ; ssh-keygen
	   "Passphrase for key root@GNU.ORG: " ; plink
	   "[sudo] password for user:" ; Ubuntu sudo
	   "Password (again):"
	   "Enter password:"))
	fail)
    (dolist (str password-strings)
      (unless (string-match comint-password-prompt-regexp str)
	(setq fail t)
	(princ (format " ERROR: comint-password-prompt-regexp did not match %s\n"
		       str))))
    (if fail
	(princ "FAILED: comint-password-prompt-regexp test\n")
      (princ "PASSED: comint-password-prompt-regexp test\n"))))

(provide 'comint-testsuite)

;;; comint-testsuite.el ends here

